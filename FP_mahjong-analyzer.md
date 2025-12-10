# Analisis Konsep Functional Programming pada Proyek mahjong-analyzer

Berikut adalah analisis penerapan konsep-konsep Functional Programming pada codebase `mahjong-analyzer`.

## 1. Algebraic Data Type (ADT)

### Contoh 1: `data AgariHand`
**File:** `mahjong-analyzer/src/Hand.hs` (Line 78-82)

```haskell
data AgariHand 
    = Standard [KMeld] Pair  -- 4 Meld + 1 Pair 
    | Chiitoi [Pair]        -- 7 Pair
    | Kokushi Hand          -- 13 + 1 Honor & Terminal
    deriving (Eq, Show)
```

**Penjelasan Kode:**
Tipe `AgariHand` mendefinisikan bentuk-bentuk tangan kemenangan dalam Mahjong. Ada 3 varian: standar (4 set + 1 pair), Chiitoi (7 pasang), dan Kokushi (13 anak yatim).

**Mengapa ini disebut Algebraic Data Type (ADT):**
*   **Sum Type (Alternatif):** Sebuah tangan kemenangan (`AgariHand`) pasti salah satu dari `Standard`, `Chiitoi`, **ATAU** `Kokushi`. Tidak bisa berupa campuran. Ini dimodelkan dengan sum type.
*   **Product Type (Komposit):** Konstruktor `Standard` memuat `[KMeld]` **DAN** `Pair`. Untuk membuat nilai `Standard`, kita butuh kedua data tersebut. Ini adalah product type.
* ADT ini memungkinkan representasi domain yang presisi. Kita tidak mungkin membuat "tangan Standard tapi isinya cuma pair" karena compiler akan menolak konstruksi data yang salah. Fungsi-fungsi selanjutnya (seperti scoring) cukup melakukan pattern matching pada ketiga varian ini.

### Contoh 2: `data Tile`
**File:** `mahjong-analyzer/src/Hand.hs` (Line 15-18)

```haskell
data Tile = Tile {
    suitTile :: Suit
    , numberTile :: Int
}
    deriving (Eq)
```

**Penjelasan Kode:**
Mendefinisikan kartu Mahjong yang terdiri dari *Suit* (jenis) dan nomor.

**Mengapa ini disebut Algebraic Data Type (ADT):**
Ini adalah *Record Syntax* untuk Product Type. Sebuah `Tile` adalah gabungan dari `Suit` *dan* `Int`. ADT ini membungkus dua tipe primitif menjadi satu entitas domain yang kohesif.

## 2. Pattern Matching

### Contoh 1: Fungsi `isSimpleTile`
**File:** `mahjong-analyzer/src/YakuCalculator.hs` (Line 23-26)

```haskell
isSimpleTile :: Tile -> Bool
isSimpleTile (Tile Honor _) = False
isSimpleTile (Tile _ 1)     = False
isSimpleTile (Tile _ 9)     = False
isSimpleTile _              = True
```

**Penjelasan Kode:**
Fungsi ini menentukan apakah sebuah tile tergolong "sederhana" (bukan Honor, bukan Terminal 1/9).

**Mengapa ini disebut Pattern Matching:**
*   **Pencocokan Spesifik:** Baris pertama `(Tile Honor _)` mencocokkan *setiap* tile yang suit-nya `Honor`, mengabaikan nomornya (menggunakan wildcard `_`).
*   **Pencocokan Nilai:** Baris kedua `(Tile _ 1)` mencocokkan tile *apa saja* (suit apa saja) asalkan nomornya 1.
*   **Catch-All:** Baris terakhir `_` menangkap semua kasus yang tidak cocok dengan pola di atas (yaitu tile angka 2-8 yang bukan Honor).
*   Tanpa pattern matching, kita harus menulis `if (suit == Honor) return False; else if (num == 1) return False...` yang jauh lebih verbose dan rawan error.

### Contoh 2: Fungsi `shantenSuitTileFirst`
**File:** `mahjong-analyzer/src/ShantenCalculate.hs` (Line 56-57)

```haskell
shantenSuitTileFirst hand result
  | Map.null hand = result
  | otherwise = ...
```

**Penjelasan Kode:**
Menggunakan *Guards* untuk mengecek kondisi map kosong.

**Mengapa ini disebut Pattern Matching:**
Meskipun menggunakan sintaks guard (`|`), secara konseptual ini adalah percabangan logika berbasis kondisi data input. Lebih jelas lagi di baris 70 (tidak dikutip penuh tapi ada di file) di mana terdapat pattern matching `(ok, rm, dl) <- cases` di dalam list comprehension, yang membongkar tuple 3-elemen menjadi tiga variabel terpisah.

## 3. Function Composition

### Contoh 1: `findPair`
**File:** `mahjong-analyzer/src/Hand.hs` (Line 106)

```haskell
findPair = map Pair . Map.keys . Map.filter (>= 2)
```

**Penjelasan Kode:**
Mencari pasangan tile dalam tangan.

**Mengapa ini disebut Function Composition:**
*   Operator `.` merangkai tiga fungsi menjadi satu pipeline:
    1.  `Map.filter (>= 2)`: Ambil elemen map yang jumlahnya minimal 2.
    2.  `Map.keys`: Ambil kuncinya saja (jenis tile-nya).
    3.  `map Pair`: Bungkus setiap tile menjadi tipe `Pair`.
*   Kita membaca kode ini dari kanan ke kiri sebagai aliran transformasi data. Input `HandCount` masuk ke kanan, output `[Pair]` keluar dari kiri. Tidak ada variabel perantara (seperti `filtered = ...`, `keys = ...`) yang didefinisikan, membuat kode sangat deklaratif.

### Contoh 2: `countDragonTriplets`
**File:** `mahjong-analyzer/src/YakuCalculator.hs` (Line 157-158)

```haskell
countDragonTriplets = 
    length . 
    filter ( ... )
```

**Penjelasan Kode:**
Menghitung jumlah triplet naga.

**Mengapa ini disebut Function Composition:**
Menggabungkan `length` dan `filter`. Hasil filter (list) langsung diumpankan ke `length`.

## 4. High Order Function

### Contoh 1: `memoTree`
**File:** `mahjong-analyzer/src/ShantenCalculate.hs` (Line 126)

```haskell
memoTree :: ((Int -> a) -> Int -> a) -> Int -> a
memoTree f = memoTree_f
```

**Penjelasan Kode:**
Fungsi `memoTree` adalah inti dari teknik optimasi *memoization* menggunakan struktur tree tak terbatas.

**Mengapa ini disebut High Order Function (HOF):**
*   **Argumen Fungsi:** `memoTree` menerima argumen `f`. Tipe `f` cukup kompleks: `((Int -> a) -> Int -> a)`. Ini adalah fungsi *open recursion*. `f` menerima sebuah fungsi (sebagai "cara memanggil diri sendiri secara rekursif") dan sebuah integer, lalu menghasilkan nilai `a`.
*   **Mengembalikan Fungsi:** `memoTree` mengembalikan fungsi bertipe `Int -> a`. Fungsi hasil kembalian ini adalah versi yang "sudah dimemoisasi" dari logika aslinya. Saat dipanggil, ia akan mencari nilai di struktur data tree alih-alih menghitung ulang.

### Contoh 2: `any` dan `all` pada `isKokushi`
**File:** `mahjong-analyzer/src/AgariCheck.hs` (Line 83-84)

```haskell
        hasAll = all (`elem` uniqueTiles) allOrphans
        hasPair = any (	 -> Map.findWithDefault 0 t hc >= 2) allOrphans
```

**Penjelasan Kode:**
Mengecek kondisi kemenangan Kokushi Musou.

**Mengapa ini disebut High Order Function (HOF):**
*   `all` menerima predikat `` (`elem` uniqueTiles) `` dan list `allOrphans`. Ia menerapkan predikat ke semua elemen list.
*   `any` menerima lambda predikat dan list.
*   HOF ini mengabstraksi pola iterasi "cek semua" dan "cek salah satu".

## 5. Lambda Function

### Contoh 1: `removeTile`
**File:** `mahjong-analyzer/src/AgariCheck.hs` (Line 15)

```haskell
removeTile handCount tile = Map.adjust (
c -> c - 1) tile handCount
```

**Penjelasan Kode:**
Mengurangi jumlah tile tertentu di tangan.

**Mengapa ini disebut Lambda Function:**
*   `\c -> c - 1` adalah fungsi anonim.
*   Fungsi ini diberikan sebagai argumen ke `Map.adjust`. `Map.adjust` membutuhkan "instruksi" tentang cara mengubah nilai yang ada di map. Lambda ini memberikan instruksi itu: "ambil nilai lama `c`, kurangi 1". Kita tidak perlu membuat fungsi bernama `decrement c = c - 1`.

### Contoh 2: Lambda pada Property Testing `prop_scorePermutationInvariant`
**File:** `mahjong-analyzer/test/Properties.hs` (Line 76)

```haskell
  in forAll (shuffle h) $ \h2 ->
       let hc2 = handToCount h2
       ...
```

**Penjelasan Kode:**
Mendefinisikan properti pengujian.

**Mengapa ini disebut Lambda Function:**
*   Lambda `\h2 -> ...` menangkap nilai `h2` yang dihasilkan oleh generator `shuffle h`.
*   Body dari lambda ini berisi logika pengujian yang kompleks (let binding, perhitungan skor). Seluruh blok logika ini diperlakukan sebagai satu unit fungsi yang dipassing ke fungsi `forAll` milik QuickCheck.

## 6. List Comprehension

### Contoh 1: `stringToTiles`
**File:** `mahjong-analyzer/src/Hand.hs` (Line 100)

```haskell
handToCount hand = Map.fromListWith (+) [ (tile, 1) | tile <- hand ]
```

**Penjelasan Kode:**
Mengubah list tile menjadi map frekuensi.

**Mengapa ini disebut List Comprehension:**
*   Sintaks `[ (tile, 1) | tile <- hand ]` membangun list baru.
*   Untuk setiap `tile` di dalam list `hand`, buat tuple `(tile, 1)`.
*   Ini adalah cara deklaratif untuk melakukan pemetaan (map) list. Hasilnya kemudian diolah oleh `Map.fromListWith` yang akan menjumlahkan angka `1` jika kuncinya sama, efektif menghitung frekuensi.

### Contoh 2: `tilesInMeld`
**File:** `mahjong-analyzer/src/FuCalculator.hs` (Line 23)

```haskell
tilesInMeld (Sequence tile) = 
    [ Tile s n | n <- [n1..(n1+2)]]
```

**Penjelasan Kode:**
Mengembalikan 3 tile yang membentuk sequence.

**Mengapa ini disebut List Comprehension:**
*   `[ Tile s n | n <- generator ]` adalah pola comprehension.
*   Generatornya adalah range `[n1 .. n1+2]`.
*   Untuk setiap `n` dalam range tersebut, konstruktor `Tile s n` dipanggil. Hasilnya adalah list 3 tile berurutan.

## 7. Lazy Evaluation

### Contoh 1: Infinite Tree `nats`
**File:** `mahjong-analyzer/src/ShantenCalculate.hs` (Line 115-121)

```haskell
nats :: Tree Int
nats = go 0 1
    where
        go !n !s = Tree (go l s') n (go r s')
```

**Penjelasan Kode:**
Mendefinisikan struktur data pohon biner tak terbatas yang berisi seluruh bilangan asli.

**Mengapa ini disebut Lazy Evaluation:**
*   `nats` adalah struktur data tak berhingga. Dalam bahasa strict, ini mustahil (stack overflow).
*   Di Haskell, node `Tree` hanya dibuat saat diakses. Node anak (`l` dan `r`) hanyalah *thunk* (janji komputasi) sampai seseorang mencoba melihat isinya.
*   Ini memungkinkan kita memiliki "peta memori konseptual" seluas bilangan integer, namun hanya menggunakan memori fisik untuk bagian yang benar-benar kita kunjungi saat perhitungan shanten.

### Contoh 2: Memoization `memoShantenSuit`
**File:** `mahjong-analyzer/src/ShantenCalculate.hs` (Line 129-133)

```haskell
memoShantenSuit :: Int -> Suit -> ResultSuit -> ResultSuit
memoShantenSuit = memoTree shantenInt
```

**Penjelasan Kode:**
Fungsi `memoShantenSuit` adalah versi memo dari perhitungan shanten.

**Mengapa ini disebut Lazy Evaluation:**
*   `memoTree` memetakan fungsi perhitungan ke atas pohon `nats` tak terbatas tadi.
*   Hasil perhitungan shanten untuk *setiap kemungkinan tangan* secara konseptual "sudah ada" di dalam pohon `memo`.
*   Namun, nilai sebenarnya baru dihitung (*lazy*) saat kita melakukan indexing ke posisi tertentu di pohon tersebut. Jika kita meminta `memoShantenSuit 12345 ...`, Haskell menelusuri pohon ke node 12345. Jika node itu belum dievaluasi, hitung sekarang dan simpan hasilnya di node itu. Jika sudah, kembalikan langsung. Ini adalah implementasi memoization yang murni fungsional dan efisien.

## 8. Currying

### Contoh 1: `removePair`
**File:** `mahjong-analyzer/src/AgariCheck.hs` (Line 25)

```haskell
        in foldl' (flip removeOneTile) hc tilesToRemove
```

**Penjelasan Kode:**
Menghapus sepasang tile dari tangan.

**Mengapa ini disebut Currying:**
*   `removeOneTile` bertipe `Tile -> HandCount -> HandCount`.
*   `flip removeOneTile` membalik urutan argumen menjadi `HandCount -> Tile -> HandCount`.
*   `foldl'` mengharapkan fungsi `HandCount -> Tile -> HandCount`.
*   Kita mem-passing `(flip removeOneTile)` sebagai nilai fungsi. Ini memanfaatkan fakta bahwa fungsi di Haskell ter-curry secara default (bisa dimanipulasi argumen per argumen).

### Contoh 2: `shantenInt`
**File:** `mahjong-analyzer/src/ShantenCalculate.hs` (Line 78-80)

```haskell
shantenInt
  :: (Int -> Suit -> ResultSuit -> ResultSuit)
  -> Int -> Suit -> ResultSuit -> ResultSuit
```

**Penjelasan Kode:**
Definisi tipe fungsi shanten yang digunakan dalam rekursi memoization.

**Mengapa ini disebut Currying:**
*   Fungsi ini menerima 4 argumen. Argumen pertama `(Int -> Suit ...)` adalah fungsi *callback* (rekursi diri sendiri).
*   Sistem memoization bekerja dengan menyuplai argumen pertama ini secara otomatis (teknik *fixed-point combinator*). Sisa 3 argumen lainnya (`Int`, `Suit`, `ResultSuit`) disuplai saat pemanggilan aktual. Kemampuan untuk menerapkan fungsi secara bertahap ini (Partial Application) adalah inti Currying.

## 9. Functor

### Contoh 1: Instance Functor untuk `Tree`
**File:** `mahjong-analyzer/src/ShantenCalculate.hs` (Line 107-109)

```haskell
instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)
```

**Penjelasan Kode:**
Mendefinisikan bagaimana fungsi `fmap` bekerja pada struktur data `Tree`.

**Mengapa ini disebut Functor:**
*   `Tree` adalah kontainer.
*   `fmap` menerapkan fungsi `f` ke nilai `m` di node saat ini, dan secara rekursif menerapkan `fmap` ke anak kiri (`l`) dan kanan (`r`).
*   Ini memungkinkan kita mengubah isi seluruh pohon (misal: mengubah pohon integer `nats` menjadi pohon hasil perhitungan shanten) tanpa mengubah bentuk struktur pohonnya.

### Contoh 2: Parsing (`<$>`) 
**File:** `mahjong-analyzer/test/Generators.hs` (Line 42)

```haskell
genTriplet = Triplet <$> genTile
```

**Penjelasan Kode:**
Membuat generator data `Meld` tipe Triplet.

**Mengapa ini disebut Functor:**
*   `genTile` adalah `Gen Tile` (Generator yang menghasilkan Tile).
*   `Triplet` adalah fungsi konstruktor `Tile -> Meld`.
*   `<$>` (fmap) mengangkat fungsi `Triplet` agar bekerja di dalam konteks `Gen`. Hasilnya adalah `Gen Meld`. Kita memetakan fungsi murni ke nilai probabilitas.

## 10. Applicative

### Contoh 1: Generator `KMeld`
**File:** `mahjong-analyzer/test/Generators.hs` (Line 48)

```haskell
  arbitrary = KMeld <$> arbitrary <*> arbitrary
```

**Penjelasan Kode:**
Membuat generator data acak untuk tipe `KMeld`.

**Mengapa ini disebut Applicative:**
*   `KMeld` adalah konstruktor yang butuh 2 argumen: `Meld` dan `Bool`.
*   `arbitrary` pertama menghasilkan `Gen Meld`.
*   `arbitrary` kedua menghasilkan `Gen Bool`.
*   Pola `Constructor <$> genA <*> genB` adalah idiom Applicative standar.
    1.  `KMeld <$> arbitrary` menghasilkan `Gen (Bool -> KMeld)` (Generator yang isinya fungsi yang menunggu Bool).
    2.  `<*> arbitrary` mengambil generator fungsi itu dan generator boolean, lalu menggabungkannya menjadi `Gen KMeld`.
*   Applicative memungkinkan kita menggabungkan dua konteks komputasi independen (dua generator acak) menjadi satu.

### Contoh 2: Kalkulator Wait Fu `isShanponWait`
**File:** `mahjong-analyzer/src/FuCalculator.hs` (Line 59)

```haskell
isShanponWait ctx hand = not . or $ uncurry <$> [isTankiWait, isRyanmenWait...] <*> [(ctx, hand)]
```

**Penjelasan Kode:**
Mengecek apakah tangan menunggu jenis wait tertentu.

**Mengapa ini disebut Applicative:**
*   Di sini List `[]` bertindak sebagai Applicative.
*   `[f1, f2, f3] <*> [x]` akan menghasilkan `[f1 x, f2 x, f3 x]`.
*   Kode ini menerapkan daftar fungsi predikat (`isTankiWait`, dll) ke satu input (`ctx, hand`). Hasilnya adalah daftar boolean `[Bool]`. Ini cara sangat ringkas untuk melakukan "batch processing" fungsi.

## 11. Monad

### Contoh 1: Parsing `parseHand` (`Either` Monad)
**File:** `mahjong-analyzer/src/Hand.hs` (Line 191-197)

```haskell
                case charToSuit suitChar of 
                    Left err   -> Left err
                    Right suit -> 
                        -- ...
                        in (currentTiles ++) <$> parseHand remainingString
```

**Penjelasan Kode:**
Fungsi parsing string rekursif.

**Mengapa ini disebut Monad:**
*   Kode ini menangani kemungkinan kegagalan (`Left err`) secara eksplisit.
*   Struktur `case ... of Left -> Left; Right -> ...` adalah implementasi manual dari operator bind (`>>=`) pada `Either` monad.
*   Jika `charToSuit` gagal, seluruh proses parsing berhenti dan error dikembalikan. Jika sukses, nilai `suit` dibuka dan digunakan untuk langkah selanjutnya. Ini adalah esensi *monadic error handling*.

### Contoh 2: Generator `HandContext` (`Gen` Monad)
**File:** `mahjong-analyzer/test/Generators.hs` (Line 95-103)

```haskell
  arbitrary = do
    wt <- arbitrary
    wm <- arbitrary
    -- ...
    return HandContext { ... }
```

**Penjelasan Kode:**
Membangkitkan data `HandContext` acak yang kompleks.

**Mengapa ini disebut Monad:**
*   `do` notation menunjukkan penggunaan Monad.
*   Setiap baris `x <- arbitrary` melakukan efek samping (pseudo-randomness) untuk mendapatkan nilai `x`.
*   Nilai `x` dari baris sebelumnya bisa digunakan untuk mempengaruhi baris selanjutnya (walaupun di sini independen).
*   Monad `Gen` mengelola state *seed* random number generator di belakang layar, sehingga kita bisa menulis kode pembangkitan data seolah-olah imperatif dan berurutan.

## 12. Monoid

**Analisis:**
Tidak ditemukan implementasi typeclass **Monoid** yang spesifik/kustom. Penggunaan operator `++` untuk menggabungkan list (yang merupakan Monoid) sangat umum, begitu pula penjumlahan skor (`Sum` Monoid), namun ini adalah penggunaan struktur data standar, bukan penerapan pola desain Monoid abstrak pada tipe data domain.

## 13. QuickCheck

### Contoh 1: Properti `prop_roundTrip`
**File:** `mahjong-analyzer/test/Properties.hs` (Line 17-21)

```haskell
prop_roundTrip :: AgariHand -> Bool
prop_roundTrip ag = 
  let h  = agariToHand ag
      hc = handToCount h
  in sort (countToHand hc) == sort h
```

**Penjelasan Kode:**
Properti ini menguji integritas konversi data.

**Mengapa ini disebut QuickCheck (Property-Based Testing):**
*   **Invariansi:** Properti ini menyatakan bahwa jika kita mengubah representasi tangan (`AgariHand` -> `Hand` -> `HandCount` -> `Hand`), datanya tidak boleh berubah (invarian).
*   QuickCheck akan membangkitkan ribuan variasi `AgariHand` acak menggunakan generator yang telah kita definisikan (`Arbitrary AgariHand`), lalu memverifikasi apakah persamaan ini selalu `True`.

### Contoh 2: `genStandard` (Generator Data)
**File:** `mahjong-analyzer/test/Generators.hs` (Line 62)

```haskell
genStandard = Standard <$> vectorOf 4 arbitrary <*> arbitrary
```

**Penjelasan Kode:**
Generator khusus untuk tangan standar.

**Mengapa ini disebut QuickCheck:**
*   Ini adalah bagian dari definisi *bagaimana* data uji dibuat.
*   `vectorOf 4 arbitrary` menjamin bahwa QuickCheck selalu membuat list yang berisi tepat 4 elemen Meld. Ini memastikan data uji valid secara struktural (sesuai aturan Mahjong), sehingga pengujian properti tidak gagal karena input sampah (false positive).