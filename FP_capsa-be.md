# Analisis Konsep Functional Programming pada Proyek capsa-be

Berikut adalah analisis penerapan konsep-konsep Functional Programming pada codebase `capsa-be`.

## 1. Algebraic Data Type (ADT)

### Contoh 1: `data Card`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 26-27)

```haskell
data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq, Show, Generic)
```

**Penjelasan Singkat:**
Tipe data `Card` merepresentasikan sebuah kartu dalam permainan Capsa.

**Mengapa ini disebut Algebraic Data Type?**
Ini adalah bentuk **Product Type**. Sebuah nilai `Card` dibentuk dari perkalian (gabungan) antara tipe `Rank` dan `Suit`. Untuk membuat sebuah kartu, kita harus menyediakan **kedua** nilai tersebut. Tidak bisa hanya `Rank` saja atau `Suit` saja. Ini mirip dengan tuple `(Rank, Suit)` tetapi dengan nama konstruktor yang eksplisit.

### Contoh 2: `data PlayType`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 158-163)

```haskell
data PlayType 
  = Single 
  | Pair 
  | Triple 
  | Combination CombinationType
  deriving (Eq, Show, Generic)
```

**Penjelasan Singkat:**
Tipe data `PlayType` mendefinisikan jenis-jenis kombinasi kartu yang valid untuk dimainkan.

**Mengapa ini disebut Algebraic Data Type?**
Ini adalah bentuk **Sum Type**. Sebuah nilai `PlayType` hanya bisa berupa `Single` **ATAU** `Pair` **ATAU** `Triple` **ATAU** `Combination`. 
Konstruktor `Combination` sendiri membawa *payload* berupa tipe lain (`CombinationType`), yang menunjukkan sifat rekursif atau komposit dari ADT. Ini memungkinkan kita memodelkan aturan permainan yang kompleks secara type-safe (misalnya, `Combination` harus menyertakan jenis kombinasinya).

## 2. Pattern Matching

### Contoh 1: `classifyPlay`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 177-186)

```haskell
classifyPlay :: [Card] -> Maybe PlayType
classifyPlay [] = Nothing
classifyPlay [_] = Just Single
classifyPlay cards@[c1, c2]
  | rank c1 == rank c2 = Just Pair
  | otherwise = Nothing
classifyPlay cards@[c1, c2, c3]
  | rank c1 == rank c2 && rank c2 == rank c3 = Just Triple
  | otherwise = Nothing
-- ...
```

**Penjelasan Singkat:**
Fungsi ini mengklasifikasikan sekumpulan kartu menjadi tipe permainan yang sesuai (`Single`, `Pair`, dst.) atau `Nothing` jika tidak valid.

**Mengapa ini disebut Pattern Matching?**
1.  **De-strukturisasi List:** Pola `[]` mencocokkan list kosong. Pola `[_]` mencocokkan list dengan tepat satu elemen. Pola `cards@[c1, c2]` mencocokkan list dengan tepat dua elemen, sekaligus mengikat variabel `c1` dan `c2` ke elemen-elemen tersebut dan `cards` ke list utuh.
2.  **Dispatching Logika:** Alur eksekusi ditentukan oleh bentuk data input. Jika input cocok dengan pola list 3 elemen, blok logika untuk `Triple` yang dijalankan.
3.  **Guards:** Penggunaan `| rank c1 == rank c2` (guards) memperkuat pattern matching dengan kondisi boolean tambahan.

### Contoh 2: `comparePlays`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 307-330)

```haskell
comparePlays Single [c1] [c2] =
  Just (compare c1 c2)

comparePlays Pair cards1 cards2 = do
  -- ...

comparePlays (Combination _) cards1 cards2
  | length cards1 /= 5 || length cards2 /= 5 = Nothing
  | otherwise = compareFiveCardHands cards1 cards2
```

**Penjelasan Singkat:**
Fungsi ini membandingkan dua permainan kartu untuk menentukan mana yang lebih tinggi.

**Mengapa ini disebut Pattern Matching?**
Fungsi ini melakukan pencocokan pola pada *tiga* argumen sekaligus:
1.  Argumen pertama (`PlayType`): `Single`, `Pair`, `Triple`, atau `Combination _`.
2.  Argumen kedua dan ketiga (List of Cards). 
Pada kasus `Single`, pola `[c1]` dan `[c2]` langsung mengekstrak kartu tunggal dari list input, memungkinkan perbandingan langsung `compare c1 c2` tanpa perlu fungsi akses list seperti `head`.

## 3. Function Composition

### Contoh 1: `processCards`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 143-144)

```haskell
processCards :: (Card -> Bool) -> ([Card] -> [Card]) -> [Card] -> [Card]
processCards predicate transformer = transformer . filter predicate
```

**Penjelasan Singkat:**
Fungsi helper untuk memfilter kartu lalu mentransformasikannya.

**Mengapa ini disebut Function Composition?**
Operator `.` (titik) adalah operator komposisi fungsi matematis $(f 	ext{ o } g)(x) = f(g(x))$.
Di sini, `transformer . filter predicate` menggabungkan dua fungsi:
1.  `filter predicate`: Fungsi yang mengambil list kartu dan mengembalikan list kartu yang lolos filter.
2.  `transformer`: Fungsi yang mengambil list kartu dan mengembalikan list kartu hasil transformasi.
Hasil komposisinya adalah fungsi baru yang menerima list kartu, memfilternya, lalu mentransformasi hasilnya, dalam satu pipeline yang bersih.

### Contoh 2: `hasAtLeast`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 132)

```haskell
hasAtLeast :: Int -> [Card] -> Bool
hasAtLeast n = (>= n) . length
```

**Penjelasan Singkat:**
Mengecek apakah jumlah kartu minimal `n`.

**Mengapa ini disebut Function Composition?**
Ekspresi `(>= n) . length` mengkomposisikan fungsi `length` (menghitung panjang list) dengan fungsi `(>= n)` (mengecek apakah nilai >= n). Data mengalir dari kanan ke kiri: List -> `length` -> Int -> `(>= n)` -> Bool.

## 4. High Order Function

### Contoh 1: `validateWith`
**File:** `capsa-be/src/GameLogic.hs` (Line 148-151)

```haskell
validateWith :: (a -> Bool) -> String -> a -> ValidationResult a
validateWith predicate errorMsg value
  | predicate value = Valid value
  | otherwise = Invalid errorMsg
```

**Penjelasan Singkat:**
Fungsi validasi generik yang menerima logika validasi sebagai argumen.

**Mengapa ini disebut High Order Function (HOF)?**
`validateWith` menerima fungsi `predicate` (bertipe `a -> Bool`) sebagai argumen pertamanya. Ia tidak tahu *apa* yang divalidasi (apakah list kosong? angka negatif?); ia hanya menjalankan logika yang diberikan oleh pemanggil. Ini adalah abstraksi perilaku (behavior abstraction).

### Contoh 2: `filterBySuit`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 60-61)

```haskell
filterBySuit :: Suit -> [Card] -> [Card]
filterBySuit targetSuit = filter (
card -> suit card == targetSuit)
```

**Penjelasan Singkat:**
Memfilter kartu berdasarkan suit tertentu.

**Mengapa ini disebut High Order Function (HOF)?**
Fungsi ini menggunakan `filter`, yang merupakan HOF standar Haskell. `filter` menerima sebuah fungsi predikat (di sini berupa lambda `\card -> ...`) dan sebuah list. `filterBySuit` sendiri juga bisa dianggap HOF jika kita melihatnya sebagai pembuat fungsi filter yang lebih spesifik.

## 5. Lambda Function

### Contoh 1: Lambda pada `groupByRank`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 149)

```haskell
   any (\c -> rank c == r) cards]
```

**Penjelasan Singkat:**
Bagian dari list comprehension untuk mengelompokkan kartu.

**Mengapa ini disebut Lambda Function?**
Ekspresi `\c -> rank c == r` adalah fungsi anonim (tanpa nama). Simbol `\` (backslash) mirip dengan $\lambda$ dalam kalkulus lambda. Fungsi ini dibuat *ad-hoc* untuk digunakan oleh fungsi `any` guna mengecek apakah ada kartu `c` yang memiliki rank `r`.

### Contoh 2: Lambda pada `filterBySuit`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 61)

```haskell
filterBySuit targetSuit = filter (\card -> suit card == targetSuit)
```

**Penjelasan Singkat:**
Predikat untuk `filter`.

**Mengapa ini disebut Lambda Function?**
`\card -> suit card == targetSuit` adalah fungsi anonim yang menerima `card` dan mengembalikan `Bool`. Lambda ini "menangkap" (closure) variabel `targetSuit` dari lingkup luarnya, memungkinkan pembuatan predikat dinamis berdasarkan argumen fungsi induk.

## 6. List Comprehension

### Contoh 1: `fullDeck`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 56-57)

```haskell
fullDeck :: [Card]
fullDeck = [Card r s | r <- [minBound..maxBound], s <- [minBound..maxBound]]
```

**Penjelasan Singkat:**
Membangkitkan dek kartu lengkap (52 kartu).

**Mengapa ini disebut List Comprehension?**
Sintaks `[ ... | ... ]` adalah notasi deklaratif untuk membangun list (himpunan).
*   **Generator:** `r <- ...` dan `s <- ...` membangkitkan kombinasi Cartesian dari semua Rank dan Suit.
*   **Konstruksi:** `Card r s` menciptakan elemen untuk setiap pasangan.
*   Ini setara dengan notasi himpunan matematika: { Card(r, s) | r \in Ranks, s \in Suits }.

### Contoh 2: `findPairs`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 153-156)

```haskell
findPairs :: [Card] -> [[Card]]
findPairs cards = 
  [pair | r <- [minBound..maxBound],
   let pair = [card | card <- cards, rank card == r],
   length pair >= 2]
```

**Penjelasan Singkat:**
Mencari semua pasangan (pair) dalam tangan.

**Mengapa ini disebut List Comprehension?**
Ini adalah contoh *nested list comprehension* dengan filtering.
1.  Generator luar mengiterasi setiap Rank `r`.
2.  `let pair = ...`: Mendefinisikan variabel lokal `pair` menggunakan list comprehension dalam (mengambil kartu dengan rank `r`).
3.  Filter `length pair >= 2`: Hanya menyertakan hasil jika jumlah kartu minimal 2.
Hasilnya adalah daftar semua pasangan kartu yang mungkin.

## 7. Lazy Evaluation

### Contoh 1: `infiniteDecks`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 123-124)

```haskell
infiniteDecks :: [Card]
infiniteDecks = cycle fullDeck
```

**Penjelasan Singkat:**
Mendefinisikan list kartu tak terbatas yang mengulang-ulang dek standar.

**Mengapa ini disebut Lazy Evaluation?**
Di bahasa *strict*, fungsi ini akan hang atau crash karena mencoba mengalokasikan memori tak terbatas. Di Haskell, `cycle` hanya membuat "resep" untuk list tak hingga. Elemen list baru dibuat (dievaluasi) hanya saat diminta oleh konsumen (misalnya oleh `take` di fungsi `takeCards`).

### Contoh 2: `playerTurnSequence`
**File:** `capsa-be/src/GameLogic.hs` (Line 245-246)

```haskell
playerTurnSequence :: [PlayerNumber]
playerTurnSequence = cycle [0, 3, 2, 1]
```

**Penjelasan Singkat:**
Urutan giliran pemain yang berputar terus-menerus.

**Mengapa ini disebut Lazy Evaluation?**
Sama seperti contoh sebelumnya, `cycle` menghasilkan *infinite stream*. Logika permainan dapat mengambil elemen dari list ini (`dropWhile`, `take`) tanpa khawatir tentang batas list atau indeks array, karena secara konseptual list ini tidak pernah berakhir.

## 8. Currying

### Contoh 1: `diamondCards`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 65-66)

```haskell
diamondCards :: [Card] -> [Card]
diamondCards = filterBySuit Diamond
```

**Penjelasan Singkat:**
Fungsi khusus untuk mengambil kartu Diamond.

**Mengapa ini disebut Currying (Partial Application)?**
*   `filterBySuit` bertipe `Suit -> [Card] -> [Card]`.
*   Secara teknis, tipe ini ekuivalen dengan `Suit -> ([Card] -> [Card])`.
*   Dengan memanggil `filterBySuit Diamond` tanpa memberikan argumen kedua (list kartu), kita menghasilkan sebuah fungsi baru yang "menunggu" list kartu tersebut. Ini adalah aplikasi parsial yang dimungkinkan oleh sifat fungsi Haskell yang ter-curry secara default.

### Contoh 2: `deal13Cards`
**File:** `capsa-be/src/GameLogic.hs` (Line 23-24)

```haskell
deal13Cards :: [Card] -> [Card]
deal13Cards = dealNCards 13
```

**Penjelasan Singkat:**
Fungsi untuk mengambil 13 kartu.

**Mengapa ini disebut Currying?**
`dealNCards` (alias `take`) menerima dua argumen: jumlah (`Int`) dan list (`[Card]`). `deal13Cards` didefinisikan dengan mengaplikasikan hanya argumen pertama (`13`) ke `dealNCards`. Hasilnya adalah fungsi yang menerima sisa argumennya (`[Card]`).

## 9. Functor

### Contoh 1: Instance Functor `ValidationResult`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 86-88)

```haskell
instance Functor ValidationResult where
  fmap f (Valid a) = Valid (f a)
  fmap _ (Invalid msg) = Invalid msg
```

**Penjelasan Singkat:**
Mendefinisikan bagaimana memetakan fungsi ke dalam konteks `ValidationResult`.

**Mengapa ini disebut Functor?**
Functor adalah struktur data yang bisa "dipetakan" (mappable).
*   `fmap` mengambil fungsi `f` dan nilai dalam konteks `ValidationResult`.
*   Jika konteksnya `Valid a`, fungsi `f` diterapkan ke `a` (`f a`), dan hasilnya dibungkus kembali menjadi `Valid`.
*   Jika konteksnya `Invalid`, error diteruskan tanpa perubahan.
*   Ini memungkinkan transformasi data sukses tanpa perlu `if-else` manual untuk cek error.

### Contoh 2: Penggunaan `fmap` (atau `<$>`) pada `findPlayerWith3Diamond`
**File:** `capsa-be/src/GameLogic.hs` (Line 35)

```haskell
findPlayerWith3Diamond hands = 
  fmap fst $ safeHead $ ...
```

**Penjelasan Singkat:**
Mengambil elemen pertama dari tuple hasil pencarian.

**Mengapa ini disebut Functor?**
`safeHead` mengembalikan `Maybe (PlayerNumber, [Card])`. `Maybe` adalah Functor.
`fmap fst` diterapkan pada nilai `Maybe` tersebut.
*   Jika `Just (player, cards)`, hasilnya `Just player`.
*   Jika `Nothing`, hasilnya tetap `Nothing`.
Kita memanipulasi isi `Maybe` tanpa membukanya (`unwrap`).

## 10. Applicative

### Contoh 1: Instance Applicative `ValidationResult`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 91-95)

```haskell
instance Applicative ValidationResult where
  pure = Valid
  Valid f <*> Valid x = Valid (f x)
  Invalid msg <*> _ = Invalid msg
  _ <*> Invalid msg = Invalid msg
```

**Penjelasan Singkat:**
Mendefinisikan kombinasi validasi independen.

**Mengapa ini disebut Applicative?**
*   `pure`: Memasukkan nilai biasa ke dalam konteks (`Valid`).
*   `<*>` (apply): Menerapkan fungsi yang ada di dalam konteks (`Valid f`) ke nilai yang ada di dalam konteks (`Valid x`).
*   Pola ini memungkinkan kita menjalankan beberapa validasi secara paralel/independen dan menggabungkan hasilnya jika semua sukses, atau mengambil error pertama jika ada yang gagal.

### Contoh 2: `validatePlay`
**File:** `capsa-be/src/GameLogic.hs` (Line 296-302)

```haskell
validatePlay hand cardsToPlay currentP playerP =
  validateNonEmpty cardsToPlay *>
  (if hasDuplicateCards ... ) *>
  (if playerP == currentP ... ) *>
  (if validateCards hand ... )
```

**Penjelasan Singkat:**
Memvalidasi langkah permainan dengan serangkaian cek.

**Mengapa ini disebut Applicative?**
Operator `*>` (sequence right) adalah operator Applicative.
`A *> B` berarti: "Jalankan efek A (validasi), buang hasilnya, lalu jalankan efek B, dan kembalikan hasil B".
Jika A gagal (`Invalid`), maka seluruh rangkaian gagal seketika dengan pesan error dari A. Jika A sukses (`Valid`), lanjut ke B. Ini adalah cara deklaratif untuk menyusun urutan validasi "wajib lulus semua".

## 11. Monad

### Contoh 1: Instance Monad `ValidationResult`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 98-100)

```haskell
instance Monad ValidationResult where
  Valid x >>= f = f x
  Invalid msg >>= _ = Invalid msg
```

**Penjelasan Singkat:**
Mendefinisikan *chaining* validasi yang bergantung satu sama lain.

**Mengapa ini disebut Monad?**
*   `>>=` (bind): Mengambil nilai dari konteks (`Valid x`), dan memberikannya ke fungsi `f` yang menghasilkan konteks baru.
*   Jika langkah pertama gagal (`Invalid`), fungsi `f` tidak pernah dijalankan, dan error dipropagasi.
*   Ini memungkinkan validasi sekuensial: Validasi B hanya masuk akal untuk dijalankan jika Validasi A sudah sukses (misal: cek `cards` tidak kosong dulu, baru cek isinya).

### Contoh 2: `do` notation pada `comparePlays` (Pair/Triple)
**File:** `capsa-be/src/CardRepresentation.hs` (Line 311-314)

```haskell
comparePlays Pair cards1 cards2 = do
  max1 <- getLargestCard cards1
  max2 <- getLargestCard cards2
  return (compare max1 max2)
```

**Penjelasan Singkat:**
Membandingkan dua pair.

**Mengapa ini disebut Monad?**
*   Fungsi ini berjalan dalam `Maybe` Monad.
*   `getLargestCard` mengembalikan `Maybe Card`.
*   Blok `do` menangani kemungkinan `Nothing` secara implisit.
    *   Jika `cards1` kosong, `getLargestCard` kembali `Nothing`.
    *   Seluruh blok `do` langsung berhenti dan mengembalikan `Nothing`.
    *   Kita tidak perlu `if/case` untuk mengecek apakah kartu ada atau tidak.

### Contoh 3: IO Monad pada `main`
**File:** `capsa-be/src/Main.hs` (Line 23-30)

```haskell
main :: IO ()
main = do
  putStrLn "Starting Capsa game server..."
  gameState <- createInitialState
  -- ...
```

**Penjelasan Singkat:**
Entry point aplikasi.

**Mengapa ini disebut Monad?**
`IO` adalah Monad yang menangani efek samping (side effects). `do` notation mengurutkan aksi-aksi: cetak string, buat state, jalankan server. Setiap baris bergantung pada penyelesaian baris sebelumnya secara sekuensial.

## 12. Monoid

### Contoh 1: `CardCount`
**File:** `capsa-be/src/CardRepresentation.hs` (Line 113-120)

```haskell
newtype CardCount = CardCount Int
-- ...
instance Monoid CardCount where
  mempty = CardCount 0
```

**Penjelasan Singkat:**
Wrapper untuk menghitung jumlah kartu.

**Mengapa ini disebut Monoid?**
*   **Asosiatif (`<>`):** Penjumlahan jumlah kartu bersifat asosiatif.
*   **Identitas (`mempty`):** Jumlah kartu 0 adalah elemen identitas.
*   Ini memungkinkan kita menggunakan fungsi generik seperti `foldMap` untuk menghitung total kartu dari struktur data yang kompleks.

### Contoh 2: `PlayerStats`
**File:** `capsa-be/src/GameTypes.hs` (Line 73-77)

```haskell
instance Monoid PlayerStats where
  mempty = PlayerStats 0 0 0
```

**Penjelasan Singkat:**
Statistik pemain (menang, main, total kartu).

**Mengapa ini disebut Monoid?**
Kita bisa menggabungkan dua statistik pemain (misal: statistik Sesi 1 + statistik Sesi 2) hanya dengan `stats1 <> stats2`. Monoid menjamin bahwa penggabungan ini konsisten dan `mempty` (statistik nol) tidak mengubah hasil. Ini sangat berguna untuk agregasi data.

## 13. QuickCheck

**Analisis:**
Tidak terdapat implementasi **QuickCheck** pada codebase ini. File `test/` tidak disertakan dalam daftar file yang dianalisis, dan file kode sumber utama tidak mengandung properti QuickCheck. Namun, mode `DeckDeterministic` (di `GameState.hs`) menunjukkan adanya upaya pengujian manual/deterministik, meskipun bukan *property-based testing* otomatis ala QuickCheck.

```
