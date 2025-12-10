# Analisis Konsep Functional Programming pada Proyek nano-chesskel

Berikut adalah analisis penerapan konsep-konsep Functional Programming pada codebase `nano-chesskel`.

## 1. Algebraic Data Type (ADT)

### Contoh 1: `data MoveRule`
**File:** `nano-chesskel/src/Types.hs` (Line 76-80)

```haskell
data MoveRule
  = Step Position  -- A single step to an adjacent square
  | Jump Position  -- A single leap to any square
  | Slide Position -- A repeated slide in one direction (e.g., [1,0] or [1,1])
  deriving (Show, Generic)
```

**Penjelasan Kode:**
Tipe data `MoveRule` mendefinisikan "bahasa" pergerakan bidak catur. Ia memiliki tiga varian: `Step` (langkah tunggal), `Jump` (lompatan kuda), dan `Slide` (luncuran benteng/menteri). Setiap varian membawa data `Position` yang merepresentasikan vektor arah atau offset.

**Mengapa ini disebut Algebraic Data Type (ADT):**
*   **Sum Type:** `MoveRule` adalah *sum type* karena sebuah aturan gerak hanya bisa berupa salah satu dari ketiga jenis tersebut. Kita menggunakan operator `|` untuk menggabungkan kemungkinan-kemungkinan ini. Ini memungkinkan kita memodelkan logika domain yang eksklusif satu sama lain.
*   **Product Type:** Konstruktornya (misal `Step Position`) adalah *product type* (sebenarnya wrapper tunggal di sini) yang membungkus tipe lain (`Position`).
*   Dengan ADT ini, interpreter aturan (`RuleEngine.hs`) dapat ditulis secara total dan aman: ia cukup menangani ketiga kasus ini dan dijamin tidak akan ada "jenis gerak tak dikenal" lainnya saat runtime (kecuali kode diubah).

### Contoh 2: `data Color`
**File:** `nano-chesskel/src/Types.hs` (Line 62-63)

```haskell
data Color = White | Black
  deriving (Show, Eq, Ord, Generic, FromJSON)
```

**Penjelasan Kode:**
Tipe data enumerasi sederhana untuk merepresentasikan warna pemain.

**Mengapa ini disebut Algebraic Data Type (ADT):**
Ini adalah bentuk paling sederhana dari Sum Type, sering disebut *Enumerated Type*. Tipe `Color` memiliki kardinalitas 2 (hanya ada 2 nilai yang mungkin: `White` dan `Black`). Ini jauh lebih aman dan ekspresif daripada menggunakan `Boolean` (`True`/`False`) atau `Integer` (0/1), karena maknanya eksplisit dan tipe sistem mencegah kita mencampurnya dengan nilai lain.

## 2. Pattern Matching

### Contoh 1: Interpreter Pergerakan (`evalMove`)
**File:** `nano-chesskel/src/RuleEngine.hs` (Line 95-98)

```haskell
          evalMove rule = case rule of
            Step offset  -> evalStep (applySign offset)
            Jump offset  -> evalJump (applySign offset)
            Slide direction -> evalSlide (applySign direction)
```

**Penjelasan Kode:**
Fungsi `evalMove` bertugas menerjemahkan satu aturan gerak menjadi daftar posisi legal. Ia menggunakan `case ... of` untuk memeriksa varian `MoveRule` apa yang sedang diproses.

**Mengapa ini disebut Pattern Matching:**
*   **De-strukturisasi Konstruktor:** Pola `Step offset` tidak hanya mencocokkan jika `rule` adalah konstruktor `Step`, tetapi juga sekaligus mengekstrak isi `Position`-nya ke dalam variabel `offset`. Kita tidak perlu getter method `getOffset()`.
*   **Branching Berbasis Bentuk Data:** Alur program dicabangkan berdasarkan bentuk data. Jika data berbentuk `Slide`, maka logika `evalSlide` yang dijalankan. Ini adalah inti dari paradigma *data-driven programming* di FP.

### Contoh 2: Parsing Koordinat Catur
**File:** `nano-chesskel/app/Main.hs` (Line 132-136)

```haskell
parseMove input =
  case words input of
    [fromStr, toStr] ->
      case (parseChessPos fromStr, parseChessPos toStr) of
        (Right f, Right t) -> Right (f, t)
        (Left err, _) -> Left $ "Invalid 'from' square: " ++ err
        -- ...
    _ -> Left "Invalid format..."
```

**Penjelasan Kode:**
Fungsi ini mem-parse input string seperti "a2 a3".

**Mengapa ini disebut Pattern Matching:**
*   **Pencocokan List:** Pola `[fromStr, toStr]` mencocokkan list yang tepat memiliki dua elemen. Jika input user menghasilkan 1 kata atau 3 kata, pola ini gagal dan jatuh ke *wildcard* `_` (error handling).
*   **Pencocokan Tuple & Nested Constructor:** Pola `(Right f, Right t)` adalah *nested pattern*. Ia mencocokkan sebuah tuple di mana elemen pertama **harus** konstruktor `Right` (sukses) dan elemen kedua **juga harus** `Right`. Variabel `f` dan `t` langsung terikat ke nilai di dalam `Right`. Ini cara yang sangat ringkas untuk menangani multiple *success/failure path* tanpa `if-else` bersarang yang dalam.

## 3. Function Composition

### Contoh 1: Pipeline Validasi `validateUniqueSymbols`
**File:** `nano-chesskel/src/Validator.hs` (Line 159)

```haskell
    allSymbols = concatMap (\p -> [symbol_white p, symbol_black p]) pieceDefs
```

**Penjelasan Kode:**
Kode ini mengumpulkan semua simbol karakter dari definisi bidak untuk diperiksa keunikannya.

**Mengapa ini disebut Function Composition:**
*   `concatMap` adalah fungsi yang menggabungkan (komposisi) dua operasi list fundamental: `map` dan `concat`.
*   Secara konseptual, ini setara dengan `concat . map f`.
*   Fungsi `f` (lambda) mengubah satu `PieceDef` menjadi list simbol `[Char]`.
*   `map f` mengubah list `[PieceDef]` menjadi list of lists `[[Char]]`.
*   `concat` meratakan `[[Char]]` menjadi satu list `[Char]`.
*   Penggunaan `concatMap` adalah idiom komposisi fungsi standar untuk pemrosesan list di Haskell.

### Contoh 2: `buildRuleMap`
**File:** `nano-chesskel/src/RuleEngine.hs` (Line 26)

```haskell
buildRuleMap defs = Map.fromList $ map toEntry defs
```

**Penjelasan Kode:**
Membangun Map aturan dari list definisi.

**Mengapa ini disebut Function Composition:**
*   Operator `$` adalah operator aplikasi fungsi dengan presedensi rendah, sering digunakan untuk menghindari kurung, namun secara semantik ia menghubungkan output dari satu fungsi ke input fungsi lain.
*   Ekspresi ini ekuivalen dengan `(Map.fromList . map toEntry) defs`.
*   Data mengalir: `defs` -> diproses `map toEntry` -> hasilnya diproses `Map.fromList`. Kita membangun pipeline transformasi data yang jelas.

## 4. High Order Function

### Contoh 1: `concatMap` pada `getValidMoves`
**File:** `nano-chesskel/src/RuleEngine.hs` (Line 100)

```haskell
      in concatMap evalMove moveRules
```

**Penjelasan Kode:**
`concatMap` digunakan untuk mengevaluasi setiap aturan gerak dalam list `moveRules` menggunakan fungsi `evalMove`, lalu menggabungkan semua hasil posisi yang didapat.

**Mengapa ini disebut High Order Function (HOF):**
*   **Menerima Fungsi:** `concatMap` menerima fungsi `evalMove` sebagai argumen pertama. `evalMove` adalah logika spesifik ("bagaimana mengevaluasi satu aturan"), sedangkan `concatMap` adalah kerangka umum ("bagaimana menerapkan logika itu ke banyak aturan dan menggabungkannya").
*   Pemisahan *iterasi* (tugas `concatMap`) dari *aksi per item* (tugas `evalMove`) adalah ciri utama HOF.

### Contoh 2: `any` pada `hasLegalMoves`
**File:** `nano-chesskel/src/RuleEngine.hs` (Line 183)

```haskell
    any hasMoves myPieces
```

**Penjelasan Kode:**
Fungsi ini memeriksa apakah pemain memiliki setidaknya satu langkah legal (untuk mengecek stalemate/checkmate).

**Mengapa ini disebut High Order Function (HOF):**
*   `any` adalah HOF yang menerima fungsi predikat `hasMoves` (tipe `Piece -> Bool`) dan sebuah list.
*   Ia mengabstraksi logika *short-circuiting logic OR*. Ia akan menerapkan `hasMoves` ke elemen list satu per satu. Segera setelah `hasMoves` mengembalikan `True`, `any` berhenti dan mengembalikan `True`. Kita tidak perlu menulis loop manual dengan `break` atau `return`.

## 5. Lambda Function

### Contoh 1: Lambda pada `validateKingPresence`
**File:** `nano-chesskel/src/Validator.hs` (Line 112)

```haskell
    kingDefined = any (\p -> name p == "King") pieceDefs
```

**Penjelasan Kode:**
Fungsi `any` dipanggil dengan argumen `\p -> name p == "King"`.

**Mengapa ini disebut Lambda Function:**
*   Ini adalah fungsi anonim yang didefinisikan secara *inline*. Kita tidak perlu membuat fungsi bernama `isKing` secara terpisah di file jika fungsinya hanya dipakai sekali di sini.
*   Lambda ini menangkap logika pengecekan ("apakah nama piece p adalah 'King'?").

### Contoh 2: Lambda Closure pada `validatePieceNames`
**File:** `nano-chesskel/src/Validator.hs` (Line 54-59)

```haskell
    check entry =
      let pieceName = piece entry
      in
        when (Set.notMember pieceName definedNames) ...
    
    mapM_ check formation
```

**Penjelasan Kode:**
Fungsi `check` didefinisikan di dalam `let/where` block.

**Mengapa ini disebut Lambda Function (Closure):**
*   Meskipun punya nama lokal `check`, secara fungsional ia bertindak seperti lambda.
*   Yang terpenting, ia adalah **Closure**. Ia "menangkap" variabel `definedNames` dari *scope* luar (lingkup fungsi `validatePieceNames`).
*   Di bahasa prosedural lama (seperti C), pointer fungsi biasanya tidak membawa konteks (variabel luar). Di FP, fungsi membawa lingkungan (environment) tempat ia dibuat. Ini memungkinkan `check` mengakses `definedNames` tanpa harus menerimanya sebagai argumen eksplisit setiap kali dipanggil oleh `mapM_`.

## 6. List Comprehension

### Contoh 1: Generator Infinite List pada `evalSlide`
**File:** `nano-chesskel/src/RuleEngine.hs` (Line 124)

```haskell
        let path = tail [ fromPos `add` (direction `scale` n) | n <- [0..] ]
```

**Penjelasan Kode:**
Kode ini membuat daftar posisi untuk gerakan meluncur (slide).

**Mengapa ini disebut List Comprehension:**
*   Sintaks `[ ... | ... ]` adalah notasi deklaratif untuk membangun list.
*   **Generator:** Bagian `n <- [0..]` membangkitkan nilai `n` dari 0 sampai tak hingga.
*   **Transformasi:** Bagian `fromPos ...` mendefinisikan bentuk elemen list yang dihasilkan.
*   Ini adalah cara matematika untuk mendefinisikan himpunan $\{ Pos_0 + n \cdot \vec{d} \mid n \in \mathbb{N} \}$. Kode ini sangat deklaratif: "Path adalah himpunan posisi hasil penambahan vektor arah dikali n, untuk n bilangan cacah."

### Contoh 2: Rendering Board `renderBoard`
**File:** `nano-chesskel/src/RuleEngine.hs` (Line 138)

```haskell
    rowsAsStrings = [ [cellToChar (Pos r c) | c <- [0..cols-1]] | r <- [rows-1, rows-2 .. 0] ]
```

**Penjelasan Kode:**
Membuat representasi string dari papan catur 2D.

**Mengapa ini disebut List Comprehension:**
*   **Nested Comprehension:** Comprehension di dalam comprehension.
*   List luar mengiterasi baris (`r`) dari atas ke bawah.
*   List dalam mengiterasi kolom (`c`) dari kiri ke kanan.
*   Hasilnya adalah list of list of Char (list of String), yang merepresentasikan grid papan. Ini jauh lebih ringkas daripada *nested for-loops* imperatif.

## 7. Lazy Evaluation

### Contoh 1: Infinite List pada `evalSlide`
**File:** `nano-chesskel/src/RuleEngine.hs` (Line 124)

```haskell
        let path = tail [ fromPos `add` (direction `scale` n) | n <- [0..] ]
```

**Penjelasan Kode:**
`path` didefinisikan sebagai list tak terbatas (karena `[0..]` tak terbatas).

**Mengapa ini disebut Lazy Evaluation:**
*   Dalam bahasa *strict* (seperti Python atau Java), baris ini akan menyebabkan *infinite loop* atau *memory overflow* seketika karena program mencoba membuat list tak hingga.
*   Di Haskell, list ini dibuat secara malas (*lazy*). Elemen-elemennya hanya dihitung satu per satu saat diminta.
*   Fungsi `walk` (yang mengonsumsi `path`) hanya akan mengambil elemen dari `path` sampai ia menabrak batas papan atau bidak lain. Begitu `walk` berhenti meminta, sisa elemen `path` (yang tak hingga itu) tidak pernah dihitung. Ini memungkinkan kita mendefinisikan logika "bergerak lurus selamanya" lalu memotongnya dengan kondisi batas, memisahkan logika "pembangkitan gerak" dari logika "terminasi gerak".

### Contoh 2: Efisiensi `any` pada Deteksi Checkmate
**File:** `nano-chesskel/src/RuleEngine.hs` (Line 183)

```haskell
    any hasMoves myPieces
```

**Penjelasan Kode:**
Mengecek apakah ada setidaknya satu langkah legal.

**Mengapa ini disebut Lazy Evaluation:**
*   `myPieces` mungkin berisi 16 bidak. Menghitung `hasMoves` untuk satu bidak itu mahal (perlu simulasi langkah, cek check, dll).
*   Jika bidak pertama sudah punya langkah legal, `any` akan mengembalikan `True` seketika. Berkat lazy evaluation, perhitungan `hasMoves` untuk 15 bidak sisanya **tidak pernah dieksekusi**. Ini bukan sekadar optimasi *compiler*, tapi semantik bahasa. Ini menjamin performa AI catur tetap responsif tanpa perlu menulis logika *early exit* manual.

## 8. Currying

### Contoh 1: Fungsi `isKingInCheck`
**File:** `nano-chesskel/src/RuleEngine.hs` (Line 160)

```haskell
isKingInCheck :: RuleMap -> BoardSize -> Board -> Color -> Bool
```

Penggunaan (Line 177):
```haskell
        not (isKingInCheck rules size tempBoard player)
```

**Penjelasan Kode:**
Fungsi ini dipanggil dengan 4 argumen.

**Mengapa ini disebut Currying:**
*   Secara teknis, `isKingInCheck` adalah fungsi yang menerima `RuleMap`, lalu mengembalikan fungsi lain yang menerima `BoardSize`, dan seterusnya.
*   Konsep Currying memungkinkan *Partial Application*. Misalkan kita punya `checkCurrentBoard = isKingInCheck rules size board`. `checkCurrentBoard` sekarang adalah fungsi baru bertipe `Color -> Bool`. Kita bisa mem-passing fungsi ini ke filter atau map lain. Meskipun di baris ini aplikasi penuh dilakukan, desain fungsi yang ter-curry adalah fondasi fleksibilitas FP di Haskell.

### Contoh 2: Functor Map `<$>` pada `FromJSON`
**File:** `nano-chesskel/src/Types.hs` (Line 87)

```haskell
        "Step" -> Step <$> v .: "offset"
```

**Penjelasan Kode:**
Menggunakan `<$>` untuk memetakan konstruktor `Step` ke dalam parser JSON.

**Mengapa ini disebut Currying:**
*   `Step` bertipe `Position -> MoveRule`.
*   Parser `v .: "offset"` menghasilkan `Parser Position`.
*   Operator `<$>` (fmap) membutuhkan fungsi `a -> b`. Karena `Step` ter-curry (bisa dianggap sebagai fungsi satu argumen di sini), ia bisa langsung "diangkat" (lifted) ke dalam konteks Parser. Jika `Step` membutuhkan 2 argumen, kita akan menggunakan `<$>` untuk argumen pertama dan `<*>` (Applicative) untuk argumen kedua, memanfaatkan sifat currying untuk aplikasi bertahap.

## 9. Functor

### Contoh 1: Instance `FromJSON` untuk `Position`
**File:** `nano-chesskel/src/Types.hs` (Line 35)

```haskell
    [row, col] <- toList <$> traverse parseJSON arr
```

**Penjelasan Kode:**
Mengubah array JSON menjadi list Haskell menggunakan `toList`.

**Mengapa ini disebut Functor:**
*   `traverse parseJSON arr` mengembalikan nilai dalam konteks `Parser`. Misalkan tipenya `Parser (Vector Value)`.
*   Kita ingin mengubah `Vector Value` di dalam hasil sukses itu menjadi list biasa `[Value]`.
*   Fungsi `toList` melakukan konversi itu.
*   `<$>` (fmap) menerapkan `toList` **di dalam** kulit `Parser`. Jika parsing gagal, `toList` tidak dijalankan. Jika sukses, hasilnya ditransformasi. Ini adalah esensi Functor: mengubah nilai di dalam konteks tanpa membuka konteksnya.

### Contoh 2: Parsing `MoveRule`
**File:** `nano-chesskel/src/Types.hs` (Line 87-89)

```haskell
      "Step" -> Step <$> v .: "offset"
      "Jump" -> Jump <$> v .: "offset"
```

**Penjelasan Kode:**
Mem-parsing JSON object menjadi ADT `MoveRule`.

**Mengapa ini disebut Functor:**
*   Parser JSON adalah sebuah Functor.
*   `v .: "offset"` adalah komputasi parser yang menghasilkan `Position`.
*   Kita ingin hasil akhirnya adalah `MoveRule` (khususnya varian `Step`).
*   Kita menggunakan `<$>` untuk menerapkan konstruktor `Step` (yang bertindak sebagai fungsi murni `Position -> MoveRule`) ke hasil parser tersebut. Hasil akhirnya adalah `Parser MoveRule`.

## 10. Applicative

### Contoh 1: `validateFormationCollisions` menggunakan `foldr`
**Analisis:**
Meskipun snippet kode yang diberikan tidak menunjukkan penggunaan operator applicative `<*>` atau `<*>` secara eksplisit yang dominan, konsep Applicative ada di balik layar `mapM_` yang digunakan luas di `Validator.hs`.

Misalnya:
```haskell
mapM_ check formation
```
`mapM_` menelusuri list, menjalankan aksi validasi untuk setiap elemen, dan menggabungkan efeknya (dalam hal ini, efeknya adalah kemungkinan kegagalan `Either String`). Ini adalah operasi sekuensial yang memanfaatkan struktur Applicative/Monad untuk menggabungkan hasil (atau menghentikan proses pada error pertama).

## 11. Monad

### Contoh 1: Validasi dengan `Either` Monad
**File:** `nano-chesskel/src/Validator.hs` (Line 21-43)

```haskell
validateRuleSet rs = do
  -- ...
  validatePieceNames pieceDefs formationEntries
  validateFormationPositions boardSize formationEntries
  -- ...
  return rs
```

**Penjelasan Kode:**
Fungsi ini melakukan serangkaian validasi berurutan.

**Mengapa ini disebut Monad:**
*   **Konteks Kegagalan (Failure Context):** Konteks di sini adalah `Either String`.
*   **Short-Circuiting:** Operator bind (`>>=`) yang tersembunyi di balik `do` notation memiliki logika: "Jalankan langkah 1. Jika gagal (`Left`), **berhenti** dan kembalikan error itu. Jangan jalankan langkah 2. Jika sukses (`Right`), jalankan langkah 2."
*   Pola ini disebut "Railway Oriented Programming". Kita menulis kode seolah-olah semuanya sukses (jalur rel hijau), dan Monad menangani percabangan error (jalur rel merah) secara otomatis di balik layar. Ini membuat kode validasi sangat bersih dan linier tanpa blok `if err != null` yang berulang-ulang.

### Contoh 2: IO Monad pada `gameLoop`
**File:** `nano-chesskel/app/Main.hs` (Line 57)

```haskell
gameLoop rules size state = do
  -- ...
  putStrLn $ "--- Turn: " ++ show player ++ " ---"
  line <- getLine
  -- ...
```

**Penjelasan Kode:**
Fungsi utama loop permainan.

**Mengapa ini disebut Monad:**
*   **Sequencing Side Effects:** IO (Input/Output) di Haskell adalah murni. `putStrLn` tidak "mencetak ke layar" saat itu juga, tapi "menghasilkan deskripsi aksi pencetakan".
*   Monad IO menggabungkan deskripsi-deskripsi aksi ini menjadi satu urutan eksekusi yang terjamin.
*   Notasi `line <- getLine` menunjukkan ketergantungan data: aksi berikutnya membutuhkan nilai `line` yang dihasilkan oleh aksi `getLine`. Monad menjamin `getLine` selesai dieksekusi dan hasilnya tersedia sebelum baris kode berikutnya jalan.

## 12. Monoid

**Analisis:**
Tidak ditemukan implementasi eksplisit typeclass **Monoid** (seperti `mempty`, `mappend`) pada snippet kode. Penggabungan list menggunakan `++` (yang secara matematis adalah operasi monoid list), namun kode tidak memanfaatkan abstraksi Monoid polimorfik.

## 13. QuickCheck

**Analisis:**
Tidak terdapat implementasi **QuickCheck** pada codebase ini. File `nano-chesskel/test/Spec.hs` hanya berisi placeholder. Proyek ini belum menerapkan *Property-Based Testing* otomatis.