# Analisis Konsep Functional Programming pada Proyek johan-eval

Berikut adalah analisis penerapan konsep-konsep Functional Programming pada codebase `johan-eval`.

## 1. Algebraic Data Type (ADT)

### Contoh 1: `data Expr`
**File:** `johan-eval/app/Main.hs` (Line 14-30)

```haskell
data Expr
  = Num Double                    -- literal number
  | Var                           -- variable x
  | Add Expr Expr                 -- a + b
  | Sub Expr Expr                 -- a - b
  -- ... (konstruktor lain)
  | Deriv Expr Expr               -- deriv(f, point)
  | Integral Expr Expr Expr       -- integral(f, a, b)
  deriving (Show, Eq)
```

**Penjelasan Kode:**
Kode ini mendefinisikan tipe data `Expr` yang merepresentasikan pohon sintaks abstrak (AST) dari ekspresi matematika. `Expr` memiliki berbagai bentuk (konstruktor) seperti angka (`Num`), variabel (`Var`), atau operasi biner (`Add`, `Sub`) yang secara rekursif memuat `Expr` lain di dalamnya.

**Mengapa ini disebut Algebraic Data Type (ADT):**
ADT adalah tipe data yang dibentuk dengan menggabungkan tipe lain menggunakan dua operasi aljabar utama: **Sum** (penjumlahan) dan **Product** (perkalian).
*   **Sum Type:** `Expr` adalah *sum type* karena sebuah nilai `Expr` bisa berupa `Num` **ATAU** `Var` **ATAU** `Add`, dan seterusnya. Pilihan ini dilambangkan dengan karakter pipe (`|`). Kita memilih salah satu varian konstruktor.
*   **Product Type:** Konstruktor seperti `Add Expr Expr` adalah *product type*. Untuk membuat nilai `Add`, kita memerlukan **DUA** nilai `Expr` sekaligus (operand kiri **DAN** operand kanan). Ini mirip dengan tuple `(Expr, Expr)`.
*   **Rekursif:** Sifat rekursif `Expr` (misalnya `Add` memuat `Expr`) adalah ciri khas ADT untuk merepresentasikan struktur data pohon yang kompleks secara elegan dan aman (type-safe).

### Contoh 2: `data Result`
**File:** `johan-eval/app/Main.hs` (Line 34-37)

```haskell
Result
  = Value Double
  | Error String
  deriving (Show, Eq)
```

**Penjelasan Kode:**
Tipe data `Result` digunakan untuk membungkus hasil evaluasi. Ia bisa sukses dan membawa nilai `Double` (`Value`), atau gagal dan membawa pesan error `String` (`Error`).

**Mengapa ini disebut Algebraic Data Type (ADT):**
Ini adalah contoh klasik *Sum Type*. Sebuah nilai bertipe `Result` hanya bisa berada dalam satu dari dua keadaan: `Value` **ATAU** `Error`.
*   Konstruktor `Value` membawa *payload* bertipe `Double`.
*   Konstruktor `Error` membawa *payload* bertipe `String`.
Konsep ini menggantikan mekanisme *exception handling* imperatif (seperti `try-catch`) dengan representasi data eksplisit. Kita dipaksa oleh sistem tipe untuk menangani kedua kemungkinan ini saat menggunakan nilai `Result`.

## 2. Pattern Matching

### Contoh 1: Fungsi `satisfy`
**File:** `johan-eval/app/Main.hs` (Line 69-71)

```haskell
satisfy f = Parser $ \case
  (x:xs) | f x -> Just (x,xs)
  _            -> Nothing
```

**Penjelasan Kode:**
Fungsi `satisfy` membuat parser dasar. Ia menerima fungsi predikat `f`. Di dalamnya, ia menggunakan ekstensi `LambdaCase` untuk memeriksa input string. Jika input adalah list tidak kosong `(x:xs)` dan karakter pertama `x` memenuhi `f`, parser sukses. Jika tidak (list kosong atau `f x` salah), parser gagal (`Nothing`).

**Mengapa ini disebut Pattern Matching:**
Pattern matching adalah mekanisme untuk "membongkar" struktur data dan memeriksa bentuknya sekaligus.
*   **De-strukturisasi:** Pola `(x:xs)` secara langsung memecah list input menjadi kepala (`x`) dan ekor (`xs`). Kita tidak perlu memanggil fungsi `head` atau `tail` secara manual.
*   **Pencocokan:** Kode ini mencocokkan bentuk data input. Apakah input berbentuk list dengan minimal satu elemen? Jika ya, variabel `x` dan `xs` terikat nilainya.
*   **Wildcard:** Pola `_` (underscore) adalah *catch-all pattern* yang cocok dengan *apa saja*. Di sini, ia menangani semua kasus yang tidak tertangani oleh pola sebelumnya (misalnya string kosong `""`), memastikan fungsi total dan aman dari *runtime error*.

### Contoh 2: Fungsi `evalAt`
**File:** `johan-eval/app/Main.hs` (Line 200-241)

```haskell
evalAt :: Expr -> Double -> Result
evalAt e xv = case e of
  Num n -> Value n
  Var -> Value xv
  Add a b -> bin (+) a b
  -- ...
  Integral f a b ->
    case (evalAt a xv, evalAt b xv) of
      (Value aa, Value bb) -> numericIntegral f aa bb
      (Error e, _) -> Error e
      (_, Error e) -> Error e
```

**Penjelasan Kode:**
Fungsi ini mengevaluasi ekspresi `Expr` pada nilai `x` tertentu (`xv`). Ia menggunakan `case ... of` untuk memeriksa konstruktor mana yang membentuk `e`.

**Mengapa ini disebut Pattern Matching:**
*   **Dispatching Logika:** Pattern matching di sini berfungsi sebagai *switch-case* yang jauh lebih kuat. Ia mengarahkan eksekusi program berdasarkan *struktur* data `Expr`. Jika `e` adalah `Num n`, ambil `n`. Jika `e` adalah `Add a b`, ambil sub-ekspresi `a` dan `b` lalu proses.
*   **Nested Pattern Matching:** Pada bagian `Integral`, terjadi *nested pattern matching* terhadap tuple `(evalAt a xv, evalAt b xv)`.
    *   Pola `(Value aa, Value bb)` hanya cocok jika **kedua** evaluasi batas integral berhasil.
    *   Pola `(Error e, _)` cocok jika evaluasi batas bawah gagal, menangkap pesan errornya dalam `e`, dan mengabaikan hasil batas atas.
Ini menunjukkan kekuatan pattern matching untuk menangani logika kontrol alur yang kompleks secara visual deklaratif.

## 3. Function Composition

### Contoh 1: Fungsi `charP`
**File:** `johan-eval/app/Main.hs` (Line 74)

```haskell
charP :: Char -> Parser Char
charP = satisfy . (==)
```

**Penjelasan Kode:**
Fungsi `charP` membuat parser yang mengenali karakter spesifik. Ia didefinisikan dengan menggabungkan fungsi `satisfy` (yang menerima predikat) dengan operator kesamaan `(==)`.

**Mengapa ini disebut Function Composition:**
*   **Operator `(.)`:** Operator titik (`.`) adalah operator komposisi fungsi matematis $(f \circ g)(x) = f(g(x))$.
*   **Penggabungan Logika:** Di sini, `(==)` adalah fungsi yang menerima karakter target dan karakter input, lalu mengembalikan `Bool`. `satisfy` adalah fungsi yang menerima fungsi `Bool` (predikat).
*   Ekspresi `satisfy . (==)` menciptakan fungsi baru secara *point-free* (tanpa menyebutkan argumen karakter inputnya secara eksplisit). Alur datanya adalah: Argumen karakter (misal `'a'`) diberikan ke `(==)`, menghasilkan fungsi predikat `(== 'a')`. Predikat ini kemudian langsung diteruskan ("dipipakan") ke fungsi `satisfy`.

## 4. High Order Function

### Contoh 1: Fungsi `satisfy`
**File:** `johan-eval/app/Main.hs` (Line 68-69)

```haskell
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case ...
```

**Penjelasan Kode:**
`satisfy` adalah fungsi dasar pembangun parser. Ia menerima satu argumen `f`.

**Mengapa ini disebut High Order Function (HOF):**
Sebuah fungsi disebut HOF jika ia melakukan salah satu (atau kedua) hal berikut:
1.  Menerima fungsi lain sebagai argumen.
2.  Mengembalikan fungsi sebagai hasil.

Dalam kasus `satisfy`:
*   **Menerima Fungsi:** Argumen `f` memiliki tipe `(Char -> Bool)`, yang merupakan sebuah fungsi (predikat). `satisfy` tidak tahu logika spesifik apa yang dicek (apakah huruf besar, digit, atau spasi); ia hanya menjalankan logika `f` yang diberikan pemanggil. Ini adalah bentuk *abstraksi perilaku*.

### Contoh 2: Fungsi `foldl` dalam `numericIntegral`
**File:** `johan-eval/app/Main.hs` (Line 302)

```haskell
    in case foldl accumulate (Right 0.0) indices of
```

Konteks fungsi `accumulate`:
```haskell
          accumulate acc i = ... -- fungsi lokal
```

**Penjelasan Kode:**
`numericIntegral` menghitung integral tentu menggunakan metode Simpson. Ia menggunakan `foldl` untuk menjumlahkan kontribusi area dari setiap segmen.

**Mengapa ini disebut High Order Function (HOF):**
*   `foldl` (fold left) adalah HOF standar yang sangat powerful untuk iterasi list.
*   **Parameter Fungsi:** Ia menerima fungsi `accumulate` sebagai argumen pertama. Fungsi `accumulate` mendefinisikan *bagaimana* cara menggabungkan elemen saat ini (`i`) dengan hasil akumulasi sebelumnya (`acc`).
*   Dengan menggunakan HOF `foldl`, kita mengabstraksi mekanisme *looping* (rekursi). Kita tidak menulis `for` loop manual; kita hanya memberikan logika penggabungan (`accumulate`), dan `foldl` yang mengurus iterasi listnya.

## 5. Lambda Function

### Contoh 1: Instance `Functor` untuk `Parser`
**File:** `johan-eval/app/Main.hs` (Line 44)

```haskell
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    Just (f x, rest)
```

**Penjelasan Kode:**
Bagian `\input -> ...` mendefinisikan sebuah fungsi anonim (tanpa nama) yang menerima satu argumen `input`.

**Mengapa ini disebut Lambda Function:**
*   **Fungsi Anonim:** Sintaks `\` (backslash, mirip $\lambda$) digunakan untuk mendefinisikan fungsi *on-the-fly*. Fungsi ini tidak diberi nama `foo` atau `bar` di tataran top-level.
*   **First-Class Value:** Lambda ini langsung digunakan sebagai nilai pembungkus di dalam konstruktor `Parser`. Di Haskell, fungsi adalah *first-class citizen* yang bisa dibuat di mana saja dan dipassing ke mana saja, sama seperti integer atau string.

### Contoh 2: Di dalam `funcP`
**File:** `johan-eval/app/Main.hs` (Line 162)

```haskell
    m2 <- (charP ',' *> expr >>= \b -> pure (Just b)) <|> pure Nothing
```

**Penjelasan Kode:**
Bagian `\b -> pure (Just b)` adalah lambda function.

**Mengapa ini disebut Lambda Function:**
*   Lambda ini digunakan sebagai argumen untuk operator bind (`>>=`).
*   Operator bind mengharapkan fungsi di sebelah kanannya. Alih-alih mendefinisikan fungsi bernama terpisah seperti `wrapInJust b = pure (Just b)`, kita menulisnya secara ringkas di tempat (inline) menggunakan lambda `\b -> ...`. Ini membuat kode lebih ringkas untuk logika transformasi sederhana.

## 6. List Comprehension

**Analisis:**
Tidak terdapat implementasi **List Comprehension** pada codebase ini. Kode lebih banyak menggunakan kombinator parser atau fungsi rekursif/HOF standar. Sebagai contoh, pembuatan list range `[0 .. n']` dilakukan dengan sintaks range biasa, bukan comprehension.

## 7. Lazy Evaluation

### Contoh 1: List Range `[0 .. n']`
**File:** `johan-eval/app/Main.hs` (Line 292)

```haskell
          indices = [0 .. n']
```

**Penjelasan Kode:**
Variabel `indices` didefinisikan sebagai list integer dari 0 hingga `n'`. Di baris 302, list ini dikonsumsi oleh `foldl`.

**Mengapa ini disebut Lazy Evaluation:**
*   **Penundaan Komputasi:** Di Haskell, ekspresi tidak dievaluasi sampai nilainya benar-benar dibutuhkan (*call-by-need*). Definisi `indices` tidak langsung membuat array integer di memori. Ia hanya membuat "janji" (thunk) untuk menghasilkan list tersebut.
*   **Evaluasi Bertahap:** Saat `foldl` berjalan, ia meminta elemen list satu per satu. Haskell akan men-generate angka 0, lalu memberikannya ke `accumulate`, lalu melupakannya (jika tidak ada referensi lain), lalu men-generate angka 1, dan seterusnya.
*   Ini memungkinkan efisiensi memori yang tinggi bahkan jika `n'` sangat besar (misal jutaan), karena list tidak pernah dimuat utuh ke memori sekaligus.

## 8. Currying

### Contoh 1: Penggunaan Operator Binari `bin`
**File:** `johan-eval/app/Main.hs` (Line 204-206)

```haskell
  evalAt :: Expr -> Double -> Result
  evalAt e xv = case e of
    -- ...
    Add a b -> bin (+) a b
    Sub a b -> bin (-) a b
    Mul a b -> bin (*) a b
```

**Penjelasan Kode:**
Fungsi `bin` dipanggil dengan argumen pertama berupa operator: `(+)`, `(-)`, atau `(*)`.

**Mengapa ini disebut Currying:**
*   **Fungsi sebagai Argumen:** Operator `(+)` sebenarnya adalah fungsi bertipe `Double -> Double -> Double`.
*   **Aplikasi Parsial:** Fungsi `bin` didefinisikan (di klausa `where` yang tidak ditampilkan penuh di sini) menerima 3 argumen: `op`, `a`, `b`.
*   Konsep Currying berarti fungsi multi-argumen sebenarnya adalah rangkaian fungsi satu argumen. Tipe `(+)` bisa dibaca sebagai `Double -> (Double -> Double)`. Kita bisa mem-passing `(+)` "apa adanya" ke fungsi `bin` tanpa harus memberikan kedua operandnya (angka) saat itu juga. Fungsi `bin` nanti yang akan "mengisi" argumen untuk `(+)` di dalam badan fungsinya. Ini memungkinkan fleksibilitas luar biasa dalam *higher order function*.

### Contoh 2: Konstruktor `Add`
**File:** `johan-eval/app/Main.hs` (Line 128)

```haskell
addop =
      (Add <$ charP '+')
```

**Penjelasan Kode:**
Operator `<$` (dari Functor) mengganti hasil parser `charP '+'` dengan nilai `Add`.

**Mengapa ini disebut Currying:**
*   `Add` adalah konstruktor tipe data `Expr`. Tipenya adalah `Expr -> Expr -> Expr`.
*   Di Haskell, konstruktor ADT juga merupakan fungsi yang ter-curry secara otomatis. 
*   Di sini, `Add` digunakan sebagai *nilai fungsi* (tanpa argumen). Parser `addop` pada akhirnya mengembalikan fungsi `Add` yang "menunggu" dua argumen `Expr`. Fungsi ini nanti akan diaplikasikan oleh parser `chainl1` (Line 111) ketika operand kiri dan kanan sudah berhasil di-parse. Jika `Add` tidak ter-curry (harus menerima semua argumen sekaligus seperti di C/Java), pola kombinator parser yang elegan ini tidak akan bekerja.

## 9. Functor

### Contoh 1: Instance Functor `Parser`
**File:** `johan-eval/app/Main.hs` (Line 43-46)

```haskell
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    Just (f x, rest)
```

**Penjelasan Kode:**
Kode ini mengimplementasikan *typeclass* `Functor` untuk tipe `Parser`.

**Mengapa ini disebut Functor:**
*   **Pemetaan Struktur:** Konsep Functor adalah tentang menerapkan fungsi ke nilai yang "terbungkus" dalam suatu konteks, tanpa merusak konteks tersebut.
*   Di sini, konteksnya adalah `Parser`. `fmap f` mengambil sebuah `Parser a` (parser yang menghasilkan tipe `a`) dan mengubahnya menjadi `Parser b` (parser yang menghasilkan tipe `b`) dengan menggunakan fungsi transformasi `f :: a -> b`.
*   Perhatikan implementasinya: struktur parsing (menerima input, menghasilkan sisa string `rest`) tetap sama. Yang berubah hanya nilai hasilnya (`x` menjadi `f x`).

### Contoh 2: Penggunaan `<$>` (fmap infix)
**File:** `johan-eval/app/Main.hs` (Line 96)

```haskell
signedDoubleP = (negate <$> (charP '-' *> doubleP)) <|> doubleP
```

**Penjelasan Kode:**
`(negate <$> ...)` menerapkan fungsi `negate` pada hasil parser.

**Mengapa ini disebut Functor:**
*   `charP '-' *> doubleP` adalah parser yang membaca tanda minus lalu membaca angka double, dan mengembalikan angka tersebut (misal `5.0`).
*   Operator `<$>` adalah alias untuk `fmap`. Ia "menyuntikkan" fungsi `negate` ke dalam proses parser tersebut.
*   Hasilnya adalah parser baru yang ketika dijalankan, otomatis menegasikannya (menjadi `-5.0`) sebelum dikembalikan ke pemanggil. Ini cara deklaratif untuk memodifikasi data dalam pipeline pemrosesan.

## 10. Applicative

### Contoh 1: Instance Applicative `Parser`
**File:** `johan-eval/app/Main.hs` (Line 48-53)

```haskell
instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser pf) <*> (Parser px) = ...
```

**Penjelasan Kode:**
Ini adalah implementasi *typeclass* `Applicative` untuk `Parser`.

**Mengapa ini disebut Applicative:**
*   **Komputasi Berkonteks Ganda:** Applicative lebih kuat dari Functor. Jika Functor memetakan fungsi biasa (`a -> b`) ke nilai terbungkus (`f a`), Applicative memetakan *fungsi terbungkus* (`f (a -> b)`) ke nilai terbungkus (`f a`).
*   Dalam konteks parser, `<*>` (disebut *apply*) memungkinkan kita menjalankan dua parser berurutan. Parser pertama (`pf`) menghasilkan fungsi, parser kedua (`px`) menghasilkan data. `<*>` menggabungkan keduanya: menjalankan `pf` pada input, lalu menjalankan `px` pada sisa input, lalu mengaplikasikan fungsi hasil `pf` ke data hasil `px`. Ini fundamental untuk mem-parse struktur berurutan.

### Contoh 2: Kombinasi Parser `*>` dan `<*`
**File:** `johan-eval/app/Main.hs` (Line 124)

```haskell
    <|> (charP '(' *> expr <* charP ')')
```

**Penjelasan Kode:**
Kode ini mem-parse ekspresi dalam kurung: `( ... )`.

**Mengapa ini disebut Applicative:**
*   Operator `*>` dan `<*` adalah varian dari operator Applicative yang membuang salah satu hasilnya.
*   `charP '(' *> expr`: Jalankan parser kurung buka, lalu jalankan parser `expr`. Ambil hasil `expr`, buang hasil kurung buka.
*   `... <* charP ')'`: Ambil hasil sebelumnya (`expr`), jalankan parser kurung tutup. Ambil hasil `expr`, buang hasil kurung tutup.
*   Ini menunjukkan kekuatan Applicative untuk mengurutkan *efek* (dalam hal ini, konsumsi karakter input) sambil memilih data mana yang ingin disimpan, sangat berguna untuk membuang karakter sintaksis (delimiter) seperti tanda kurung.

## 11. Monad

### Contoh 1: Instance Monad `Parser`
**File:** `johan-eval/app/Main.hs` (Line 55-59)

```haskell
instance Monad Parser where
  (Parser p) >>= f = Parser $ \input -> do
    (x, rest) <- p input
    runParser (f x) rest
  return = pure
```

**Penjelasan Kode:**
Implementasi *typeclass* `Monad` untuk `Parser`. Fungsi utamanya adalah `>>=` (bind).

**Mengapa ini disebut Monad:**
*   **Ketergantungan Sekuensial:** Monad memungkinkan komputasi berantai di mana langkah kedua *bergantung* pada hasil langkah pertama.
*   Dalam `>>=` (bind): Parser pertama `p` dijalankan, menghasilkan `x`. Kemudian, fungsi `f` dipanggil dengan nilai `x` tersebut (`f x`). Fungsi `f` ini *menghasilkan parser baru*. Parser baru inilah yang kemudian dijalankan pada sisa input (`rest`).
*   Ini berbeda dengan Applicative statis. Di Monad, struktur parser selanjutnya bisa berubah total tergantung apa yang dibaca sebelumnya (konteks sensitif).

### Contoh 2: `do` notation pada `funcP`
**File:** `johan-eval/app/Main.hs` (Line 158-166)

```haskell
funcP :: Parser Expr
funcP = do
  fname <- identP
  charP '('
  a <- expr
  m2 <- (charP ',' *> expr >>= \b -> pure (Just b)) <|> pure Nothing
  -- ...
```

**Penjelasan Kode:**
Blok `do` digunakan untuk mengurutkan langkah parsing fungsi: baca nama -> baca kurung -> baca argumen -> baca argumen opsional.

**Mengapa ini disebut Monad:**
*   **Syntactic Sugar:** `do` notation hanyalah cara manis menulis rangkaian operator `>>=`. 
*   Baris `fname <- identP` berarti: jalankan parser `identP`, ambil hasilnya, ikat ke variabel `fname`. Variabel `fname` ini sekarang tersedia untuk baris-baris di bawahnya. Ini menciptakan nuansa pemrograman imperatif (urutan instruksi) di dalam paradigma fungsional murni, menangani *state* (string input yang berkurang) secara implisit di balik layar Monad Parser.

## 12. Monoid

**Analisis:**
Tidak ditemukan implementasi eksplisit atau penggunaan konsep **Monoid** yang signifikan pada file `Main.hs`. Meskipun list `[a]` adalah Monoid (dengan `++` dan `[]`), penggunaannya di sini adalah standar list manipulation, bukan abstraksi Monoid polimorfik (misal `mconcat` atau `foldMap`). Tidak ada instance `Monoid` yang didefinisikan.

## 13. QuickCheck

### Contoh 1: Properti `prop_add_comm`
**File:** `johan-eval/app/Main.hs` (Line 309-311)

```haskell
prop_add_comm :: Double -> Double -> Bool
prop_add_comm x y =
  eval (Add (Num x) (Num y))
  == eval (Add (Num y) (Num x))
```

**Penjelasan Kode:**
Fungsi ini mendefinisikan sebuah properti matematika: komutativitas penjumlahan. Ia menyatakan bahwa `x + y` harus sama dengan `y + x` dalam sistem evaluator yang dibuat.

**Mengapa ini disebut QuickCheck (Property-Based Testing):**
*   **Spesifikasi vs Contoh:** Alih-alih menulis *test case* spesifik (misal `1 + 2 == 3`), kita menulis *spesifikasi* general (untuk *semua* `x` dan `y`, urutan penjumlahan tidak mempengaruhi hasil).
*   **Generator Otomatis:** QuickCheck akan membaca tipe argumen fungsi ini (`Double`), lalu secara otomatis membangkitkan ratusan pasangan angka acak (positif, negatif, nol, pecahan, besar, kecil) dan mengujinya ke fungsi ini. Jika ada satu saja pasangan yang gagal (hasilnya `False`), test dianggap gagal.

### Contoh 2: Menjalankan Test (`runAllTests`)
**File:** `johan-eval/app/Main.hs` (Line 354-355)

```haskell
runAllTests :: IO ()
runAllTests = do
  quickCheck prop_parseFloor
  quickCheck prop_parseCeil
```

**Penjelasan Kode:**
Fungsi `quickCheck` dipanggil untuk mengeksekusi properti.

**Mengapa ini disebut QuickCheck:**
*   Ini adalah *entry point* perpustakaan QuickCheck. Ia menghubungkan generator data acak dengan fungsi properti (predikat) yang kita tulis, menjalankan loop pengujian, dan melaporkan hasilnya ke terminal. Ini adalah penerapan otomatisasi pengujian kualitas perangkat lunak berbasis properti fungsional.
