# Analisis Konsep Functional Programming pada Proyek cabo-card-functional

Berikut adalah analisis penerapan konsep-konsep Functional Programming pada codebase `cabo-card-functional`.

## 1. Algebraic Data Type (ADT)

### Contoh 1: `data GamePhase`
**File:** `cabo-card-functional/src/GameStates.hs` (Line 11-25)

```haskell
data GamePhase
    = InitialPeekPhase [Int]    
    | InitPeekFeedback Int [Int] 
    | DrawPhase
    | DiscardPhase
    | TimpaRound 
        { targetRank :: Rank
        , originIdx :: Int
        , askingIdx :: Int
        , savedPowerup :: Powerup
        }
    | ResolvePowerup Powerup
    | PostRoundDecision       
    | GameOver
    deriving (Show, Eq, Generic, ToJSON, FromJSON)
```

**Penjelasan Kode:**
`GamePhase` adalah struktur data yang merepresentasikan status (state machine) permainan.

**Mengapa ini disebut Algebraic Data Type (ADT):**
*   **Sum Type:** Tipe ini adalah gabungan dari berbagai kemungkinan fase permainan. Variabel bertipe `GamePhase` hanya bisa berupa `DrawPhase` **ATAU** `DiscardPhase` **ATAU** `TimpaRound`, dan seterusnya.
*   **Product Type:** Konstruktor `TimpaRound` adalah *product type* karena ia menggabungkan beberapa data sekaligus (`Rank`, `Int`, `Int`, `Powerup`) dalam satu kesatuan (seperti record).
*   Desain ini memungkinkan kita memodelkan *State Machine* secara eksplisit dan aman. Compiler akan memaksa kita menangani setiap fase ini saat menulis logika permainan, mencegah *invalid state transition*.

### Contoh 2: `data Card`
**File:** `cabo-card-functional/src/Card.hs` (Line 47-48)

```haskell
data Card = Card {rank :: Rank, suit:: Suit, powerup :: Powerup} 
  deriving (Eq, Generic, ToJSON, FromJSON)
```

**Penjelasan Kode:**
Mendefinisikan entitas kartu permainan.

**Mengapa ini disebut Algebraic Data Type (ADT):**
*   Ini adalah *Product Type* (Record). Sebuah `Card` dibentuk dari perkalian (gabungan) tiga himpunan tipe data lain: `Rank` x `Suit` x `Powerup`.
*   Tanpa ketiga komponen ini, sebuah nilai `Card` tidak bisa dibuat.

## 2. Pattern Matching

### Contoh 1: `applyAction`
**File:** `cabo-card-functional/src/GameEngine.hs` (Line 16-25)

```haskell
applyAction ofGameState (InitPeekAction p i1 i2) = logicInitPeek ofGameState p i1 i2
applyAction ofGameState (DrawAction _)           = logicDraw ofGameState
applyAction ofGameState (DiscardAction _ idx)    = logicDiscard ofGameState idx
-- ...
```

**Penjelasan Kode:**
Fungsi `applyAction` bertugas mendistribusikan logika berdasarkan aksi yang diterima.

**Mengapa ini disebut Pattern Matching:**
*   **Dispatching:** Fungsi ini tidak menggunakan `if action == DrawAction` atau `switch (action.type)`. Ia mencocokkan *konstruktor* dari tipe data `GameAction`.
*   **Destructuring:** Perhatikan baris `(InitPeekAction p i1 i2)`. Pattern matching tidak hanya mengecek jenis aksi, tetapi sekaligus mengekstrak variabel `p`, `i1`, dan `i2` dari dalam struktur data tersebut agar bisa langsung digunakan di sisi kanan persamaan.

### Contoh 2: `powerRules`
**File:** `cabo-card-functional/src/Card.hs` (Line 71-84)

```haskell
powerRules :: Rank -> Suit -> Powerup
powerRules Joker Black      = PeekDouble
powerRules Joker Red        = Normal
powerRules King Clubs       = PeekSwitch
-- ...
```

**Penjelasan Kode:**
Fungsi ini menentukan efek (powerup) dari sebuah kartu.

**Mengapa ini disebut Pattern Matching:**
*   Ini adalah definisi fungsi berbasis pola. Alih-alih satu blok kode besar dengan banyak `if-else`, kita mendefinisikan "fakta-fakta" kecil.
*   Pola `Joker Black` mencocokkan argumen `Rank` yang bernilai `Joker` **DAN** `Suit` yang bernilai `Black`.
*   Pola `_ _ = Normal` di baris terakhir bertindak sebagai *default case* (catch-all) untuk semua kombinasi kartu yang tidak didefinisikan secara spesifik sebelumnya.

## 3. Function Composition

### Contoh 1: Operator `.&&.`
**File:** `cabo-card-functional/src/GameRules.hs` (Line 81)

```haskell
(r1 .&&. r2) ofGameState action = r1 ofGameState action >> r2 ofGameState action
```

**Penjelasan Kode:**
Operator infix kustom untuk menggabungkan dua aturan (`Rule`).

**Mengapa ini disebut Function Composition:**
*   Meskipun tidak menggunakan operator titik (`.`), ini adalah esensi komposisi fungsi dalam konteks validasi.
*   Kita menggabungkan fungsi `r1` dan `r2` menjadi fungsi baru. Output (efek samping/error) dari `r1` diteruskan ke `r2` menggunakan operator monadik `>>` (sekuensial). Jika `r1` sukses, jalankan `r2`. Jika `r1` gagal, kembalikan error `r1`.

### Contoh 2: `applyPenaltyPoints`
**File:** `cabo-card-functional/src/GameEngine.hs` (Line 245)

```haskell
    let sorted = sortBy (compare `on` (handScore.hand)) (players ofGameState)
```

**Penjelasan Kode:**
Mengurutkan pemain berdasarkan skor tangan.

**Mengapa ini disebut Function Composition:**
*   `(handScore.hand)` menggabungkan dua fungsi: `hand` (ambil objek Hand dari Player) dan `handScore` (hitung skor dari Hand).
*   Fungsi komposit ini `Player -> Int` kemudian digunakan oleh fungsi `on` untuk menentukan dasar perbandingan bagi `compare`.

## 4. High Order Function

### Contoh 1: `gameRules`
**File:** `cabo-card-functional/src/GameRules.hs` (Line 86)

```haskell
gameRules = isPlayerTurn .&&. isPhaseCorrect .&&. isValidIndex
```

**Penjelasan Kode:**
Mendefinisikan aturan permainan gabungan.

**Mengapa ini disebut High Order Function (HOF):**
*   Operator `.&&.` adalah HOF. Ia menerima dua fungsi (`Rule`) sebagai input dan menghasilkan fungsi `Rule` baru sebagai output.
*   Ini memungkinkan kita membangun logika validasi yang kompleks dari blok-blok kecil yang sederhana dan terisolasi (`isPlayerTurn`, dll).

### Contoh 2: `map` pada `handScore`
**File:** `cabo-card-functional/src/Player.hs` (Line 17)

```haskell
handScore (Hand hands) = sum (map cardValue hands)
```

**Penjelasan Kode:**
Menghitung total nilai kartu di tangan.

**Mengapa ini disebut High Order Function (HOF):**
*   `map` adalah HOF yang menerima fungsi `cardValue` dan list `hands`.
*   Ia menerapkan `cardValue` ke setiap kartu dalam list, mengubah `[Card]` menjadi `[Int]`, tanpa kita perlu menulis loop manual.

## 5. Lambda Function

### Contoh 1: Lambda pada `show` GameState
**File:** `cabo-card-functional/src/GameEngine.hs` (Line 233)

```haskell
    ofGameState 
    { 
        players = map (\p -> p { matchPoints = matchPoints p + lookupPt (playerId p) pointsMap }) (players ofGameState) 
    }
```

**Penjelasan Kode:**
Memperbarui poin pemain di akhir ronde.

**Mengapa ini disebut Lambda Function:**
*   `\p -> ...` adalah fungsi anonim.
*   Fungsi ini mendefinisikan logika transformasi untuk satu pemain: "ambil pemain `p`, buat salinannya dengan field `matchPoints` yang sudah ditambah".
*   Lambda ini dibuat *in-place* dan langsung dipassing ke fungsi `map`.

### Contoh 2: Lambda pada `logicKabul`
**File:** `cabo-card-functional/src/GameEngine.hs` (Line 212)

```haskell
        allScores   = map (\p -> (playerId p, handScore (hand p))) playerList
```

**Penjelasan Kode:**
Membuat daftar skor untuk log.

**Mengapa ini disebut Lambda Function:**
*   Fungsi anonim `\p -> ...` mengambil data `Player` dan mengubahnya menjadi tuple `(ID, Score)`.
*   Penggunaan lambda membuat kode ringkas karena logika transformasi ini sederhana dan hanya dipakai di satu tempat.

## 6. List Comprehension

### Contoh 1: `buildDeck`
**File:** `cabo-card-functional/src/Card.hs` (Line 93-96)

```haskell
      standard =
        [ Card r s (powerRules r s)
        | r <- [Ace .. King]
        , s <- [Hearts, Diamonds, Clubs, Spades]
        ]
```

**Penjelasan Kode:**
Membuat dek kartu standar 52 kartu.

**Mengapa ini disebut List Comprehension:**
*   Ini adalah notasi deklaratif untuk membangun himpunan (list).
*   Kode ini dibaca: "Buat list kartu `Card r s ...`, di mana `r` diambil dari Ace sampai King, DAN `s` diambil dari Hearts sampai Spades".
*   Ini setara dengan *nested loop* di bahasa imperatif, tapi diekspresikan sebagai definisi data.

### Contoh 2: `handToCount`
**Analisis:** Tidak ditemukan penggunaan list comprehension lain yang signifikan pada file yang dianalisis.

## 7. Lazy Evaluation

### Contoh 1: Evaluasi `gameRules`
**File:** `cabo-card-functional/src/GameEngine.hs` (Line 11-12)

```haskell
    gameRules ofGameState action
    applyAction ofGameState action
```

**Penjelasan Kode:**
Memvalidasi aturan sebelum menjalankan aksi.

**Mengapa ini disebut Lazy Evaluation (dalam konteks Monad):**
*   Haskell mengevaluasi ekspresi secara malas. Namun, dalam blok `do` untuk `Either` monad, ada mekanisme kontrol aliran.
*   Jika `gameRules` menghasilkan `Left "Error"`, eksekusi berhenti di situ. Ekspresi `applyAction` **tidak akan pernah dievaluasi**.
*   Meskipun ini lebih merupakan fitur *short-circuiting* monad, ini sangat bergantung pada semantik evaluasi non-strict Haskell yang memungkinkan definisi struktur kontrol seperti ini sebagai library, bukan *keyword* bahasa.

## 8. Currying

### Contoh 1: `logicDiscard`
**File:** `cabo-card-functional/src/GameEngine.hs` (Line 87)

```haskell
logicDiscard :: GameState -> Int -> Either String GameState
```

**Penjelasan Kode:**
Fungsi logika discard.

**Mengapa ini disebut Currying:**
*   Di Haskell, fungsi `logicDiscard` sebenarnya menerima **satu** argumen (`GameState`) dan mengembalikan **sebuah fungsi** yang menerima `Int` dan mengembalikan `Either ...`.
*   Tipe `GameState -> Int -> ...` adalah singkatan dari `GameState -> (Int -> ...)`.
*   Ini memungkinkan kita melakukan *partial application*, misal `discardCurrentState = logicDiscard currentState`. Variabel `discardCurrentState` sekarang adalah fungsi yang hanya butuh `Int` (index kartu) untuk jalan.

### Contoh 2: `isPlayerTurn`
**File:** `cabo-card-functional/src/GameRules.hs` (Line 23)

```haskell
isPlayerTurn :: Rule 
-- Tipe Rule = GameState -> GameAction -> Either String ()
```

**Penjelasan Kode:**
Fungsi validasi giliran.

**Mengapa ini disebut Currying:**
*   Fungsi ini dipassing ke operator `.&&.` (Line 86: `isPlayerTurn .&&. isPhaseCorrect`).
*   Operator `.&&.` (Line 81) menerima dua fungsi `Rule`. Ia memanggil `r1 ofGameState action`.
*   Kemampuan untuk memperlakukan fungsi `isPlayerTurn` sebagai nilai yang bisa dipassing, digabung, dan diaplikasikan argumennya satu per satu adalah inti dari Currying.

## 9. Functor

### Contoh 1: `fmap` pada `parseInput`
**File:** `cabo-card-functional/app/Main.hs` (Line 96)

```haskell
        ["discard", iStr]       -> fmap (DiscardAction pid) (readMaybe iStr)
```

**Penjelasan Kode:**
Mem-parsing input user untuk aksi discard.

**Mengapa ini disebut Functor:**
*   `readMaybe iStr` mengembalikan `Maybe Int`.
*   Konstruktor `DiscardAction pid` membutuhkan `Int` (bukan `Maybe Int`).
*   Kita menggunakan `fmap` (Functor map) untuk "mengangkat" fungsi konstruktor `DiscardAction` ke dalam konteks `Maybe`.
*   Jika `readMaybe` berhasil (`Just 5`), hasilnya `Just (DiscardAction pid 5)`. Jika gagal (`Nothing`), hasilnya `Nothing`. Kita memanipulasi nilai di dalam "kotak" Maybe tanpa perlu membukanya secara manual dengan `case`.

### Contoh 2: `TaggedAction` FromJSON instance
**File:** `cabo-card-functional/test/TestMain.hs` (Line 20-22)

```haskell
    parseJSON = A.withObject "TaggedAction" $ \v -> TaggedAction
        <$> v A..: "tag"
        <*> v A..:? "contents" A..!= A.Null
```

**Penjelasan Kode:**
Parsing JSON.

**Mengapa ini disebut Functor:**
*   Operator `<$>` adalah infix untuk `fmap`.
*   Ia menerapkan fungsi murni `TaggedAction` (konstruktor) ke hasil parser pertama `v .: "tag"`. Ini adalah langkah pertama dalam pola Applicative parsing.

## 10. Applicative

### Contoh 1: Parsing JSON `TaggedAction`
**File:** `cabo-card-functional/test/TestMain.hs` (Line 20-22)

```haskell
        <$> v A..: "tag"
        <*> v A..:? "contents" A..!= A.Null
```

**Penjelasan Kode:**
Lanjutan parsing JSON.

**Mengapa ini disebut Applicative:**
*   Operator `<*>` (apply) adalah ciri khas Applicative.
*   `<$>` menghasilkan parser yang berisi fungsi (partial constructor).
*   `<*>` mengambil parser fungsi tersebut dan parser kedua (`v .:? "contents"`), menjalankannya, lalu mengaplikasikan fungsi ke hasil parser kedua.
*   Ini memungkinkan kita merangkai pengambilan beberapa field JSON secara independen dan menggabungkannya menjadi satu objek `TaggedAction` di akhir.

### Contoh 2: `parseInput` untuk TargetAction
**File:** `cabo-card-functional/app/Main.hs` (Line 103)

```haskell
            case (readMaybe pidStr, mapM readMaybe idxStrs) of
```

**Penjelasan Kode:**
Mem-parsing list index target.

**Mengapa ini disebut Applicative:**
*   `mapM` pada struktur `List` (sebagai Traversable) menggunakan konsep Applicative.
*   `readMaybe` menghasilkan `Maybe Int`. `idxStrs` adalah `[String]`.
*   `mapM readMaybe idxStrs` mengubah `[String]` menjadi `Maybe [Int]`.
*   Transformasi dari "List of Maybe" menjadi "Maybe of List" (dimana jika salah satu elemen `Nothing`, seluruh hasil jadi `Nothing`) adalah operasi yang dimungkinkan oleh sifat Applicative dari `Maybe`.

## 11. Monad

### Contoh 1: `updateGame` (`Either` Monad)
**File:** `cabo-card-functional/src/GameEngine.hs` (Line 10-13)

```haskell
updateGame ofGameState action = do
    gameRules ofGameState action
    applyAction ofGameState action
```

**Penjelasan Kode:**
Mengupdate state game dengan penanganan error.

**Mengapa ini disebut Monad:**
*   Fungsi ini beroperasi dalam `Either String` Monad.
*   `do` notation menyembunyikan logika *branching* error.
*   Baris pertama `gameRules ...` dijalankan. Jika ia mengembalikan `Left "Error"`, seluruh fungsi `updateGame` berhenti dan mengembalikan error tersebut (short-circuit).
*   Baris kedua `applyAction ...` hanya dijalankan jika baris pertama mengembalikan `Right`. Ini adalah pola *Railway Oriented Programming* untuk *error handling*.

### Contoh 2: `shuffleDeck` (`IO` Monad)
**File:** `cabo-card-functional/src/Card.hs` (Line 106-116)

```haskell
shuffleDeck cards = do
    -- ...
    arr <- newListArray (0, len - 1) cards
    forM [0 .. len - 2] $ \i -> do
        j <- randomRIO (i, len - 1)
        -- ...
```

**Penjelasan Kode:**
Mengacak kartu.

**Mengapa ini disebut Monad:**
*   Fungsi ini membutuhkan efek samping (randomness `randomRIO` dan mutasi array `writeArray`), jadi harus berjalan dalam `IO` Monad.
*   `do` block mengurutkan aksi-aksi imperatif ini.
*   `j <- randomRIO ...` menunjukkan ketergantungan nilai: kita butuh hasil angka acak `j` sebelum bisa melakukan penukaran array di baris berikutnya. Monad menjamin urutan eksekusi ini.

## 12. Monoid

**Analisis:**
Tidak ditemukan implementasi eksplisit typeclass **Monoid** pada kode yang dianalisis. Penggunaan operator `++` (append) untuk list dan `sum` untuk skor adalah operasi monoid standar, namun tidak ada tipe data kustom yang mengimplementasikan instance `Monoid` atau `Semigroup`.

## 13. QuickCheck

**Analisis:**
Tidak ditemukan implementasi **QuickCheck** pada proyek ini. File `test/TestMain.hs` berisi server web menggunakan Scotty untuk keperluan testing UI manual, bukan *automated property-based testing*.