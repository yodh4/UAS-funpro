# Analisis Konsep Functional Programming pada Proyek functional-rpg

Berikut adalah analisis penerapan konsep-konsep Functional Programming pada codebase `functional-rpg`.

## 1. Algebraic Data Type (ADT)

### Contoh 1: `data Combat a`
**File:** `functional-rpg/app/Models.hs` (Line 328-332)

```haskell
data Combat a 
  = Player a
  | Enemy a
  | Corpse a
  | Combat (Combat a) [Combat a]
  deriving (Show, Eq)
```

**Penjelasan Singkat:**
Tipe data rekursif yang merepresentasikan entitas dalam pertarungan.

**Mengapa ini disebut Algebraic Data Type?**
Ini adalah bentuk **Sum Type** yang sangat fleksibel.
*   Konstruktor `Player`, `Enemy`, dan `Corpse` adalah varian yang membungkus nilai generik `a` (biasanya `Stat`).
*   Konstruktor `Combat` bersifat rekursif dan komposit, memegang satu `Combat a` (pemain) dan list `[Combat a]` (musuh-musuh).
*   Struktur ini memungkinkan representasi pohon dari state pertarungan, di mana status pertarungan itu sendiri (`Combat`) bisa berisi entitas-entitas (`Player`, `Enemy`) yang juga bertipe `Combat`.

### Contoh 2: `data Event`
**File:** `functional-rpg/app/Models.hs` (Line 360-363)

```haskell
data Event
  = Dialog String String 
  | Choice String String String Scenario String Scenario
  | CombatEvent (Combat Stat) Scenario Scenario
  deriving (Show, Eq)
```

**Penjelasan Singkat:**
Merepresentasikan jenis-jenis event dalam skenario game.

**Mengapa ini disebut Algebraic Data Type?**
*   **Sum Type:** Event bisa berupa `Dialog` **ATAU** `Choice` **ATAU** `CombatEvent`.
*   **Product Type:** Setiap konstruktor memuat data yang berbeda. `Dialog` memuat dua `String`. `CombatEvent` memuat state pertarungan awal dan dua skenario lanjutan (menang/kalah). Ini memodelkan logika percabangan cerita game secara data.

## 2. Pattern Matching

### Contoh 1: `updateTurnOrder` (Versi `cycleTillMatch`)
**File:** `functional-rpg/app/Models.hs` (Line 275-277)

```haskell
cycleTillMatch :: (Combat Stat) -> [Combat Stat] -> [Combat Stat]
cycleTillMatch target (x:xs)
  | nameMatches target x = x:xs
  | otherwise = cycleTillMatch target (xs ++ [x])
```

**Penjelasan Singkat:**
Merotasi list giliran sampai entitas target berada di depan (head).

**Mengapa ini disebut Pattern Matching?**
1.  **De-strukturisasi List:** Pola `(x:xs)` memisahkan kepala dan ekor list.
2.  **Guards:** Penggunaan `|` untuk mengecek kondisi `nameMatches target x`. Jika cocok, kembalikan list saat ini. Jika tidak, panggil rekursi dengan list yang sudah dirotasi. Pola ini sangat umum untuk manipulasi list di Haskell.

### Contoh 2: `updateModel`
**File:** `functional-rpg/app/Models.hs` (Line 72-76)

```haskell
updateModel :: Action -> Effect ROOT Model Action
updateModel action = do
  model <- get
  case action of
    Next        -> ...
    Choose selection -> ...
    InitiateCombat -> ...
```

**Penjelasan Singkat:**
Fungsi utama untuk update state aplikasi berdasarkan aksi.

**Mengapa ini disebut Pattern Matching?**
Struktur `case action of` memilah logika berdasarkan konstruktor `Action`. Pola `Choose selection` tidak hanya mencocokkan aksi `Choose`, tetapi juga mengekstrak nilai string `selection` yang dibawa oleh aksi tersebut. Ini adalah inti dari arsitektur ELM/Miso.

## 3. Function Composition

### Contoh 1: `viewModel`
**File:** `functional-rpg/app/Views.hs` (Line 32-39)

*Catatan: Meskipun tidak menggunakan operator `.` secara eksplisit, struktur deklaratif HTML DSL Miso sangat kental dengan komposisi.*

Namun, contoh eksplisit `.` ada di `Models.hs`:

**File:** `functional-rpg/app/Models.hs` (Line 275)

(Kode tidak eksplisit menggunakan `.`, tapi konsepnya ada di *getter* lenses).

Mari kita lihat contoh definisi fungsi komposisi manual yang mungkin digunakan atau pola serupa.
Di `Main.hs` (Line 22):
`run (startApp (component initialModel updateModel viewModel))`
Ini adalah komposisi fungsi `run`, `startApp`, dan `component`. `run . startApp . component ...`

Contoh yang lebih jelas di `Models.hs` (Line 307):
`getMultiplier target statuses = foldl checkMultiplier 1.0 statuses`
Ini bukan komposisi fungsi dengan `.`, tapi aplikasi parsial dan HOF.

Codebase ini cenderung menggunakan gaya aplikasi fungsi langsung (`f x y`) atau operator pipe/bind (`>>=`, `&`) daripada komposisi titik (`.`).

## 4. High Order Function

### Contoh 1: `foldl` pada `getMultiplier`
**File:** `functional-rpg/app/Models.hs` (Line 307)

```haskell
getMultiplier target statuses=
  foldl checkMultiplier 1.0 statuses
```

**Penjelasan Singkat:**
Menghitung multiplier damage dari list status.

**Mengapa ini disebut High Order Function (HOF)?**
`foldl` adalah HOF standar. Ia menerima fungsi `checkMultiplier` sebagai argumen untuk mengakumulasi nilai. `checkMultiplier` sendiri mendefinisikan logika penggabungan (reduksi) list status menjadi satu nilai `Double`.

### Contoh 2: `component`
**File:** `functional-rpg/app/Main.hs` (Line 22)

```haskell
run (startApp (component initialModel updateModel viewModel))
```

**Penjelasan Singkat:**
Membangun aplikasi Miso.

**Mengapa ini disebut High Order Function (HOF)?**
Fungsi `component` (dari library Miso) menerima fungsi lain sebagai argumen, yaitu `updateModel` (logika update) dan `viewModel` (logika render). Ia merangkai fungsi-fungsi ini menjadi satu unit aplikasi yang kohesif.

## 5. Lambda Function

### Contoh 1: Definisi Lensa (Lens)
**File:** `functional-rpg/app/Models.hs` (Line 25)

```haskell
event = lens _event $ \record field -> record { _event = field }
```

**Penjelasan Singkat:**
Membuat lensa untuk field `_event`.

**Mengapa ini disebut Lambda Function?**
Ekspresi `\record field -> record { _event = field }` adalah fungsi anonim (lambda). Simbol `\` menandai awal lambda. Fungsi ini mendefinisikan cara *setter* bekerja: menerima record lama dan nilai field baru, lalu mengembalikan record baru yang sudah diupdate. Fungsi ini dipassing sebagai argumen ke fungsi `lens`.

### Contoh 2: `any` (Implisit di `checkCombat`)
Meskipun tidak ditulis eksplisit dengan `\x -> ...` di snippet yang diberikan untuk logika combat, pola umum penggunaan fungsi seperti `filter` atau `map` di Haskell sering melibatkan lambda jika fungsinya sederhana.

## 6. List Comprehension

**Analisis:**
Tidak ditemukan penggunaan sintaks list comprehension `[ x | x <- list ]` yang signifikan pada file `Models.hs` atau `Views.hs`. Kode lebih banyak menggunakan fungsi rekursif manual (seperti `buildEnemyBox`, `buildSkills`, `cycleTillMatch`) dan HOF (`foldl`, `map` via `fmap` Functor) untuk memproses list.

## 7. Lazy Evaluation

### Contoh 1: `cycleList` (Infinite List)
**File:** `functional-rpg/app/Models.hs` (Line 263-264)

```haskell
cycleList :: [a] -> [a]
cycleList (x:xs) = xs ++ [x]
```
*Koreksi:* Fungsi `cycleList` di atas sebenarnya bukan lazy infinite list (hanya memindah head ke tail). Tapi Haskell secara default lazy.

Contoh yang lebih tepat mungkin adalah penggunaan `random` generator yang bisa menghasilkan stream angka acak tak terbatas, meskipun di sini digunakan secara imperatif langkah demi langkah.

Namun, sifat lazy Haskell memungkinkan definisi struktur data rekursif seperti `MoveList` (Line 284):
```haskell
data MoveList
  = MoveElement String Move
  | MoveList [MoveList]
```
Kita bisa mendefinisikan struktur `MoveList` yang sangat dalam atau bahkan sirkular tanpa masalah, selama kita hanya mengakses bagian yang diperlukan saja.

## 8. Currying

### Contoh 1: `getMultiplier`
**File:** `functional-rpg/app/Models.hs` (Line 306)

```haskell
getMultiplier :: String -> [StatusEffect] -> Double
```

**Penjelasan Singkat:**
Fungsi penghitung multiplier.

**Mengapa ini disebut Currying?**
Di Haskell, semua fungsi secara default ter-curry. `getMultiplier` sebenarnya adalah fungsi yang menerima `String` dan mengembalikan fungsi `[StatusEffect] -> Double`.
Ini memungkinkan **Partial Application**. Misalnya kita bisa membuat fungsi:
`getFireMultiplier = getMultiplier "Fire"`
Fungsi baru ini hanya membutuhkan list status effect untuk bekerja.

### Contoh 2: Konstruktor Data `Stat`
**File:** `functional-rpg/app/Models.hs` (Line 324)

```haskell
data Stat
  = Stat String Double [StatusEffect] [String] String
```

**Penjelasan Singkat:**
Konstruktor untuk tipe `Stat`.

**Mengapa ini disebut Currying?**
Konstruktor `Stat` bertipe `String -> Double -> [StatusEffect] -> [String] -> String -> Stat`. Kita bisa mengaplikasikannya sebagian, misal `Stat "Player" 100.0` akan menghasilkan fungsi yang menunggu sisa argumennya. Ini sangat berguna saat menggunakan fungsi seperti `map` atau `applicative`.

## 9. Functor

### Contoh 1: Instance Functor `Combat`
**File:** `functional-rpg/app/Models.hs` (Line 335-339)

```haskell
instance Functor Combat where
  fmap f (Player a) = Player (f a)
  fmap f (Enemy a) = Enemy (f a)
  fmap f (Combat player enemies) 
    = Combat (fmap f player) (map (fmap f) enemies)
```

**Penjelasan Singkat:**
Implementasi `fmap` untuk tipe `Combat`.

**Mengapa ini disebut Functor?**
Typeclass Functor memungkinkan kita menerapkan fungsi `f` ke nilai yang ada *di dalam* struktur `Combat`, tanpa mengubah strukturnya.
*   Jika `Player a`, ubah jadi `Player (f a)`.
*   Jika `Combat`, terapkan `f` secara rekursif ke player dan list enemies di dalamnya.
*   Ini memungkinkan transformasi data massal (misal: kurangi HP semua entitas sebesar 10) dengan satu baris kode: `fmap (reduceHP 10) combatState`.

### Contoh 2: Operator `<#` (Miso Effect)
**File:** `functional-rpg/app/Models.hs` (Line 137)

```haskell
newModel <# scheduleEvent (ProcessTurn)
```

**Penjelasan Singkat:**
Mengupdate model dan menjadwalkan efek samping.

**Mengapa ini disebut Functor (variant)?**
Operator `<#` adalah operator spesifik Miso yang bekerja mirip dengan fmap namun dalam konteks Effect. Ia "menyuntikkan" efek ke dalam tuple hasil update model.

## 10. Applicative

### Contoh 1: Instance Applicative `Combat`
**File:** `functional-rpg/app/Models.hs` (Line 341-346)

```haskell
instance Applicative Combat where
  pure x = Combat (Player x) []
  (Player f) <*> (Player a) = Player (f a)
  -- ... (pola lain)
  (Combat p1 fs) <*> (Combat p2 as) = 
    Combat (p1 <*> p2) [x <*> y | x <- fs, y <- as]
```

**Penjelasan Singkat:**
Implementasi `pure` dan `<*>` untuk `Combat`.

**Mengapa ini disebut Applicative?**
*   `pure`: Membungkus nilai mentah menjadi konteks `Combat` minimal.
*   `<*>` (apply): Menerapkan fungsi yang ada di dalam struktur `Combat` ke nilai yang ada di dalam struktur `Combat` lain.
*   Implementasi `[x <*> y | x <- fs, y <- as]` menunjukkan kombinasi Cartesian (setiap fungsi diterapkan ke setiap nilai), yang merupakan perilaku standar list applicative. Ini memungkinkan kombinasi struktur data kompleks.

## 11. Monad

### Contoh 1: Instance Monad `Combat`
**File:** `functional-rpg/app/Models.hs` (Line 348-353)

```haskell
instance Monad Combat where
  Player a >>= f = f a
  Enemy a >>= f = f a
  Corpse a >>= f = f a
  Combat player enemies >>= f = 
    Combat (player >>= f) (fmap (>>= f) enemies)
```

**Penjelasan Singkat:**
Implementasi `bind` (`>>=`) untuk `Combat`.

**Mengapa ini disebut Monad?**
*   Operator `>>=` memungkinkan chaining operasi di mana hasil operasi sebelumnya menentukan struktur data berikutnya.
*   Dalam konteks game ini, ini digunakan untuk **transformasi state**.
*   Fungsi `f` bertipe `a -> Combat b`. Artinya, fungsi ini bisa mengubah `Player` menjadi `Corpse` (jika HP <= 0).
*   Tanpa Monad, kita hanya bisa mengubah nilai di dalamnya (seperti Functor), tapi tidak bisa mengubah *kontainer*-nya (misal dari `Player` hidup menjadi `Corpse` mati).

### Contoh 2: Penggunaan Monad `Combat` di `updateModel`
**File:** `functional-rpg/app/Models.hs` (Line 117)

```haskell
        newCombatStatus = currentStatus >>= (attack targetName selectedMove)
```

**Penjelasan Singkat:**
Mengaplikasikan serangan ke state combat.

**Mengapa ini disebut Monad?**
*   `currentStatus` adalah `Combat Stat`.
*   `attack` adalah fungsi yang mengembalikan `Combat Stat` (bisa mengembalikan `Corpse` jika mati).
*   Operator `>>=` membongkar `currentStatus`, memberikan isinya ke fungsi `attack`.
*   Jika `attack` memutuskan entitas mati, ia mengembalikan konstruktor `Corpse`. Struktur `Combat` yang baru ini kemudian dibentuk ulang. Ini adalah contoh penggunaan Monad yang sangat elegan untuk manajemen state game.

## 12. Monoid

**Analisis:**
Tidak ditemukan implementasi eksplisit `instance Monoid` pada codebase ini. Penggabungan string menggunakan `++` (Line 193: `msg = name ++ " attacked player..."`) adalah penggunaan Monoid untuk List/String secara standar, tapi tidak ada tipe data kustom yang mengimplementasikan `mempty` dan `mappend`.

## 13. QuickCheck

**Analisis:**
Tidak terdapat implementasi **QuickCheck** pada codebase ini. `Models.hs` menggunakan `System.Random` (Line 175) untuk pengacakan manual, bukan untuk property-based testing.

```