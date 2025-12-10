# Analisis Konsep Functional Programming pada Proyek ecommerce-pemfung

Berikut adalah analisis penerapan konsep-konsep Functional Programming pada codebase `ecommerce-pemfung`. Proyek ini menunjukkan penerapan FP yang sangat kuat dan idiomatik dalam lingkungan TypeScript.

## 1. Algebraic Data Type (ADT)

### Contoh 1: `type OrderStatus`
**File:** `ecommerce-pemfung/lib/domain/types.ts` (Line 23-29)

```typescript
export type OrderStatus =
  | { status: 'pending' }
  | { status: 'paid'; paidAt?: string }
  | { status: 'shipped'; shippedAt?: string; tracking?: string }
  | { status: 'delivered'; deliveredAt?: string }
  | { status: 'cancelled'; reason?: string }
  | { status: 'refunded'; refundedAt?: string; reason?: string };
```

**Penjelasan Singkat:**
Tipe `OrderStatus` mendefinisikan status pesanan sebagai *Discriminated Union*.

**Mengapa ini disebut Algebraic Data Type?**
1.  **Sum Type:** `OrderStatus` adalah gabungan (union) dari beberapa bentuk objek yang berbeda. Sebuah status hanya bisa berupa `pending` **ATAU** `paid` **ATAU** `shipped`, dst.
2.  **Product Type:** Setiap varian (misal `{ status: 'shipped'; ... }`) adalah *product type* (record) yang menggabungkan beberapa properti. Properti `tracking` hanya ada jika statusnya `shipped`. Ini membuat *invalid state* (seperti status `pending` tapi punya `tracking number`) mustahil direpresentasikan oleh sistem tipe (*make illegal states unrepresentable*).

### Contoh 2: `type Validation<E, A>`
**File:** `ecommerce-pemfung/lib/fp/validation.ts` (Line 4-6)

```typescript
export type Success<A> = { _tag: 'Success'; value: A };
export type Failure<E> = { _tag: 'Failure'; errors: E[] };

export type Validation<E, A> = Success<A> | Failure<E>;
```

**Penjelasan Singkat:**
Tipe `Validation` merepresentasikan hasil validasi yang bisa sukses atau gagal.

**Mengapa ini disebut Algebraic Data Type?**
Ini adalah implementasi manual dari tipe `Either` atau `Result` (Sum Type). Nilai `Validation` pasti berupa `Success` (membawa nilai `A`) atau `Failure` (membawa list error `E[]`). Properti `_tag` berfungsi sebagai *discriminator* untuk membedakan kedua varian tersebut saat runtime (mirip konstruktor di Haskell).

## 2. Pattern Matching

### Contoh 1: Transisi Status Order (`transitionOrder`)
**File:** `ecommerce-pemfung/lib/order/stateMachine.ts` (Line 8-46)

```typescript
export const transitionOrder = (
  currentStatus: OrderStatus,
  event: OrderEvent
): OrderStatus | null => {
  return match([currentStatus, event])
    .with(
      [{ status: 'pending' }, { type: 'ConfirmPayment' }],
      () => ({ status: 'paid' as const, paidAt: new Date().toISOString() })
    )
    .with(
      [{ status: 'pending' }, { type: 'Cancel' }],
      ([, e]) => ({ status: 'cancelled' as const, reason: e.reason })
    )
    // ... (case lain)
    .otherwise(() => null);
};
```

**Penjelasan Singkat:**
Fungsi ini menggunakan library `ts-pattern` untuk menentukan status pesanan berikutnya berdasarkan status saat ini dan *event* yang terjadi.

**Mengapa ini disebut Pattern Matching?**
1.  **Struktural:** Fungsi `match` memeriksa struktur data gabungan `[currentStatus, event]`.
2.  **Destructuring:** Pada pola `[{ status: 'pending' }, { type: 'Cancel' }]`, kode tidak hanya mencocokkan tipe event `Cancel`, tetapi juga secara implisit mengekstrak event `e` di dalam callback `([, e]) => ...` untuk mengakses `e.reason`.
3.  **Ekspresif:** Ini menggantikan logika `if-else` atau `switch-case` bersarang yang rumit dengan deklarasi pola (status + event) -> hasil.

### Contoh 2: Serialisasi Hasil Pipeline Analytics
**File:** `ecommerce-pemfung/app/api/analytics/[sellerId]/route.ts` (Line 13-22)

```typescript
  return result.match(
    (data) => NextResponse.json(data),
    (error) => {
      const status = 
        error.code === 'INVALID_SELLER_ID' ? 400 :
        error.code === 'NO_ORDERS_FOUND' ? 404 :
        500;
      // ...
    }
  );
```

**Penjelasan Singkat:**
Metode `.match()` dari `ResultAsync` (library `neverthrow`) digunakan untuk menangani hasil sukses atau gagal dari pipeline analitik.

**Mengapa ini disebut Pattern Matching?**
Meskipun ini adalah method, konsepnya adalah pattern matching pada tipe ADT `Result` (Success vs Failure). Kita dipaksa memberikan dua fungsi handler: satu untuk kasus sukses (`data`), satu untuk kasus gagal (`error`). Ini menjamin penanganan error yang komprehensif (*exhaustive handling*).

## 3. Function Composition

### Contoh 1: Pipeline Analytics (`analyticsPipeline`)
**File:** `ecommerce-pemfung/lib/analytics/pipeline.ts` (Line 115-120)

```typescript
export const analyticsPipeline = (
  sellerId: string
): ResultAsync<SerializedAnalyticsResult, AnalyticsError> => {
  return validateSellerId(sellerId)
    .andThen(fetchOrdersForSeller)
    .andThen(calculateAnalytics)
    .andThen(serializeAnalytics);
};
```

**Penjelasan Singkat:**
Fungsi ini mendefinisikan alur pemrosesan analitik dengan merangkai empat fungsi kecil.

**Mengapa ini disebut Function Composition?**
1.  **Chaining:** Metode `.andThen()` (mirip dengan `bind` atau `flatMap` di Monad) menghubungkan output dari satu fungsi ke input fungsi berikutnya.
2.  **Pipeline:** Data mengalir: `sellerId` -> `validate` -> `fetch` -> `calculate` -> `serialize`. Setiap langkah hanya fokus pada tugasnya sendiri.
3.  **Point-free Style:** Perhatikan bahwa argumen untuk fungsi-fungsi di dalam chain tidak ditulis eksplisit (misal `.andThen(ctx => fetchOrdersForSeller(ctx))`), melainkan nama fungsinya langsung dipassing.

### Contoh 2: `applyFilters`
**File:** `ecommerce-pemfung/lib/fp/productFilters.ts` (Line 67)

```typescript
  return pipe(...filterFns)(products);
```

**Penjelasan Singkat:**
Fungsi `applyFilters` mengumpulkan array fungsi filter (`filterFns`) dan menjalankannya secara berurutan pada `products`.

**Mengapa ini disebut Function Composition?**
Fungsi `pipe` (dari `lib/fp/utils.ts` atau `fp-ts`) secara matematis mengkomposisikan fungsi-fungsi: `f(g(h(x)))` menjadi `pipe(h, g, f)(x)`. Ini memungkinkan kita membangun logika filter kompleks dari filter-filter sederhana (`byText`, `byCategory`, dll.) secara dinamis.

## 4. High Order Function

### Contoh 1: `filterBy`
**File:** `ecommerce-pemfung/lib/fp/productFilters.ts` (Line 49-50)

```typescript
const filterBy = (predicate: (p: ProductDoc) => boolean) =>
  (products: ProductDoc[]): ProductDoc[] => products.filter(predicate);
```

**Penjelasan Singkat:**
`filterBy` adalah fungsi yang membuat fungsi filter baru berdasarkan predikat yang diberikan.

**Mengapa ini disebut High Order Function?**
1.  **Menerima Fungsi:** Argumen `predicate` adalah sebuah fungsi yang mengembalikan boolean.
2.  **Mengembalikan Fungsi:** Output dari `filterBy` adalah sebuah fungsi baru `(products: ProductDoc[]) => ProductDoc[]`.
3.  Ini memungkinkan pembuatan fungsi spesifik seperti `const filterActive = filterBy(p => p.isActive)` secara deklaratif.

### Contoh 2: `foldMap`
**File:** `ecommerce-pemfung/lib/analytics/service.ts` (Line 132)

```typescript
export const calculateStatistics = (orders: OrderDocument[], sellerObjectId: mongoose.Types.ObjectId): SalesStatistics =>
  foldMap(monoidSalesStatistics)(orderToStats(sellerObjectId))(orders);
```

**Penjelasan Singkat:**
`foldMap` digunakan untuk mengubah list order menjadi statistik tunggal.

**Mengapa ini disebut High Order Function?**
`foldMap` (dari `fp-ts`) menerima:
1.  Sebuah struktur **Monoid** (objek dengan fungsi `concat` dan nilai `empty`).
2.  Sebuah **fungsi pemetaan** (`orderToStats(...)`) yang mengubah satu item menjadi tipe Monoid tersebut.
`foldMap` kemudian mengabstraksi proses iterasi, pemetaan, dan penggabungan (reduce) menjadi satu operasi tingkat tinggi.

## 5. Lambda Function

### Contoh 1: Lambda pada `calculateRevenueMetrics`
**File:** `ecommerce-pemfung/lib/analytics/specialized.ts` (Line 233)

```typescript
    A.map(([hour, data]) => ({
      hour,
      revenue: data.revenue,
      orderCount: data.count,
    })),
```

**Penjelasan Singkat:**
Fungsi panah (arrow function) `([hour, data]) => ({...})` digunakan di dalam `A.map`.

**Mengapa ini disebut Lambda Function?**
Ini adalah fungsi anonim yang didefinisikan secara *inline* (langsung di tempat penggunaan). Fungsi ini mentransformasi entri map menjadi objek `RevenueByHour`. Penggunaan lambda membuat kode transformasi data menjadi ringkas dan lokal tanpa perlu mendefinisikan fungsi bernama terpisah.

### Contoh 2: Lambda pada `validateAll`
**File:** `ecommerce-pemfung/lib/api/validation.ts` (Line 136)

```typescript
export const validateAll = <T>(
  ...predicates: Array<(data: T) => boolean>
): ((data: T) => boolean) =>
  (data: T) => predicates.every((predicate) => predicate(data));
```

**Penjelasan Singkat:**
Fungsi `validateAll` mengembalikan sebuah lambda `(data: T) => ...` yang menjalankan semua predikat.

**Mengapa ini disebut Lambda Function?**
Lambda ini bertindak sebagai *closure* yang menangkap variabel `predicates` dari lingkup luarnya. Ini memungkinkan kita membuat validator gabungan (komposit) yang "mengingat" aturan-aturan validasinya.

## 6. List Comprehension

**Analisis:**
TypeScript/JavaScript tidak memiliki sintaks List Comprehension asli seperti Haskell atau Python (`[x*2 for x in list]`). Namun, pola yang setara dicapai menggunakan method array `map`, `filter`, dan `reduce` yang dirangkai (chained).

Contoh ekuivalen di **File:** `ecommerce-pemfung/lib/analytics/specialized.ts` (Line 162-166):

```typescript
  const statusCounts = pipe(
    filtered,
    A.map(getOrderStatus),
    A.reduce(...) // ...
  );
```
Ini adalah cara fungsional standar di JS/TS untuk memproses list secara deklaratif.

## 7. Lazy Evaluation

### Contoh 1: `ResultAsync`
**File:** `ecommerce-pemfung/lib/analytics/pipeline.ts` (Line 115)

```typescript
export const analyticsPipeline = (sellerId: string) => { ... }
```

**Penjelasan Singkat:**
Pipeline ini mengembalikan objek `ResultAsync`.

**Mengapa ini disebut Lazy Evaluation (dalam konteks ini):**
Meskipun JS/TS strict, `ResultAsync` dari `neverthrow` (dan Promise pada umumnya) menunda penanganan hasil masa depan. Namun, contoh yang lebih kuat dari *lazy evaluation* murni ada pada penggunaan pustaka seperti `rxjs` (Observables).

### Contoh 2: RxJS Observables
**File:** `ecommerce-pemfung/lib/notifications/service.ts` (Line 35)

```typescript
export const sellerNotifications$ = (sellerId: UserId): Observable<SystemEvent> => {
  return eventBus$.pipe(
    filter((event): event is SystemEvent => { ... })
  );
};
```

**Penjelasan Singkat:**
`sellerNotifications$` mendefinisikan aliran data (stream) notifikasi.

**Mengapa ini disebut Lazy Evaluation?**
Observable adalah struktur data malas (*lazy push collection*). Definisi pipeline `pipe(filter(...))` tidak melakukan apa-apa dan tidak memakan resource CPU sampai ada yang melakukan `.subscribe()`. Aliran data dan transformasinya didefinisikan secara deklaratif di awal, tetapi eksekusinya ditunda.

## 8. Currying

### Contoh 1: `orderToStats`
**File:** `ecommerce-pemfung/lib/analytics/service.ts` (Line 15)

```typescript
export const orderToStats = (sellerObjectId: mongoose.Types.ObjectId) => (order: OrderDocument): SalesStatistics => { ... }
```

**Penjelasan Singkat:**
Fungsi `orderToStats` menerima `sellerObjectId`, lalu mengembalikan fungsi baru yang menerima `order`.

**Mengapa ini disebut Currying?**
1.  **Partial Application:** Kita bisa membuat fungsi spesifik `const statsForMyShop = orderToStats(mySellerId)`. Fungsi `statsForMyShop` sekarang hanya membutuhkan satu argumen (`order`).
2.  **Kompatibilitas Pipeline:** Fungsi ter-curry ini cocok sempurna dengan tanda tangan `foldMap` (Line 133) yang mengharapkan fungsi mapper dengan satu argumen. Tanpa currying, kita harus menulis wrapper `orders.map(o => orderToStats(id, o))`.

### Contoh 2: `byCategory`
**File:** `ecommerce-pemfung/lib/fp/productFilters.ts` (Line 22)

```typescript
export const byCategory = (category?: string) => (p: ProductDoc) => { ... };
```

**Penjelasan Singkat:**
Fungsi ini membuat predikat filter berdasarkan kategori.

**Mengapa ini disebut Currying?**
Teknik ini memisahkan konfigurasi filter (`category`) dari data yang difilter (`product`).
`byCategory('Electronics')` menghasilkan fungsi `(p: ProductDoc) => boolean`. Ini memudahkan komposisi filter dinamis.

## 9. Functor

### Contoh 1: `Validation` Functor
**File:** `ecommerce-pemfung/lib/fp/validation.ts` (Line 14)

```typescript
export const map = <E, A, B>(fa: Validation<E, A>, f: (a: A) => B): Validation<E, B> =>
  isSuccess(fa) ? success(f(fa.value)) : fa;
```

**Penjelasan Singkat:**
Fungsi `map` memungkinkan kita mengubah nilai sukses di dalam `Validation` tanpa menyentuh status error-nya.

**Mengapa ini disebut Functor?**
Tipe data `Validation` (bersama fungsi `map`) memenuhi sifat Functor. Ia adalah "kontainer" yang isinya bisa dipetakan (`f: A -> B`) menjadi (`Validation<E, B>`). Jika `Validation` berisi Error, fungsi `f` tidak dijalankan, dan Error diteruskan (preserve structure).

### Contoh 2: `ResultAsync.map`
**File:** `ecommerce-pemfung/lib/analytics/pipeline.ts` (Line 132)

```typescript
    .map((context) => calculateOverviewAnalytics(context.orders, context.sellerObjectId))
```

**Penjelasan Singkat:**
Metode `.map` digunakan untuk mengubah data konteks menjadi hasil analitik.

**Mengapa ini disebut Functor?**
`ResultAsync` adalah Functor. `.map` menerapkan transformasi pada nilai *future* yang sukses. Ini memungkinkan kita memanipulasi data di dalam "kotak" asinkronus yang aman dari error tanpa perlu membuka kotaknya (`await` + `try/catch`) secara manual.

## 10. Applicative

### Contoh 1: Validasi Kartu (`validateCart`)
**File:** `ecommerce-pemfung/lib/cart/validation.ts` (Line 75)

```typescript
  const validations: Validation<CartValidationError, CartInput>[] = [
    validateQuantities(cart),
    validateCartSize(cart, context),
    // ...
  ];

  return map(sequence(validations), () => cart);
```

**Penjelasan Singkat:**
Fungsi ini menjalankan serangkaian validasi independen dan mengumpulkan hasilnya.

**Mengapa ini disebut Applicative?**
1.  **Independensi:** `validateQuantities` dan `validateCartSize` tidak saling bergantung. Mereka bisa dijalankan paralel/independen.
2.  **Akumulasi Error:** Fungsi `sequence` (yang dibangun di atas `ap` atau Apply) menggabungkan list `Validation[]` menjadi satu `Validation`. Berbeda dengan Monad yang *fail-fast* (berhenti di error pertama), Applicative style di sini memungkinkan kita mengumpulkan **semua** error dari semua validator yang gagal sekaligus (misal: "Quantity invalid" DAN "Product not found"). Ini memberikan UX yang lebih baik.

## 11. Monad

### Contoh 1: Railway Oriented Programming (`andThen`)
**File:** `ecommerce-pemfung/lib/analytics/pipeline.ts` (Line 115)

```typescript
  return validateSellerId(sellerId)
    .andThen(fetchOrdersForSeller)
    .andThen(calculateAnalytics)
    .andThen(serializeAnalytics);
```

**Penjelasan Singkat:**
Rangkaian operasi di mana langkah selanjutnya bergantung pada kesuksesan langkah sebelumnya.

**Mengapa ini disebut Monad?**
1.  **Sekuensial:** `andThen` adalah nama lain untuk `bind` atau `flatMap`.
2.  **Short-Circuiting:** Jika `validateSellerId` gagal (mengembalikan `Err`), `fetchOrdersForSeller` **tidak pernah dijalankan**. Error dari langkah pertama langsung "melompat" ke ujung pipeline.
3.  **FlatMap:** `fetchOrdersForSeller` mengembalikan `ResultAsync` baru. `andThen` bertugas "meratakan" (*flatten*) struktur `ResultAsync<ResultAsync<...>>` menjadi satu `ResultAsync`. Ini adalah esensi dari Monad.

### Contoh 2: `validation.ts` (Implisit)
Meskipun `validation.ts` lebih menonjolkan Applicative (`sequence`), struktur `Success` dan `Failure` serta kemampuan untuk chain operasi secara sekuensial (jika diimplementasikan `bind`/`chain`) menjadikannya Monad.

## 12. Monoid

### Contoh 1: `monoidSalesStatistics`
**File:** `ecommerce-pemfung/lib/analytics/monoid.ts` (Line 100-125)

```typescript
export const monoidSalesStatistics: Monoid<SalesStatistics> = {
  concat: (x, y) => ({
    totalSales: x.totalSales + y.totalSales,
    orderCount: x.orderCount + y.orderCount,
    // ... merging logic lainnya
  }),
  empty: {
    totalSales: 0,
    orderCount: 0,
    // ... nilai nol lainnya
  },
};
```

**Penjelasan Singkat:**
Objek ini mendefinisikan cara menggabungkan dua `SalesStatistics` menjadi satu, dan mendefinisikan nilai "kosong"-nya.

**Mengapa ini disebut Monoid?**
1.  **Binary Operation (`concat`):** Mengambil dua statistik dan menghasilkan satu statistik gabungan (asosiatif).
2.  **Identity Element (`empty`):** Statistik kosong yang jika digabung dengan statistik X, hasilnya tetap X.
3.  **Kegunaan:** Monoid ini memungkinkan kita menggunakan fungsi `foldMap` (di `service.ts`) untuk meringkas ribuan order menjadi satu laporan statistik dengan sangat efisien dan paralelisable. Kita hanya perlu mendefinisikan "cara menambah 1 order", dan Monoid menangani "cara menjumlahkan semuanya".

## 13. Event Sourcing

### Contoh 1: Perhitungan Saldo (`sumBalanceEvents`)
**File:** `ecommerce-pemfung/lib/balance/operations.ts` (Line 67)

```typescript
export const sumBalanceEvents = (events: BalanceEventRecord[]): number =>
  events.reduce((total, event) => total + calculateEventValue(event), 0);
```

**Penjelasan Singkat:**
Saldo pengguna tidak disimpan sebagai angka di database, melainkan dihitung ulang dari riwayat transaksi (`events`).

**Mengapa ini disebut Event Sourcing (FP Concept):**
*   **Immutability:** Data transaksi (`events`) bersifat *append-only* dan tidak pernah diubah (*immutable*).
*   **State as Function of History:** State saat ini (saldo) adalah hasil dari fungsi murni (`reduce`) yang diterapkan pada seluruh riwayat event. `CurrentState = f(Events)`.
*   Ini sangat "fungsional" karena menghindari *mutable state* global (saldo di tabel user) yang rentan *race condition*.
