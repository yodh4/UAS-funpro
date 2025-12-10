# Analisis Konsep Functional Programming pada Fitur Sharing & Autentikasi Proyek Catatan Backend

Berikut adalah analisis mendalam mengenai penerapan konsep-konsep Functional Programming pada modul-modul yang terkait dengan fitur *sharing notes* dan autentikasi dalam proyek `catatan_backend`.

## 1. Algebraic Data Type (ADT)

### Contoh 1: Tipe Data Hasil (Tuple `{:ok, ...} | {:error, ...}`)
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/shares.ex` (Line 18)

```elixir
  @spec create_or_get_share(map()) :: {:ok, map(), :created | :ok} | {:error, term()}
```

**Penjelasan Singkat:**
Spesifikasi tipe ini mendefinisikan kemungkinan hasil dari fungsi `create_or_get_share`.

**Mengapa ini disebut Algebraic Data Type (ADT)?**
Ini adalah bentuk **Sum Type** yang sangat umum di Elixir (sering disebut sebagai "Result Type" atau "Tagged Union" di bahasa lain).
1.  **Varian Sukses (`{:ok, map(), :created | :ok}`):** Ini adalah *Product Type* (tuple) yang menggabungkan status sukses (`:ok`), data hasil (`map()`), dan status tambahan (`:created` atau `:ok`).
2.  **Varian Gagal (`{:error, term()}`):** Ini adalah *Product Type* lain yang menggabungkan status error (`:error`) dengan alasan error (`term()`).
3.  Tipe gabungan `Result` adalah penjumlahan (Sum) dari kedua varian ini. Sebuah fungsi hanya bisa mengembalikan salah satu dari keduanya, memaksa pemanggil untuk menangani kedua kasus (biasanya dengan pattern matching).

### Contoh 2: Tipe Data `user_profile`
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/actor/writer.ex` (Line 7-11)

```elixir
  @type user_profile :: %{
          required(:id) => String.t(),
          required(:name) => String.t(),
          required(:permission) => permission()
        }
```

**Penjelasan Singkat:**
Mendefinisikan bentuk data profil pengguna.

**Mengapa ini disebut Algebraic Data Type (ADT)?**
Ini adalah definisi **Product Type** menggunakan map. Sebuah `user_profile` yang valid dibentuk dari "perkalian" (kombinasi) tipe `id` (String), `name` (String), dan `permission`. Struktur ini menjamin bahwa setiap profil pengguna memiliki atribut-atribut tersebut dengan tipe yang benar.

## 2. Pattern Matching

### Contoh 1: Ekstraksi Header Token JWT
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/open_auth_client.ex` (Line 58-60)

```elixir
  defp peek_token_header(token) do
    case String.split(token, ".") do
      [header_b64, _payload_b64, _signature_b64] ->
        # ... logika dekode header ...
      _ ->
        {:error, :invalid_token_format}
    end
  end
```

**Penjelasan Singkat:**
Fungsi ini memecah string JWT berdasarkan karakter titik (`.`) untuk mengambil bagian headernya.

**Mengapa ini disebut Pattern Matching?**
1.  **Destructuring List:** Pola `[header_b64, _payload_b64, _signature_b64]` mencocokkan list yang memiliki tepat 3 elemen. Ini sekaligus memvalidasi format dasar JWT (harus terdiri dari 3 bagian) dan mengekstrak variabel `header_b64` untuk digunakan.
2.  **Wildcard:** Variabel yang diawali garis bawah (`_payload_b64`) menandakan nilai yang dicocokkan tetapi tidak digunakan (diabaikan).
3.  **Catch-all:** Pola `_ ->` menangani kasus di mana hasil `String.split` tidak berupa list 3 elemen (token tidak valid), mencegah runtime error.

### Contoh 2: Otorisasi Akses Share
**File:** `catatan/packages/catatan_backend/lib/catatan_backend_web/controllers/share_notes.ex` (Line 137-147)

```elixir
  defp authorized_to_view?(share_metadata, current_user) do
    access_type = Map.get(share_metadata, "access_type")

    case access_type do
      "public" ->
        true

      "private" ->
        # ... logika cek email ...

      _ ->
        false
    end
  end
```

**Penjelasan Singkat:**
Menentukan apakah user boleh melihat catatan berdasarkan tipe akses share link.

**Mengapa ini disebut Pattern Matching?**
Logika kontrol alur ("public" vs "private") ditangani dengan mencocokkan nilai variabel `access_type`. Alih-alih serangkaian `if access_type == "public" ... else if ...`, kode menggunakan struktur `case` yang lebih deklaratif. Pola `_` menangani tipe akses yang tidak dikenal secara aman (default deny).

## 3. Function Composition

### Contoh 1: Pipeline Autentikasi `verify_token`
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/open_auth_client.ex` (Line 36-40)

```elixir
  def verify_token(token) do
    with {:ok, jwks} <- fetch_jwks(),
         {:ok, header} <- peek_token_header(token),
         {:ok, key} <- find_key(jwks, header),
         {:ok, claims} <- verify_with_key(token, key) do
      {:ok, claims}
    # ...
```

**Penjelasan Singkat:**
Fungsi ini memverifikasi token JWT melalui serangkaian langkah: ambil JWKS -> baca header -> cari kunci publik -> verifikasi signature.

**Mengapa ini disebut Function Composition (Monadic)?**
Meskipun tidak menggunakan operator pipe (`|>`), blok `with` di sini adalah bentuk komposisi fungsi untuk operasi yang bisa gagal (mirip **Monad**).
1.  Setiap langkah (`fetch_jwks`, `peek_token_header`, dst.) adalah fungsi independen.
2.  Output sukses dari langkah sebelumnya (misal `jwks`) menjadi input untuk langkah berikutnya (`find_key`).
3.  Jika ada satu langkah gagal (mengembalikan `{:error, ...}`), seluruh rantai berhenti (short-circuit). Ini mengkomposisikan logika "jalur sukses" menjadi satu aliran linier.

### Contoh 2: Manipulasi List Email
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/shares/get.ex` (Line 72-75)

```elixir
          details =
            share_details
            |> Map.put_new("permission_level", "read")
            |> convert_allowed_emails_to_list()
```

**Penjelasan Singkat:**
Memproses data detail share dari database sebelum dikembalikan.

**Mengapa ini disebut Function Composition?**
Operator `|>` merangkai transformasi data. Data `share_details` mengalir melalui fungsi `Map.put_new` lalu hasilnya diteruskan ke `convert_allowed_emails_to_list`. Ini menciptakan pipeline transformasi data yang jelas dan mudah dibaca.

## 4. High Order Function

### Contoh 1: `Enum.find` pada `find_key`
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/open_auth_client.ex` (Line 84)

```elixir
        key =
          if kid do
            Enum.find(keys, fn k -> Map.get(k, "kid") == kid end)
          else
            List.first(keys)
          end
```

**Penjelasan Singkat:**
Mencari kunci (key) dalam list JWKS yang memiliki `kid` (Key ID) yang cocok.

**Mengapa ini disebut High Order Function (HOF)?**
`Enum.find` adalah HOF karena menerima fungsi lain (predikat anonim `fn k -> ... end`) sebagai argumen. Logika pencarian ("bagaimana cara menentukan item yang dicari?") diabstraksi ke dalam fungsi argumen tersebut, sementara `Enum.find` menangani mekanisme iterasi list-nya.

### Contoh 2: Transformasi Data dengan `Map.new`
**File:** `catatan/packages/catatan_backend/lib/catatan_backend_web/error.ex` (Line 11)

```elixir
  def format_ecto_error(errors) do
    errors
    |> Map.new(fn {field, {message, _opts}} -> {field, message} end)
  end
```

**Penjelasan Singkat:**
Mengubah list error dari Ecto changeset menjadi Map sederhana.

**Mengapa ini disebut High Order Function (HOF)?**
`Map.new` menerima fungsi transformasi `fn {field, ...} -> ... end`. HOF ini menerapkan fungsi tersebut ke setiap elemen koleksi input (`errors`) untuk membentuk pasangan key-value baru dalam map hasil.

## 5. Lambda Function

### Contoh 1: Predikat Pencarian Kunci
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/open_auth_client.ex` (Line 84)

```elixir
            Enum.find(keys, fn k -> Map.get(k, "kid") == kid end)
```

**Penjelasan Singkat:**
Fungsi anonim yang digunakan untuk mengecek apakah `kid` dari sebuah kunci cocok dengan `kid` yang dicari.

**Mengapa ini disebut Lambda Function?**
Ekspresi `fn k -> Map.get(k, "kid") == kid end` mendefinisikan sebuah fungsi tanpa nama (*anonymous function* atau lambda) secara *inline*. Fungsi ini menangkap variabel `kid` dari lingkup luarnya (closure) untuk melakukan perbandingan.

### Contoh 2: Transformasi Map
**File:** `catatan/packages/catatan_backend/lib/catatan_backend_web/error.ex` (Line 12)

```elixir
    |> Map.new(fn {field, {message, _opts}} -> {field, message} end)
```

**Penjelasan Singkat:**
Fungsi anonim untuk mengambil field dan pesan error.

**Mengapa ini disebut Lambda Function?**
Ini adalah definisi fungsi ad-hoc yang digunakan hanya dalam konteks transformasi map ini. Ia menggunakan pattern matching pada argumennya (`{field, {message, _opts}}`) untuk mengekstrak data yang relevan secara ringkas.

## 6. List Comprehension

**Analisis:**
Dalam file-file yang dianalisis untuk fitur sharing dan auth, **tidak ditemukan** penggunaan sintaks `for` (List Comprehension) secara eksplisit. Kode lebih banyak menggunakan fungsi-fungsi dari modul `Enum` (seperti `Enum.map`, `Enum.find`) yang secara fungsional setara dengan list comprehension tetapi menggunakan gaya HOF pipeline.

## 7. Lazy Evaluation

### Contoh 1: `with` statement (Evaluasi Bersyarat)
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/shares.ex` (Line 23-57)

```elixir
    with {:ok, share_data} <- Get.by_note_id(note_id) do
      # ... logika update ...
    else
      {:error, :not_found} ->
        # ... logika create baru ...
    end
```

**Penjelasan Singkat:**
Logika untuk membuat atau mengambil share link.

**Mengapa ini disebut Lazy Evaluation (dalam konteks kontrol alur)?**
Meskipun bukan *lazy evaluation* dalam arti struktur data tak terbatas, blok `with` dan `case` menerapkan evaluasi malas pada jalur eksekusi.
Jika `Get.by_note_id(note_id)` mengembalikan error, maka blok kode utama (`do ... end`) **tidak pernah dievaluasi**. Logika pembuatan share baru (`Create.insert_share_batch`) di dalam blok `else` hanya dieksekusi jika kondisi pertama gagal. Ini menunda (atau membatalkan) komputasi yang tidak perlu/tidak valid.

## 8. Currying

**Analisis:**
Elixir tidak mendukung *auto-currying* seperti Haskell. Namun, konsep **Partial Application** (yang mirip) diterapkan.

### Contoh: `update_share_batch` (Konseptual)
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/shares/create.ex` (Line 66)

```elixir
  def update_share_batch(share_id, note_id, access_type, permission_level, allowed_emails)
```

Fungsi ini menerima 5 argumen. Dalam paradigma FP murni dengan currying, ini akan menjadi fungsi yang menerima 1 argumen dan mengembalikan fungsi lain. Di Elixir, kita memanggilnya dengan semua argumen sekaligus.

Namun, penggunaan `&` (capture operator) bisa dianggap sebagai *manual partial application*.
Contoh (hipotetis penggunaan):
`Enum.map(shares, &update_share_batch(&1.id, ...))`
Meskipun tidak eksplisit di kode sumber yang diberikan, desain fungsi-fungsi kecil yang independen (seperti di modul `Create` dan `Get`) mendukung komposisi dan penggunaan parsial jika diperlukan oleh HOF.

## 9. Functor

**Analisis:**
Elixir tidak memiliki typeclass Functor formal. Namun, pola Functor terlihat pada transformasi data dalam struktur "kontainer" seperti Map atau List.

### Contoh: `convert_allowed_emails_to_list`
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/shares/get.ex` (Line 106-109)

```elixir
  defp convert_allowed_emails_to_list(map) do
    case Map.get(map, "allowed_emails") do
      %MapSet{} = set -> Map.put(map, "allowed_emails", MapSet.to_list(set))
      _ -> map
    end
  end
```

**Penjelasan Singkat:**
Mengubah nilai `allowed_emails` dari `MapSet` menjadi `List` di dalam sebuah Map.

**Mengapa ini menerapkan konsep Functor?**
Fungsi ini memetakan (memodifikasi) nilai di dalam struktur data (`map`) tanpa mengubah struktur utamanya (tetap sebuah map). Ini mirip dengan operasi `fmap` pada Functor, di mana kita menerapkan transformasi `MapSet -> List` pada isi dari kunci tertentu.

## 10. Applicative

**Analisis:**
Pola Applicative (menjalankan komputasi independen dalam konteks) kurang eksplisit dibandingkan pola Monadik (`with`) di Elixir. Namun, konstruksi struct atau map baru dari beberapa nilai yang mungkin diambil dari map lain adalah bentuk sederhana dari *lifting* nilai ke dalam konteks baru.

### Contoh: Konstruksi `user_profile` (implisit)
Saat data diekstrak dari token JWT dan kemudian digunakan untuk membentuk struct `user` atau map sesi, itu mirip dengan pola Applicative builder: mengambil nilai-nilai independen (email, id) dan menggabungkannya menjadi satu struktur hasil.

## 11. Monad

### Contoh 1: `with` statement untuk Error Handling
**File:** `catatan/packages/catatan_backend/lib/catatan_backend_web/controllers/share_notes.ex` (Line 23-63)

```elixir
    with {:ok, validated_params} <- SharesValidator.validate_share_creation(params) do
      # ...
      if is_nil(current_user) ... do
         # ...
      else
        case Notes.get_note_by_id(note_id) do
          {:ok, note} ->
             # ...
             with {:ok, share, status} <- Shares.create_or_get_share(validated_params) do
                # Success path
             else
                {:error, reason} -> # Error path 2
             end
          {:error, :not_found} -> # Error path 1
        end
      end
    else
      {:error, errors} -> # Validation Error path
        # ...
    end
```

**Penjelasan Singkat:**
Controller ini menangani logika *sharing* yang kompleks: validasi input -> cek autentikasi -> cek kepemilikan note -> buat/ambil share link.

**Mengapa ini disebut Monad (Railway Oriented Programming)?**
Blok `with` (dan `case` bertingkat) di sini mengimplementasikan pola **Monad Either**.
1.  **Chaining:** Eksekusi berjalan langkah demi langkah. Langkah `create_or_get_share` hanya dijalankan jika validasi parameter dan cek kepemilikan sukses.
2.  **Konteks Kegagalan:** Setiap fungsi (`validate`, `get_note`, `create_share`) mengembalikan tuple `{:ok, val}` atau `{:error, err}`. Ini adalah "konteks" komputasi.
3.  **Bind/FlatMap:** Konstruksi bahasa Elixir secara otomatis "membuka" nilai sukses (`val` dari `{:ok, val}`) untuk langkah berikutnya, atau langsung "melempar" nilai error (`{:error, err}`) ke blok penanganan error (`else` atau pattern match gagal). Ini adalah esensi operator bind (`>>=`) pada Monad.

## 12. Monoid

### Contoh 1: Penggabungan List (Implisit)
Meskipun tidak ada definisi Monoid kustom di file yang dianalisis, penggunaan list dan map di Elixir sering memanfaatkan sifat Monoid.
Misalnya penggabungan opsi konfigurasi atau header:

**File:** `catatan/packages/catatan_backend/lib/catatan_backend_web/cors.ex` (Line 55)

```elixir
    add_cors_headers(%{conn | method: nil}, options) ++
      [
        {"access-control-max-age", "#{options[:max_age]}"},
        # ...
      ]
```

Operator `++` adalah operasi biner asosiatif untuk List Monoid, dengan elemen identitas list kosong `[]`.

## 13. QuickCheck

**Analisis:**
Tidak ditemukan file pengujian properti (*property-based testing*) menggunakan library seperti `StreamData` (QuickCheck-nya Elixir) dalam daftar file yang diberikan untuk modul shares dan auth. Pengujian yang ada (seperti `test/catatan_backend/api_key/generate_test.exs`) adalah *unit testing* berbasis contoh (*example-based testing*), bukan pembangkitan kasus uji acak secara otomatis.
