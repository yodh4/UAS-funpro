# Analisis Konsep Functional Programming pada Proyek Catatan Backend

Berikut adalah analisis penerapan konsep-konsep Functional Programming pada codebase `@catatan/packages/catatan_backend`. Proyek ini ditulis dalam **Elixir**, sebuah bahasa pemrograman fungsional yang berjalan di atas BEAM (Erlang virtual machine).

## 1. Algebraic Data Type (ADT)

### Contoh 1: `defstruct` pada modul `CatatanBackend.Notes.Crdt.Element`
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/notes/crdt/element.ex` (Line 4-13)

```elixir
  @type id :: {writer_id :: String.t(), clock :: non_neg_integer()}

  @type t :: %__MODULE__{
          id: id(),
          origin: id() | nil,
          right_origin: id() | nil,
          content: String.t(),
          note_id: String.t(),
          deleted_at: String.t() | nil
        }

  defstruct [:id, :origin, :right_origin, :content, :deleted_at, :note_id]
```

**Penjelasan Singkat:**
Struct `%Element{}` merepresentasikan elemen dasar dalam struktur data CRDT YATA. Ini mendefinisikan bentuk data yang pasti dengan field-field tertentu.

**Mengapa ini disebut Algebraic Data Type (ADT)?**
Di Elixir, struct adalah bentuk dari **Product Type**.
1.  **Product Type:** Sebuah nilai `%Element{}` dibentuk dari gabungan (perkalian Cartesian) dari semua field-nya (`id` DAN `origin` DAN `right_origin` DAN ...). Untuk membuat sebuah `Element` yang valid, kita memerlukan kombinasi nilai untuk semua field tersebut.
2.  Tipe `id()` sendiri didefinisikan sebagai tuple `{String.t(), non_neg_integer()}`, yang juga merupakan *Product Type* sederhana.
3.  Penggunaan tipe union `| nil` pada `origin` menunjukkan sifat **Sum Type** (nilai bisa berupa `id` ATAU `nil`), yang merupakan ciri khas representasi opsi (Optional/Maybe) dalam sistem tipe aljabar.

### Contoh 2: Tipe data `GamePhase` (Tidak ada, diganti dengan `Result` tuple)
Di Elixir, ADT seringkali direpresentasikan secara implisit menggunakan **Tuple** dan **Atom** (Tagged Unions).

**File:** `catatan/packages/catatan_backend/lib/catatan_backend/notes/crdt/yata.ex` (Line 67)

```elixir
@spec delete(t, String.t()) :: {:ok, t, Element.t()} | {:error, :not_found}
```

**Penjelasan Singkat:**
Spesifikasi tipe kembalian fungsi `delete` mendefinisikan dua kemungkinan bentuk hasil.

**Mengapa ini disebut Algebraic Data Type (ADT)?**
Ini adalah bentuk **Sum Type** (Tagged Union) yang idiomatik di Elixir/Erlang.
1.  Hasil fungsi adalah salah satu dari varian berikut:
    *   `{:ok, t, Element.t()}`: Varian sukses (Success case).
    *   `{:error, :not_found}`: Varian gagal (Failure case).
2.  Atom `:ok` dan `:error` bertindak sebagai *tag* atau konstruktor yang membedakan kedua varian tersebut. Ini setara dengan tipe `Result<T, E>` atau `Either` di bahasa fungsional ber-tipe statis (seperti Haskell/Rust). Pola ini memungkinkan *Pattern Matching* untuk menangani alur program secara eksplisit.

## 2. Pattern Matching

### Contoh 1: Fungsi `compare_conflicting`
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/notes/crdt/yata.ex` (Line 167-187)

```elixir
  defp compare_conflicting(%Element{} = a, %Element{} = b, all_elements) do
    cond do
      a.id == b.id ->
        true

      a.right_origin == b.id ->
        true
      # ...
```

**Penjelasan Singkat:**
Fungsi ini membandingkan dua struct `%Element{}` untuk menentukan urutan dalam CRDT.

**Mengapa ini disebut Pattern Matching?**
1.  **Destructuring Struct:** Pada argumen fungsi, `%Element{} = a` memastikan bahwa argumen yang diterima *harus* berupa struct `Element`. Ini sekaligus mencocokkan tipe data dan mengikatnya ke variabel `a`.
2.  **Kondisional:** Meskipun blok utamanya menggunakan `cond` (yang lebih mirip `if-else if`), deklarasi argumen fungsi itu sendiri menggunakan pattern matching untuk memvalidasi struktur data input secara deklaratif.

### Contoh 2: Fungsi `normalize_payload`
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/email/process.ex` (Line 55-63)

```elixir
  defp normalize_payload(%{"to" => to, "subject" => subject, "type" => "registration"} = payload)
       when is_binary(to) and is_binary(subject) do
    {:ok,
     %{
       to: to,
       subject: subject,
       type: :registration,
       data: atomize_keys(Map.get(payload, "data", %{}))
     }}
  end
```

**Penjelasan Singkat:**
Fungsi ini menormalisasi payload email, khusus untuk tipe "registration".

**Mengapa ini disebut Pattern Matching?**
1.  **Pencocokan Isi Map:** Pola `%{"to" => to, ... "type" => "registration"}` melakukan pencocokan sangat spesifik. Fungsi ini **hanya** akan dijalankan jika map input memiliki kunci "type" dengan nilai persis "registration".
2.  **Ekstraksi Nilai:** Nilai dari kunci "to" dan "subject" langsung diekstrak ke variabel `to` dan `subject`.
3.  **Guards:** Klausa `when is_binary(to) ...` menambahkan lapisan validasi tambahan pada pola tersebut.
4.  Jika input tidak cocok dengan pola ini (misal "type" bukan "registration"), Elixir akan mencoba mencocokkan definisi fungsi `normalize_payload` berikutnya (jika ada). Ini adalah mekanisme *dispatching* logika yang sangat kuat berbasis data.

## 3. Function Composition

### Contoh 1: Fungsi `to_text` (Pipe Operator `|>`)
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/notes/crdt/yata.ex` (Line 143-147)

```elixir
  def to_text(%__MODULE__{} = yata) do
    yata
    |> to_list()
    |> Enum.map(& &1.content)
    |> Enum.join()
  end
```

**Penjelasan Singkat:**
Mengubah struktur data YATA menjadi string teks tunggal.

**Mengapa ini disebut Function Composition?**
1.  **Operator Pipe (`|>`):** Operator ini adalah cara Elixir melakukan komposisi fungsi. Ekspresi `x |> f |> g` setara secara matematis dengan `g(f(x))`.
2.  **Aliran Data:** Data mengalir dari satu fungsi ke fungsi berikutnya:
    *   `yata` masuk ke `to_list()` -> menghasilkan list elemen.
    *   List elemen masuk ke `Enum.map(...)` -> menghasilkan list string (konten).
    *   List string masuk ke `Enum.join()` -> menghasilkan string tunggal.
3.  Ini membuat kode sangat mudah dibaca (seperti urutan instruksi) sambil tetap mempertahankan sifat fungsional (transformasi data murni).

### Contoh 2: Fungsi `deliver_email`
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/email/process.ex` (Line 38-46)

```elixir
  defp deliver_email(to, subject, html_body_content, text_body_content) do
    # ...
    new()
    |> to(to)
    |> from(from_email)
    |> subject(subject)
    |> html_body(html_body_content)
    |> text_body(text_body_content)
    |> SESMailer.deliver()
  end
```

**Penjelasan Singkat:**
Membangun dan mengirim objek email menggunakan library Swoosh.

**Mengapa ini disebut Function Composition?**
Sama seperti contoh sebelumnya, ini menggunakan operator pipe untuk membangun struktur data secara bertahap. Fungsi `new()` membuat struct email kosong, lalu fungsi-fungsi seperti `to()`, `from()`, `subject()` masing-masing menerima struct email tersebut, memodifikasinya (mengembalikan struct baru), dan meneruskannya ke langkah berikutnya. Ini adalah pola *Functional Builder*.

## 4. High Order Function

### Contoh 1: `Enum.reduce` pada `delete_batch`
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/notes/crdt/yata.ex` (Line 95-107)

```elixir
    {updated_elements, deleted, not_found} =
      Enum.reduce(element_ids, {yata.elements, [], []}, fn element_id,
                                                           {elements, deleted_acc, not_found_acc} ->
        case Map.get(elements, element_id) do
          nil ->
            {elements, deleted_acc, [element_id | not_found_acc]}

          element ->
            # ... logika update ...
            {updated_elements, [updated_element | deleted_acc], not_found_acc}
        end
      end)
```

**Penjelasan Singkat:**
Fungsi ini menghapus banyak elemen sekaligus dengan mengiterasi list ID.

**Mengapa ini disebut High Order Function (HOF)?**
1.  **Menerima Fungsi:** `Enum.reduce` adalah HOF klasik (fold). Argumen ketiganya adalah sebuah **fungsi anonim** (`fn element_id, acc -> ... end`).
2.  **Abstraksi Iterasi:** `Enum.reduce` menangani mekanisme loop/rekursi. Kita hanya perlu memberikan logika "apa yang harus dilakukan pada setiap langkah" (fungsi reduksi). Fungsi ini mengakumulasi state (`{elements, deleted_acc, ...}`) seiring berjalannya iterasi.

### Contoh 2: `Enum.reject` pada `generate_delta`
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/notes/crdt/sync.ex` (Line 7-10)

```elixir
    |> Enum.reject(fn element ->
      {writer_id, clock} = element.id
      StateVector.has_seen?(client_state_vector, writer_id, clock)
    end)
```

**Penjelasan Singkat:**
Menyaring elemen-elemen yang sudah dilihat oleh klien.

**Mengapa ini disebut High Order Function (HOF)?**
`Enum.reject` menerima sebuah fungsi predikat. Ia akan menjalankan fungsi tersebut untuk setiap elemen dan membuang elemen yang menghasilkan `true`. Ini adalah abstraksi fungsional untuk filtering.

## 5. Lambda Function

### Contoh 1: Fungsi Anonim dalam `Enum.map`
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/notes/crdt/yata.ex` (Line 146)

```elixir
    |> Enum.map(& &1.content)
```

**Penjelasan Singkat:**
Mengambil field `content` dari setiap elemen.

**Mengapa ini disebut Lambda Function?**
1.  **Sintaks Capture (`&`):** Ekspresi `& &1.content` adalah sintaks singkat (shorthand) di Elixir untuk fungsi anonim (lambda).
2.  Secara lengkap, ini setara dengan `fn x -> x.content end`.
3.  Fungsi ini tidak memiliki nama dan dibuat *in-place* untuk keperluan spesifik transformasi data di dalam pipeline `map`.

### Contoh 2: Fungsi Anonim dalam `StateVector.merge`
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/notes/crdt/state_vector.ex` (Line 26)

```elixir
  def merge(sv1, sv2) do
    Map.merge(sv1, sv2, fn _k, v1, v2 -> max(v1, v2) end)
  end
```

**Penjelasan Singkat:**
Menggabungkan dua State Vector dengan mengambil nilai maksimum clock.

**Mengapa ini disebut Lambda Function?**
Ekspresi `fn _k, v1, v2 -> max(v1, v2) end` adalah definisi fungsi lambda eksplisit. Fungsi ini mendefinisikan strategi resolusi konflik saat penggabungan Map: "jika ada kunci yang sama, ambil nilai yang lebih besar (`max`)". Fungsi ini dipassing sebagai argumen ke HOF `Map.merge`.

## 6. List Comprehension

### Contoh 1: `for` comprehension pada `missing`
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/notes/crdt/state_vector.ex` (Line 18-24)

*Catatan: File yang diberikan menggunakan `Enum.flat_map`, namun konsepnya serupa. Jika ada penggunaan `for` di file lain, itu adalah comprehension.*

Melihat file `state_vector.ex`, kode menggunakan `Enum.flat_map` yang secara semantik mirip dengan list comprehension:

```elixir
  def missing(ours, theirs) do
    Enum.flat_map(theirs, fn {writer_id, their_clock} ->
      # ... logika ...
    end)
  end
```

Meskipun sintaks `for` tidak terlihat di snippet utama yang dianalisis, Elixir memiliki konstruksi `for` yang sangat *powerful* sebagai List Comprehension.

Contoh (hipotetis berdasarkan pola umum Elixir):
```elixir
for {k, v} <- map, v > 10, do: k
```
Ini akan menyaring map dan menghasilkan list key. Jika kode proyek menggunakan `for`, itu adalah penerapan langsung List Comprehension.

## 7. Lazy Evaluation

### Contoh 1: `Enum.reduce_while`
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/migrator.ex` (Line 68)

```elixir
    result =
      Enum.reduce_while(pending, {:ok, 0}, fn file, {:ok, count} ->
        case run_migration(file) do
          :ok -> {:cont, {:ok, count + 1}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)
```

**Penjelasan Singkat:**
Menjalankan migrasi database satu per satu, dan berhenti segera jika ada error.

**Mengapa ini disebut Lazy Evaluation (Short-circuiting)?**
1.  **Evaluasi Terhenti:** `reduce_while` tidak memproses seluruh list `pending` secara membabi buta. Ia mengevaluasi elemen satu per satu.
2.  **Halt:** Jika fungsi callback mengembalikan `{:halt, ...}`, iterasi langsung berhenti. Elemen sisa dalam list tidak akan pernah diproses. Ini menghemat resource dan mencegah efek samping yang tidak diinginkan (menjalankan migrasi selanjutnya setelah yang sebelumnya gagal).

### Contoh 2: `Stream` (Konseptual)
Meskipun snippet yang diberikan banyak menggunakan `Enum` (yang *eager*), Elixir memiliki modul `Stream` untuk pemrosesan malas (*lazy*). Penggunaan `GenStage` (seperti di `email/process.ex`) juga merupakan bentuk evaluasi malas berbasis *demand-driven* (backpressure), di mana data hanya diproses saat konsumen memintanya, bukan didorong paksa.

## 8. Currying

**Analisis:**
Elixir (dan Erlang) **tidak mendukung Currying secara native** seperti Haskell. Fungsi di Elixir memiliki arity (jumlah argumen) yang tetap. `func(a, b)` berbeda dengan `func(a)(b)`.

Namun, konsep **Partial Application** (yang mirip) sering dicapai dengan `Kernel.apply/2` atau dengan membungkus fungsi dalam fungsi anonim lain.

Contoh di `catatan/packages/catatan_backend/lib/catatan_backend/notes/crdt/sync.ex` (Line 14):
```elixir
Enum.reduce(elements, yata, &Yata.integrate(&2, &1))
```
Di sini, `&Yata.integrate(&2, &1)` membuat fungsi baru yang "menangkap" referensi ke fungsi `Yata.integrate`. Meskipun bukan currying otomatis, ini adalah teknik fungsional untuk memanipulasi fungsi.

## 9. Functor

**Analisis:**
Elixir tidak memiliki typeclass atau trait system seperti Haskell, jadi konsep "Functor" tidak diformalkan secara ketat. Namun, fungsi `map` pada list atau struktur data lain adalah manifestasi dari perilaku Functor.

### Contoh: `Enum.map`
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/notes/crdt/store.ex` (Line 29)

```elixir
      elements = Enum.map(page, &row_to_element/1)
```

**Penjelasan Singkat:**
Mengubah list baris database (`row`) menjadi list struct `Element`.

**Mengapa ini menerapkan konsep Functor?**
`Enum.map` menerapkan fungsi transformasi (`row_to_element`) ke setiap nilai di dalam "kontainer" (List/Enumerable) tanpa mengubah struktur kontainernya. Ini memenuhi definisi operasional Functor: memetakan fungsi ke atas struktur data.

## 10. Applicative

**Analisis:**
Seperti Functor, Applicative tidak eksplisit di Elixir. Namun, pola `with` statement di Elixir sangat mirip dengan komputasi monadik/aplikatif yang menangani konteks "kemungkinan gagal" (`{:ok, ...}` atau `{:error, ...}`).

### Contoh: `with` statement
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/notes/store.ex` (Line 86-88)

```elixir
    with {:ok, elements} <- YataStore.load_elements(note_id),
         {:ok, state_vector} <- YataStore.load_state_vector(note_id) do
      yata = reconstruct_yata(note_id, writer_id, elements, state_vector)
      {:ok, yata}
    end
```

**Penjelasan Singkat:**
Memuat elemen dan state vector secara berurutan.

**Mengapa ini mirip Applicative/Monad?**
Konstruksi ini merangkai dua operasi yang bisa gagal (`load_elements` dan `load_state_vector`). Jika langkah pertama sukses, hasilnya dibuka (`unwrapped`) dan langkah kedua dijalankan. Jika langkah kedua sukses, hasilnya digabungkan di blok `do`. Ini sangat mirip dengan notasi `do` di Haskell atau penggunaan Applicative `<*>` untuk mengumpulkan hasil dari beberapa komputasi independen.

## 11. Monad

**Analisis:**
Elixir adalah bahasa fungsional yang dinamis. Pola **"Railway Oriented Programming"** menggunakan tuple `{:ok, val}` dan `{:error, reason}` bersama dengan `with` statement adalah implementasi pragmatis dari konsep Monad (khususnya Either Monad) untuk penanganan error.

### Contoh: `with` statement untuk Error Handling
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/email/process.ex` (Line 27-31)

```elixir
    with {:ok, %{to: to, subject: subject, type: type, data: data}} <- normalize_payload(payload),
         {:ok, %{html: html, text: text}} <- Templates.render(type, data),
         {:ok, _response} <- deliver_email(to, subject, html, text) do
      delete_message(queue_url, receipt_handle)
    else
      {:error, reason} ->
        Logger.error("Failed to process email payload: #{inspect(reason)}")
    end
```

**Penjelasan Singkat:**
Pipeline pemrosesan email: normalisasi -> render -> kirim -> hapus.

**Mengapa ini disebut Monad (pola)?**
1.  **Chaining:** Operasi dirangkai secara sekuensial. Langkah 2 (`Templates.render`) membutuhkan hasil sukses dari langkah 1 (`normalize_payload`).
2.  **Short-Circuiting (Fail Fast):** Jika `normalize_payload` mengembalikan `{:error, ...}`, eksekusi langsung lompat ke blok `else`. Langkah `render` dan `deliver` dilewati. Ini persis perilaku operator bind (`>>=`) pada Monad `Either` atau `Maybe`.
3.  Ini memungkinkan penulisan "happy path" (jalur sukses) yang bersih dan linier, memisahkan logika utama dari logika penanganan error.

## 12. Monoid

### Contoh: Penggabungan State Vector
**File:** `catatan/packages/catatan_backend/lib/catatan_backend/notes/crdt/state_vector.ex` (Line 25-27)

```elixir
  @spec merge(t, t) :: t
  def merge(sv1, sv2) do
    Map.merge(sv1, sv2, fn _k, v1, v2 -> max(v1, v2) end)
  end
```

**Penjelasan Singkat:**
Menggabungkan dua map State Vector.

**Mengapa ini disebut Monoid?**
Secara matematis, State Vector dalam CRDT ini membentuk sebuah Monoid (tepatnya Join-Semilattice):
1.  **Operasi Biner Asosiatif (`merge`):** Menggabungkan dua state vector `A` dan `B`. Urutan penggabungan (grouping) tidak mengubah hasil akhir.
2.  **Elemen Identitas:** Fungsi `initialize` (Line 9) mengembalikan `%{}`, yang bertindak sebagai elemen identitas (menggabungkan map kosong dengan map apa pun menghasilkan map tersebut).
3.  Sifat Monoid ini sangat krusial untuk CRDT karena menjamin bahwa sinkronisasi state bisa dilakukan berulang kali dan dalam urutan sembarang namun tetap menghasilkan state akhir yang konsisten (*convergence*).

## 13. QuickCheck (Property-Based Testing)

**Analisis:**
Dalam ekosistem Elixir, konsep QuickCheck diimplementasikan oleh library **StreamData**. Meskipun saya tidak melihat file tes yang secara eksplisit mengimpor `StreamData` dalam snippet yang diberikan, file `test/catatan_backend/notes/crdt/element_test.exs` menunjukkan pengujian unit yang kuat.

Namun, prinsip invarian (yang menjadi inti property-based testing) terlihat pada tes berikut:

### Contoh: Tes Invarian Roundtrip
**File:** `catatan/packages/catatan_backend/test/catatan_backend/notes/crdt/element_test.exs` (Line 55-58)

```elixir
  describe "encode_id/1 and decode_id/1 roundtrip" do
    test "roundtrip preserves the id" do
      id = {"my-writer", 123}
      assert id == id |> Element.encode_id() |> Element.decode_id()
    end
  end
```

**Penjelasan Singkat:**
Menguji bahwa proses encode lalu decode mengembalikan nilai awal.

**Mengapa ini terkait Konsep Property-Based Testing?**
Ini menguji **sifat (property)** dari fungsi: "Untuk setiap ID yang valid, `decode(encode(id))` harus sama dengan `id`". Dalam property-based testing penuh, kita akan menggunakan generator untuk membuat ribuan ID acak dan memverifikasi sifat ini. Di sini, ini dilakukan secara manual ("example-based"), tetapi pola pikirnya mengarah ke validasi invarian sistem.
