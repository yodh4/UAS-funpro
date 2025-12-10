# Panduan Membaca Kode Elixir pada Proyek Catatan Backend

Dokumen ini disusun untuk membantu pembaca yang sudah familiar dengan konsep *Functional Programming* (FP) tetapi masih baru dalam bahasa Elixir. Panduan ini menjelaskan sintaks dan idiom Elixir yang digunakan dalam proyek `catatan_backend` melalui contoh kode nyata.

## 1. Modul dan Atribut

Setiap file Elixir biasanya mendefinisikan sebuah modul. Modul adalah wadah untuk sekumpulan fungsi, makro, dan struct.

**Contoh (`lib/catatan_backend/notes/crdt/yata.ex`):**

```elixir
defmodule CatatanBackend.Notes.Crdt.Yata do
  @moduledoc """
  Dokumentasi modul.
  """

  alias CatatanBackend.Notes.Crdt.StateVector
  # ...
end
```

*   `defmodule NamaModul do ... end`: Mendefinisikan modul baru. Nama modul biasanya mengikuti struktur direktori dan ditulis dalam format `CamelCase`.
*   `@moduledoc`: Atribut modul untuk dokumentasi. String dengan `"""` adalah *heredoc* (string multi-baris).
*   `alias`: Memungkinkan pemanggilan modul dengan nama pendeknya. Contoh di atas mengizinkan penggunaan `StateVector` alih-alih nama lengkap `CatatanBackend.Notes.Crdt.StateVector`.

## 2. Struct dan Tipe Data

Elixir memiliki tipe data dasar (integer, float, atom, list, tuple, map) dan tipe bentukan seperti Struct.

**Contoh (`lib/catatan_backend/notes/crdt/element.ex`):**

```elixir
  @type id :: {writer_id :: String.t(), clock :: non_neg_integer()}

  @type t :: %__MODULE__{
          id: id(),
          origin: id() | nil,
          content: String.t(),
          # ...
        }

  defstruct [:id, :origin, :right_origin, :content, :deleted_at, :note_id]
```

*   `@type`: Mendefinisikan spesifikasi tipe (typespec) untuk dokumentasi dan analisis statis (Dialyzer).
*   `{...}`: Sintaks untuk **Tuple**. Tuple adalah koleksi berurutan dengan ukuran tetap.
*   `atom`: Konstanta yang namanya adalah nilainya sendiri, diawali dengan `:` (contoh: `:id`, `:origin`).
*   `defstruct`: Mendefinisikan **Struct**, yaitu Map dengan key yang sudah ditentukan (compile-time) dan nama modul sebagai "tipe"-nya.
*   `%__MODULE__{...}`: Mengacu pada struct yang didefinisikan dalam modul saat ini.

## 3. Definisi Fungsi dan Pattern Matching

Fungsi didefinisikan dengan `def` (publik) atau `defp` (privat). Elixir sangat bergantung pada *pattern matching* di argumen fungsi.

**Contoh (`lib/catatan_backend/notes/crdt/element.ex`):**

```elixir
  # Fungsi publik dengan satu klausa pattern matching
  def parse(%{
        "id" => id,
        "origin" => origin,
        "content" => content
      }) do
    {:ok,
     %__MODULE__{
       id: parse_id(id),
       origin: parse_id(origin),
       content: content
     }}
  end

  # Klausa "catch-all" jika input tidak cocok dengan pola di atas
  def parse(_), do: {:error, "Invalid element format"}

  # Fungsi privat dengan multiple klausa
  defp parse_id(nil), do: nil

  # Pattern matching pada list dan guards (when ...)
  defp parse_id([writer_id, clock]) when is_binary(writer_id) and is_integer(clock),
    do: {writer_id, clock}
```

*   `def nama_fungsi(arg) do ... end`: Blok fungsi standar.
*   `def nama_fungsi(arg), do: ...`: Sintaks satu baris (*one-liner*).
*   **Pattern Matching pada Map**: `%{"id" => id}` mencocokkan map yang memiliki key "id" dan mengikat nilainya ke variabel `id`.
*   **Underscore (`_`)**: *Wildcard pattern* yang cocok dengan nilai apa pun dan mengabaikannya.
*   **Guards (`when ...`)**: Kondisi tambahan yang harus dipenuhi agar pola cocok. Contoh: `is_binary(writer_id)` memastikan `writer_id` bertipe string.

## 4. Operator Pipe (`|>`)

Operator ini sangat ikonik di Elixir. Ia mengambil hasil dari ekspresi di sebelah kirinya dan memasukkannya sebagai **argumen pertama** ke fungsi di sebelah kanannya.

**Contoh (`lib/catatan_backend/notes/crdt/yata.ex`):**

```elixir
  def to_text(%__MODULE__{} = yata) do
    yata
    |> to_list()
    |> Enum.map(& &1.content)
    |> Enum.join()
  end
```

Kode di atas ekuivalen dengan:
```elixir
Enum.join(Enum.map(to_list(yata), & &1.content))
```
Pembacaan dengan pipe menjadi lebih natural: "Ambil `yata`, ubah ke list, lalu map isinya, lalu gabungkan".

*   `& &1.content`: Sintaks singkat untuk *anonymous function* (lambda). Sama dengan `fn x -> x.content end`.

## 5. Struktur Kontrol: `case`, `cond`, `if`

Elixir memiliki beberapa struktur kontrol alur, namun `case` yang berbasis pattern matching adalah yang paling sering digunakan.

**Contoh `case` (`lib/catatan_backend/notes/store.ex`):**

```elixir
  def get_metadata(note_id) do
    # ... (kode query database) ...
    with {:ok, prepared} <- CassandraClient.prepare(...),
         {:ok, page} <- CassandraClient.execute(...) do
      
      # Pattern matching pada hasil query
      case Enum.to_list(page) do
        [] ->
          {:error, :not_found}

        [row | _] ->
          {:ok, %{"id" => row["id"], ...}}
      end

    else
      {:error, reason} -> {:error, {:cassandra, reason}}
    end
  end
```

*   `case ekspresi do ... end`: Membandingkan hasil ekspresi dengan berbagai pola.
*   `[] -> ...`: Cocok jika list kosong.
*   `[row | _] -> ...`: Cocok jika list tidak kosong. Mengambil elemen pertama (`head`) sebagai `row` dan mengabaikan sisanya (`tail`).

**Contoh `cond` (`lib/catatan_backend/notes.ex`):**

```elixir
        cond do
          is_nil(current_owner) ->
            # ... logika claim ...

          current_owner == user_id ->
            # ... logika sudah dimiliki ...

          true ->
            # ... logika error (default case) ...
        end
```

*   `cond`: Mirip dengan `if-else if-else` berantai. Ia mengevaluasi kondisi dari atas ke bawah dan mengeksekusi blok pertama yang bernilai `truthy` (selain `false` atau `nil`). `true -> ...` bertindak sebagai *fallback*.

## 6. Konstruksi `with`

`with` digunakan untuk merangkai beberapa operasi yang mungkin gagal (mengembalikan `{:error, ...}`) tanpa *nested case* yang dalam ("pyramid of doom").

**Contoh (`lib/catatan_backend/notes/store.ex`):**

```elixir
  def create(note_id, owner_id) do
    # ...
    with {:ok, prepared} <- CassandraClient.prepare(...),
         {:ok, _result} <- CassandraClient.execute(...) do
      {:ok, %{...}}
    else
      {:error, reason} -> {:error, {:cassandra, reason}}
    end
  end
```

*   Setiap baris `pola <- ekspresi` dievaluasi.
*   Jika `ekspresi` menghasilkan nilai yang cocok dengan `pola` (misal `{:ok, ...}`), variabel di pola akan terikat dan eksekusi lanjut ke baris berikutnya.
*   Jika ada yang tidak cocok (misal mengembalikan `{:error, ...}`), rantai berhenti dan eksekusi lompat ke blok `else`.

## 7. GenServer (OTP)

Elixir berjalan di atas VM Erlang yang terkenal dengan model konkurensi aktor-nya. `GenServer` adalah abstraksi standar untuk membuat proses yang menyimpan state, menangani pesan sinkron (`call`), dan asinkron (`cast`).

**Contoh (`lib/catatan_backend/server/notes_session.ex`):**

```elixir
defmodule CatatanBackend.Server.NotesSession do
  use GenServer # Menyertakan perilaku GenServer

  # API Klien (interface publik)
  def start_link(note_id) do
    GenServer.start_link(__MODULE__, note_id, name: via_tuple(note_id))
  end

  def join(note_id, user_profile) do
    GenServer.call(via_tuple(note_id), {:join, user_profile})
  end

  # Callback Server (implementasi internal)
  @impl true
  def init(note_id) do
    state = %{note_id: note_id, writers: %{}}
    {:ok, state}
  end

  @impl true
  def handle_call({:join, user_profile}, _from, state) do
    # ... logika join ...
    {:reply, {:ok, writer}, new_state}
  end

  @impl true
  def handle_cast({:move, ...}, state) do
    # ... logika update state tanpa reply ...
    {:noreply, new_state}
  end
end
```

*   `use GenServer`: Mengimpor fungsionalitas GenServer.
*   `start_link`: Fungsi untuk memulai proses baru.
*   `GenServer.call`: Mengirim pesan sinkron (menunggu balasan). Ditangani oleh `handle_call`.
*   `GenServer.cast`: Mengirim pesan asinkron (fire-and-forget). Ditangani oleh `handle_cast`.
*   `handle_info`: Menangani pesan internal (seperti timer atau pesan dari proses lain).
*   `state`: Variabel terakhir dalam return tuple (`{:reply, ..., state}` atau `{:noreply, state}`) adalah state terbaru yang akan disimpan oleh proses.

## 8. Phoenix Framework (Web)

Kode web (Controller, Channel) menggunakan makro dan DSL dari Phoenix.

**Contoh Controller (`lib/catatan_backend_web/controllers/notes.ex`):**

```elixir
  def create(conn, params) do
    # ...
    case Notes.create_note(owner_id) do
      {:ok, note} ->
        conn
        |> put_status(:created)
        |> Response.success_response("Note created successfully", note)

      {:error, reason} ->
        # ...
    end
  end
```

*   `conn`: Struct koneksi HTTP yang dimanipulasi secara fungsional melalui pipeline (menggunakan `|>`).
*   `put_status`: Mengubah status HTTP response.

**Contoh Channel (`lib/catatan_backend_web/channels/notes.ex`):**

```elixir
  # Pattern matching pada topik channel ("notes:123")
  def join("notes:" <> notes_id, _payload, socket) do
    # ... logika join ...
    {:ok, response, socket}
  end

  # Menangani pesan masuk dari klien
  def handle_in("insert", payload, socket) do
    # ... logika insert ...
    {:reply, {:ok, result}, socket}
  end
```

*   `<>`: Operator konkatenasi string (biner). Digunakan dalam pattern matching untuk mengekstrak `notes_id`.
*   `socket`: Menyimpan state koneksi WebSocket (mirip `conn` di HTTP).

## 9. Sigils

Elixir memiliki sintaks khusus yang disebut *sigils* untuk literal tertentu, diawali dengan `~`.

**Contoh (`lib/catatan_backend_web/template/registration/template.ex`):**

```elixir
  def render_html(assigns) do
    ~H"""
    <!DOCTYPE html ...>
    <html dir="ltr" lang="en">
      <!-- ... -->
      {@verification_code}
    </html>
    """
  end
```

*   `~H"""..."""`: Sigil untuk HEEx (HTML EEx). Digunakan dalam Phoenix LiveView dan Component untuk menulis template HTML yang aman dan tervalidasi.
*   `{@variabel}`: Interpolasi variabel dalam template HEEx.

## 10. Tipe Data

Elixir memiliki berbagai tipe data bawaan yang sering digunakan. Berikut adalah beberapa yang paling umum:

### 10.1. Tipe Data Dasar

*   **Integer**: Bilangan bulat.
    ```elixir
    1
    10
    -5
    1_000_000 # underscore untuk keterbacaan
    ```
*   **Float**: Bilangan desimal.
    ```elixir
    1.0
    3.14159
    -0.12
    1.0e-10
    ```
*   **Atom**: Konstanta yang namanya adalah nilainya sendiri. Diawali dengan `:`.
    ```elixir
    :ok
    :error
    :user_id
    :catatan_backend # Nama modul juga merupakan atom
    true  # alias untuk :true
    false # alias untuk :false
    nil   # alias untuk :nil
    ```
*   **Boolean**: `true` dan `false` (sebenarnya adalah atom).
    ```elixir
    true
    false
    ```
*   **String (Binary)**: Urutan byte yang diapit tanda kutip ganda `"`. Selalu encoding UTF-8.
    ```elixir
    "Halo Dunia"
    "Baris satu\nBaris dua"
    "Interpolasi: #{1 + 1}"
    ```

### 10.2. Tipe Data Koleksi

*   **List**: Linked list (bukan array). Operasi di kepala (`head`) cepat, akses acak lambat `O(n)`.
    ```elixir
    [1, 2, 3]
    ["a", "b", "c"]
    [1, "dua", :tiga] # Bisa campuran tipe
    ```
    Operasi umum:
    ```elixir
    hd([1, 2]) # 1 (head)
    tl([1, 2]) # [2] (tail)
    [1 | [2, 3]] # [1, 2, 3] (consing)
    ```
*   **Tuple**: Koleksi berurutan dengan ukuran tetap. Akses elemen cepat, modifikasi mahal (menyalin seluruh tuple). Sering digunakan untuk mengembalikan multiple value (misal `{:ok, value}`).
    ```elixir
    {1, 2}
    {:ok, "berhasil"}
    {:error, :not_found, "Data tidak ada"}
    ```
*   **Map**: Koleksi key-value. Key bisa berupa tipe apa saja.
    ```elixir
    %{"nama" => "Budi", "umur" => 30}
    %{nama: "Budi", umur: 30} # Shortcut jika key adalah atom
    ```
    Akses dan update:
    ```elixir
    map = %{nama: "Budi"}
    map.nama # "Budi" (hanya jika key atom)
    map["nama"] # Error jika key atom, harus map[:nama]
    %{map | nama: "Badu"} # Update (key harus sudah ada)
    ```
*   **Struct**: Map dengan aturan lebih ketat (field ditentukan saat compile time, nama modul sebagai tipe). Lihat Bagian 2 untuk detailnya.
    ```elixir
    defmodule User do
      defstruct [:name, :age]
    end
    
    %User{name: "Budi", age: 25}
    ```
*   **Keyword List**: List dari tuple 2 elemen di mana elemen pertama adalah atom. Sering digunakan untuk opsi fungsi.
    ```elixir
    [a: 1, b: 2] 
    # Sama dengan: [{:a, 1}, {:b, 2}]
    ```
    Karakteristik: Key harus atom, urutan dijamin, key bisa duplikat.

## 11. Konsep Penting Lainnya

Ada beberapa mekanisme lain di Elixir yang sering muncul di kode proyek ini yang perlu dipahami perbedaannya.

### 11.1. Direktif Modul (import, require, use)

Selain `alias` (yang hanya menyingkat nama), ada tiga cara lain untuk berinteraksi dengan modul lain:

*   **`import`**: Membawa fungsi-fungsi dari modul lain ke dalam scope saat ini, sehingga bisa dipanggil tanpa nama modul.
    *   Sering digunakan untuk fungsi umum seperti `Plug.Conn` di web controller.
*   **`require`**: Memuat definisi **makro** dari modul lain agar bisa dikompilasi dengan benar.
    *   Wajib digunakan jika Anda ingin memanggil makro (seperti `Logger.info/1`).
*   **`use`**: Memanggil makro `__using__` di modul target. Ini biasanya menyuntikkan kode (boilerplate) ke dalam modul Anda. Sering digunakan oleh library/framework (seperti Phoenix atau GenServer).

**Contoh Penggunaan:**

```elixir
defmodule CatatanBackend.Server.NotesNew do
  use GenServer      # Menyuntikkan perilaku GenServer (handle_call, init, dll)
  require Logger     # Memungkinkan penggunaan makro Logger
  
  # ...
  
  def init(note_id) do
    Logger.info(...) # Pemanggilan makro Logger
    # ...
  end
end
```

### 11.2. Protokol (`defimpl`)

Protokol adalah mekanisme polimorfisme di Elixir (mirip *interface* di bahasa lain). `defimpl` digunakan untuk mendefinisikan implementasi spesifik dari sebuah protokol untuk tipe data tertentu.

Di proyek ini, ini sering terlihat untuk `Jason.Encoder`, yang mengatur bagaimana sebuah struct diubah menjadi JSON.

**Contoh (`lib/catatan_backend/cursor.ex`):**

```elixir
  # Implementasi protokol Jason.Encoder khusus untuk struct CatatanBackend.Cursor
  defimpl Jason.Encoder, for: CatatanBackend.Cursor do
    def encode(cursor, opts) do
      # Mengubah struct cursor menjadi Map sederhana agar bisa di-encode ke JSON
      %{
        after_element: encode_element_id(cursor.after_element),
        offset: cursor.offset
      }
      |> Jason.Encode.map(opts)
    end
    
    # ... fungsi helper privat ...
  end
```

### 11.3. Penanganan Exception (`try/rescue`)

Elixir umumnya lebih menyukai mengembalikan tuple error (`{:error, reason}`) daripada melempar exception. Namun, untuk interaksi dengan library luar atau database yang mungkin crash, blok `try/rescue` digunakan untuk menangkap error tersebut dan mengubahnya menjadi tuple yang aman.

**Contoh (`lib/catatan_backend/notes/store.ex`):**

```elixir
  def create(note_id, owner_id) do
    # ... logika database ...
  rescue
    # Menangkap exception yang mungkin terjadi (misal koneksi DB putus)
    exception -> {:error, {:exception, exception}}
  end
```

## 12. Ringkasan Sintaks

| Sintaks | Keterangan | Contoh |
| :--- | :--- | :--- |
| `defmodule` | Definisi Modul | `defmodule MyModule do ... end` |
| `def` / `defp` | Definisi Fungsi (Publik/Privat) | `def my_func(a), do: a + 1` |
| `%Struct{}` | Struct (Data) | `%User{name: "Budi"}` |
| `%{key => val}` | Map | `%{"nama" => "Budi", :umur => 20}` |
| `{a, b}` | Tuple | `{:ok, result}` |
| `:atom` | Atom (Konstanta) | `:ok`, `:error`, `:user_id` |
| `|>` | Pipe Operator | `data |> proses1() |> proses2()` |
| `%{map | key: val}` | Map Update | `%{state | count: state.count + 1}` |
| `_` | Wildcard Pattern | `case x do _ -> "match all" end` |
| `&` | Capture Operator (Lambda) | `&(&1 + 1)` sama dengan `fn x -> x + 1 end` |
| `<<...>>` | Binary / String | `"hello"` adalah binary `<<104, 101, ...>>` |
| `<>` | String Concatenation | `"Hello " <> "World"` |
