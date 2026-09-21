mod_how_to_use_ui <- function(id) {
  ns <- NS(id)
  div(class = "how-to-page",
    div(class = "section-intro",
      h1("How to Use"),
      p("Alur kerja cMSY++ untuk penilaian satu atau banyak stok.")
    ),
    div(class = "how-grid",
      div(class = "how-card", span(class = "how-number", "1"), h3("Siapkan data"),
          p("Unggah Catch CSV dan Stock ID CSV. Tangkapan menggunakan kg pada interface batch dan dikonversi ke ton sebelum masuk engine.")),
      div(class = "how-card", span(class = "how-number", "2"), h3("Pilih indeks kelimpahan"),
          p("Gunakan catch-only, hitung CPUE dari data operasi, unggah CPUE standar, atau tambahkan biomassa absolut.")),
      div(class = "how-card", span(class = "how-number", "3"), h3("Validasi"),
          p("Perbaiki seluruh error. Warning harus ditinjau, terutama satuan, tahun kosong, lonjakan tangkapan, dan cakupan CPUE.")),
      div(class = "how-card", span(class = "how-number", "4"), h3("Atur parameter"),
          p("Mulai dari preset Standard. Gunakan CV CPUE hasil standardisasi bila tersedia. Naikkan chain atau iterasi jika R-hat belum memadai.")),
      div(class = "how-card", span(class = "how-number", "5"), h3("Jalankan model"),
          p("Batasi proses paralel sesuai kapasitas komputer. Jangan menutup aplikasi saat stok masih berstatus Running.")),
      div(class = "how-card", span(class = "how-number", "6"), h3("Periksa hasil"),
          p("Baca median bersama 95% CI. Periksa Analysis, Management, Kobe, diagnostik BSM, dan retrospective sebelum menggunakan hasil."))
    ),
    div(class = "how-reference",
      h2("Format data"),
      div(class = "format-grid",
        div(h3("Catch CSV"), tags$code("Stock, yr, ct, bt"), p("bt boleh kosong untuk catch-only.")),
        div(h3("Stock ID CSV"), tags$code("Stock, Group, Resilience, btype, ..."), p("Nama Stock harus sama persis dengan Catch CSV.")),
        div(h3("Raw CPUE CSV"), tags$code("nama_alat_tangkap, jumlah_hari_operasi, gt_kapal, berat_ikan_kg"),
            p("Keempat kolom wajib terisi per baris. Hari operasi dan GT harus > 0; berat ikan boleh 0. Stock dan tahun wajib untuk analisis, tetapi boleh diisi sekali melalui interface bila seluruh file mewakili nilai yang sama. Musim, wilayah, dan kovariat tambahan bersifat opsional.")),
        div(h3("Indeks/Biomassa CSV"), tags$code("Stock, yr, bt"), p("CPUE harus positif dan konsisten; biomassa menggunakan ton."))
      )
    ),
    div(class = "how-note",
      h2("Nilai kosong pada data CPUE"),
      p("Sel kosong tidak diganti menjadi nol. Baris yang kehilangan kolom wajib dikeluarkan dan dilaporkan. Kolom opsional boleh kosong selama tidak dipakai sebagai kovariat; bila kovariat dipilih, hanya baris lengkap untuk kovariat tersebut yang masuk model.")
    ),
    div(class = "how-note",
      h2("Interpretasi"),
      p("MSY adalah estimasi dengan ketidakpastian, bukan kuota otomatis. Total MSY lintas spesies hanya ringkasan deskriptif. Hasil catch-only dan klasifikasi dengan interval yang melintasi batas status memerlukan kehati-hatian tambahan.")
    )
  )
}
