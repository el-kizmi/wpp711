# Audit Radikal Sistem Perhitungan cMSY++ WPP711

Tanggal penerimaan: 12 September 2026  
Dataset uji: `batch_catch_pelagis.csv` dan `batch_id_pelagis.csv`  
Run penerimaan historis: 12 September 2026 (artefak run dibersihkan setelah audit)

## Kesimpulan penerimaan

Pipeline hasil perbaikan mengeksekusi 30/30 stok, masing-masing dengan empat langkah retrospective (full series dan tiga peel), tanpa error proses, kegagalan socket, output parsial, atau nilai inti non-finite. Seluruh stok menghasilkan CSV CMSY++, RData, log, dan baris ringkasan terparse. Lima stok yang semula memiliki R-hat di atas 1,1 pada salah satu parameter/peel dijalankan ulang dengan 3 chain dan 120.000 iterasi; seluruh run penerimaan akhirnya memiliki R-hat maksimum 1,0967. Uji browser langsung juga berhasil membuka Analysis Plot, Management Plot, dan Kobe Plot; ketiga gambar terlihat, mengarah ke folder batch/stok yang benar, dan tidak menghasilkan error console.

Keberhasilan komputasi **bukan** bukti bahwa seluruh status stok kuat secara ilmiah. Semua 30 seri adalah catch-only (`btype=None`), semua prior biomassa/r menggunakan prior otomatis, dan 29/30 klasifikasi titik berubah pada sebagian kombinasi batas 95% CI. Oleh sebab itu status harus dibaca sebagai indikatif dan disertai interval, bukan keputusan deterministik.

## Cacat kritis yang diperbaiki

1. **Skala absolut 1.000×** — data batch WPP711 menggunakan kg, sedangkan engine mengharapkan ton dan mengeluarkan ribu ton. Input batch sekarang dibagi 1.000 sebelum engine; parser mengubah keluaran fisik kembali menjadi ton/tahun.
2. **Nested parallelism/socket failure** — outer batch dan `jags.parallel` sama-sama membuat worker. Chain JAGS sekarang berjalan tanpa PSOCK internal ketika batch memparallelkan stok.
3. **Parameter palsu** — `n.chains` dari UI sebelumnya ditimpa menjadi 2 dan label “Iterations” sebenarnya mengendalikan prior draws, bukan iterasi MCMC. Kontrak parameter telah diperbaiki dan parameter MCMC dibuat eksplisit.
4. **Parser salah metode** — catch-only mengambil `r_BSM`/`k_BSM` yang `NA`. Parser sekarang memilih BSM bila tersedia dan fallback ke CMSY.
5. **Ringkasan lintas stok rusak** — `group=NULL` dan `n_years=nrow(output)` selalu menghasilkan kelompok kosong dan satu tahun. Kini Group dibaca dari output dan rentang tahun dihitung benar.
6. **Sukses palsu** — exit code 0 tanpa baris output valid sebelumnya tetap ditandai Done. Kini output wajib memiliki satu stok tepat dan metrik inti finite/positif.
7. **CI fiktif pada Results** — UI sebelumnya menampilkan MSY ±30% buatan. Kini batas 95% resmi dari engine dipakai untuk MSY, B/Bmsy, dan F/Fmsy.
8. **Plot Results rusak/freezing** — URL kehilangan folder batch dan cache-buster berubah setiap render. Path kini lengkap, timestamp stabil, gambar lazy-loaded, dan animasi layout berat dimatikan.
9. **Override tidak diterapkan** — `e_creep` dan `force_cmsy` pada tabel override kini benar-benar masuk ke metadata per stok.
10. **Validasi terlalu lunak** — ditambahkan pemeriksaan duplikat stok-tahun/metadata, domain tahun, catch positif (karena `log(catch)`), kontinuitas, panjang seri retrospective, tipe abundance, pasangan prior, dan preflight wajib sebelum Run.
11. **Instalasi paket saat job berjalan** — auto-install pada worker dihapus. Dependensi dikunci di `R_library` proyek dan diverifikasi sebelum proses dibuat.
12. **Nama/path tidak aman dan collision run** — nama folder stok disanitasi, collision ditolak, dan ID run memakai subdetik.

## Risiko ilmiah dataset

- Tidak ada indeks kelimpahan/CPUE untuk 30 stok; hasil sepenuhnya catch-only.
- Tidak ada prior ahli untuk batas r atau biomassa awal/antara/akhir pada seluruh stok.
- Empat belas seri memiliki lompatan catch tahunan di atas 10×; ekstremnya Coryphaena hippurus 326× dan Katsuwonus pelamis 268,7×. Ini wajib diverifikasi terhadap perubahan pelaporan, satuan, cakupan armada, atau agregasi wilayah.
- Median rasio lebar CI MSY (UCL/LCL) pada run audit awal sekitar 1,85; ketidakpastian status dominan.
- Penjumlahan MSY spesies hanya statistik deskriptif. Itu bukan MSY ekosistem dan tidak boleh langsung dijadikan TAC kelompok tanpa koreksi interaksi, overlap armada, selectivity, dan kualitas pelaporan.
- Klasifikasi aplikasi memakai ambang operasional khusus (termasuk zona 0,8–1,2 B/Bmsy), bukan probabilitas posterior setiap kuadran Kobe. Label kini diberi `Robust/Uncertain` berdasarkan seluruh sudut interval 95%.

## Reproduksi

```powershell
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' scripts\run_batch_cli.R `
  --catch=batch_catch_pelagis.csv --id=batch_id_pelagis.csv `
  --catch-unit=kg --max-parallel=3 --chains=2 --retros=true --plots=false
```

Setiap run baru membuat artefak di `runs/`, termasuk `batch_status.csv`, `batch_results.csv`, output engine, dan log per stok. Jalankan seluruh berkas `tests/test_*.R` untuk memeriksa impor, kontrak input/parser, standardisasi CPUE, integrasi engine, Hasil, dan Ringkasan.

## Catatan integritas input aktif

File input aktif di root berisi tiga stok. Runner tidak menulis kembali atau mengubah file sumber tersebut.
