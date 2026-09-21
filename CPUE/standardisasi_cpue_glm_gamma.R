# ================================================================
# STANDARDISASI CPUE DENGAN GLM GAMMA
# ================================================================

# 1. PAKET --------------------------------------------------------

paket <- c(
  "readr",
  "dplyr",
  "tidyr",
  "ggplot2",
  "emmeans"
)

belum_ada <- paket[
  !vapply(
    paket,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(belum_ada) > 0) {
  install.packages(
    belum_ada,
    repos = "https://cloud.r-project.org"
  )
}

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(emmeans)


# ================================================================
# 2. INPUT TAHUN
# ================================================================

cat("====================================================\n")
cat("       INPUT PARAMETER ANALISIS CPUE                \n")
cat("====================================================\n")

tahun_user <- trimws(
  readline(
    prompt = "Masukkan Tahun Data (default 2015): "
  )
)

if (tahun_user == "") {
  tahun_input <- "2025"
} else {
  tahun_input <- tahun_user
}

if (!grepl("^[0-9]{4}$", tahun_input)) {
  stop("Tahun harus terdiri dari 4 angka, misalnya 2025.")
}

tahun_angka <- as.integer(tahun_input)

cat("\nTahun yang dipilih:", tahun_input, "\n")


# ================================================================
# 3. MEMILIH FILE CSV
# ================================================================

cat("\n====================================================\n")
cat("             PILIH FILE DATA CSV                    \n")
cat("====================================================\n")

file_data <- choose.files(
  caption = "Pilih file CSV untuk analisis CPUE",
  multi = FALSE
)

if (length(file_data) == 0 || file_data == "") {
  stop("Tidak ada file CSV yang dipilih.")
}

# Cek ekstensi file
ekstensi <- tolower(
  tools::file_ext(file_data)
)

if (ekstensi != "csv") {
  stop("File yang dipilih harus berformat CSV.")
}

cat("\nFile yang dipilih:\n")
cat(file_data, "\n")


# ================================================================
# 4. MEMBUAT FOLDER HASIL
# ================================================================

folder_hasil <- paste0(
  "Hasil_CPUE_Standar_",
  tahun_input
)

if (!dir.exists(folder_hasil)) {
  dir.create(
    folder_hasil,
    recursive = TRUE
  )
}


# ================================================================
# 5. MEMBACA DATA CSV
# ================================================================

cat("\n====================================================\n")
cat("              MEMBACA DATA                          \n")
cat("====================================================\n")

data_raw <- read_csv(
  file_data,
  show_col_types = FALSE,
  name_repair = "unique"
)

cat(
  "Jumlah kolom dalam file:",
  ncol(data_raw),
  "\n"
)

cat(
  "Jumlah baris dalam file:",
  nrow(data_raw),
  "\n"
)


# ================================================================
# 6. MEMERIKSA JUMLAH KOLOM
# ================================================================

if (ncol(data_raw) < 4) {
  stop(
    "File CSV harus memiliki minimal 4 kolom."
  )
}


# ================================================================
# 7. MENGAMBIL 4 KOLOM PERTAMA
# ================================================================

data_cpue <- data_raw[, 1:4]

names(data_cpue) <- c(
  "alat_tangkap",
  "hari_operasi",
  "gt_kapal",
  "berat_ikan_kg"
)


# ================================================================
# 8. MEMBERSIHKAN DATA
# ================================================================

data_cpue <- data_cpue %>%
  mutate(
    alat_tangkap = trimws(
      as.character(alat_tangkap)
    ),
    
    hari_operasi = suppressWarnings(
      as.numeric(hari_operasi)
    ),
    
    gt_kapal = suppressWarnings(
      as.numeric(gt_kapal)
    ),
    
    berat_ikan_kg = suppressWarnings(
      as.numeric(berat_ikan_kg)
    )
  ) %>%
  filter(
    !is.na(alat_tangkap),
    alat_tangkap != "",
    !is.na(hari_operasi),
    hari_operasi > 0,
    !is.na(gt_kapal),
    gt_kapal > 0,
    !is.na(berat_ikan_kg),
    berat_ikan_kg > 0
  )


# ================================================================
# 9. CEK DATA VALID
# ================================================================

if (nrow(data_cpue) == 0) {
  stop(
    "Tidak ada data valid setelah proses pembersihan."
  )
}

jumlah_alat_tangkap <- n_distinct(
  data_cpue$alat_tangkap
)

cat("\nJumlah data valid:",
    nrow(data_cpue),
    "baris\n")

cat(
  "Jumlah jenis alat tangkap:",
  jumlah_alat_tangkap,
  "\n"
)


# ================================================================
# 10. CEK VARIASI GT
# ================================================================

if (n_distinct(data_cpue$gt_kapal) < 2) {
  stop(
    "Nilai GT kapal harus memiliki minimal 2 nilai yang berbeda."
  )
}


# ================================================================
# 11. MEMBUAT VARIABEL CPUE
# ================================================================

data_cpue <- data_cpue %>%
  mutate(
    tahun = tahun_angka,
    
    alat_tangkap = factor(
      alat_tangkap
    ),
    
    cpue_nominal =
      berat_ikan_kg / hari_operasi,
    
    cpue_ton_gt =
      (berat_ikan_kg / 1000) /
      (gt_kapal * hari_operasi),
    
    log_gt = log(gt_kapal)
  )


# ================================================================
# 12. GT ACUAN
# ================================================================

gt_acuan <- exp(
  mean(
    data_cpue$log_gt,
    na.rm = TRUE
  )
)

cat(
  "GT acuan:",
  round(gt_acuan, 3),
  "GT\n"
)


# ================================================================
# 13. MODEL GLM GAMMA
# ================================================================

cat("\n====================================================\n")
cat("             PEMODELAN GLM GAMMA                    \n")
cat("====================================================\n")

if (jumlah_alat_tangkap == 1) {
  
  model_gamma_kg <- glm(
    cpue_nominal ~ log_gt,
    family = Gamma(link = "log"),
    data = data_cpue
  )
  
  model_gamma_ton_gt <- glm(
    cpue_ton_gt ~ log_gt,
    family = Gamma(link = "log"),
    data = data_cpue
  )
  
} else {
  
  model_gamma_kg <- glm(
    cpue_nominal ~ alat_tangkap + log_gt,
    family = Gamma(link = "log"),
    data = data_cpue
  )
  
  model_gamma_ton_gt <- glm(
    cpue_ton_gt ~ alat_tangkap + log_gt,
    family = Gamma(link = "log"),
    data = data_cpue
  )
  
}


# ================================================================
# 14. HASIL EMMEANS
# ================================================================

if (jumlah_alat_tangkap == 1) {
  
  emm_kg <- emmeans(
    model_gamma_kg,
    ~ 1,
    at = list(
      log_gt = log(gt_acuan)
    ),
    type = "response"
  )
  
  emm_ton_gt <- emmeans(
    model_gamma_ton_gt,
    ~ 1,
    at = list(
      log_gt = log(gt_acuan)
    ),
    type = "response"
  )
  
} else {
  
  emm_kg <- emmeans(
    model_gamma_kg,
    ~ alat_tangkap,
    at = list(
      log_gt = log(gt_acuan)
    ),
    type = "response"
  )
  
  emm_ton_gt <- emmeans(
    model_gamma_ton_gt,
    ~ alat_tangkap,
    at = list(
      log_gt = log(gt_acuan)
    ),
    type = "response"
  )
  
}


# ================================================================
# 15. MENAMPILKAN HASIL CPUE
# ================================================================

cat("\n====================================================\n")
cat("       CPUE STANDAR PER ALAT TANGKAP                \n")
cat("====================================================\n")

print(
  summary(
    emm_kg,
    infer = TRUE
  )
)

cat("\n----------------------------------------------------\n")

print(
  summary(
    emm_ton_gt,
    infer = TRUE
  )
)


# ================================================================
# 16. PREDIKSI CPUE
# ================================================================

data_cpue <- data_cpue %>%
  mutate(
    pred_kg = predict(
      model_gamma_kg,
      newdata = data_cpue,
      type = "response"
    ),
    
    pred_ton_gt = predict(
      model_gamma_ton_gt,
      newdata = data_cpue,
      type = "response"
    )
  )


# ================================================================
# 17. RINGKASAN PER ALAT TANGKAP
# ================================================================

ringkasan_alat <- data_cpue %>%
  group_by(alat_tangkap) %>%
  summarise(
    tahun = first(tahun),
    n_sampel = n(),
    cpue_nominal_kg_hari =
      mean(
        cpue_nominal,
        na.rm = TRUE
      ),
    cpue_standar_kg_hari =
      mean(
        pred_kg,
        na.rm = TRUE
      ),
    cpue_standar_ton_gt_hari =
      mean(
        pred_ton_gt,
        na.rm = TRUE
      ),
    .groups = "drop"
  )


# ================================================================
# 18. CPUE GABUNGAN PROPORSIONAL
# ================================================================

if (jumlah_alat_tangkap > 1) {
  
  cpue_proporsional_kg <- mean(
    data_cpue$pred_kg,
    na.rm = TRUE
  )
  
  cpue_proporsional_ton_gt <- mean(
    data_cpue$pred_ton_gt,
    na.rm = TRUE
  )
  
  ringkasan_tahun <- data.frame(
    Tahun = tahun_angka,
    Jumlah_Jenis_Alat = jumlah_alat_tangkap,
    Total_Sampel = nrow(data_cpue),
    GT_Acuan = round(
      gt_acuan,
      3
    ),
    CPUE_Standar_Proporsional_kg_hari =
      round(
        cpue_proporsional_kg,
        4
      ),
    CPUE_Standar_Proporsional_ton_GT_hari =
      round(
        cpue_proporsional_ton_gt,
        7
      )
  )
  
} else {
  
  ringkasan_tahun <- NULL
  
}


# ================================================================
# 19. MENYIMPAN HASIL KE CSV
# ================================================================

write_csv(
  ringkasan_alat,
  file.path(
    folder_hasil,
    paste0(
      "CPUE_Per_Alat_Tangkap_",
      tahun_input,
      ".csv"
    )
  )
)


# ================================================================
# 20. MENYIMPAN CPUE GABUNGAN
# ================================================================

if (jumlah_alat_tangkap > 1) {
  
  write_csv(
    ringkasan_tahun,
    file.path(
      folder_hasil,
      paste0(
        "CPUE_Gabungan_Proporsional_",
        tahun_input,
        ".csv"
      )
    )
  )
  
}


# ================================================================
# 21. MENAMPILKAN HASIL AKHIR
# ================================================================

cat("\n")
cat("====================================================\n")
cat("                  HASIL AKHIR                       \n")
cat("====================================================\n")

print(ringkasan_alat)

if (jumlah_alat_tangkap > 1) {
  
  cat("\n====================================================\n")
  cat("          CPUE GABUNGAN PROPORSIONAL                \n")
  cat("====================================================\n")
  
  print(ringkasan_tahun)
  
}

cat("\n====================================================\n")
cat("PROSES SELESAI\n")
cat("====================================================\n")

cat(
  "Folder hasil:",
  normalizePath(folder_hasil),
  "\n"
)

