# R/batch_helpers.R
# Helper functions for batch CMSY++ analysis: aggregation, classification, narrative

# ============================================================
# CLASSIFICATION FUNCTIONS
# ============================================================

classify_stock_status <- function(bbmsy, ffmsy) {
  if (is.na(bbmsy) || is.na(ffmsy)) return("Unknown")
  if (bbmsy >= 0.8 && bbmsy <= 1.2 && ffmsy <= 1.0) return("Healthy")
  if (bbmsy < 0.5) return("Depleted")
  if (bbmsy < 0.8 && ffmsy <= 1.0) return("Recovering")
  if (bbmsy >= 0.5 && bbmsy < 0.8 && ffmsy > 1.0) return("Overfished")
  if (bbmsy >= 0.8 && ffmsy > 1.0) return("Overfishing")
  if (bbmsy > 1.2) return("Underfished")
  return("Unknown")
}

classify_all <- function(results_df) {
  sapply(1:nrow(results_df), function(i) {
    classify_stock_status(results_df$last.B_Bmsy[i], results_df$last.F_Fmsy[i])
  })
}

classify_status_confidence <- function(bbmsy, bbmsy_low, bbmsy_high,
                                       ffmsy, ffmsy_low, ffmsy_high) {
  values <- c(bbmsy, bbmsy_low, bbmsy_high, ffmsy, ffmsy_low, ffmsy_high)
  if (any(!is.finite(values))) return("Unknown")
  corner_status <- unique(c(
    classify_stock_status(bbmsy_low, ffmsy_low),
    classify_stock_status(bbmsy_low, ffmsy_high),
    classify_stock_status(bbmsy_high, ffmsy_low),
    classify_stock_status(bbmsy_high, ffmsy_high)
  ))
  if (length(corner_status) == 1 && identical(corner_status, classify_stock_status(bbmsy, ffmsy)))
    "Robust" else "Uncertain"
}

status_color <- function(status) {
  switch(status,
    "Healthy"    = "#10b981",
    "Recovering" = "#f59e0b",
    "Overfished" = "#ef4444",
    "Overfishing"= "#f97316",
    "Depleted"   = "#dc2626",
    "Underfished"= "#3b82f6",
    "#6b7280"
  )
}

status_icon <- function(status) {
  switch(status,
    "Healthy"    = "check-circle",
    "Recovering" = "arrow-up",
    "Overfished" = "exclamation-triangle",
    "Overfishing"= "exclamation-circle",
    "Depleted"   = "times-circle",
    "Underfished"= "arrow-down",
    "question-circle"
  )
}

# ============================================================
# AGGREGATION: Per-Group Statistics
# ============================================================

compute_group_stats <- function(species_results_list, group_name) {
  # Filter results that belong to this group
  group_res <- Filter(function(x) !is.null(x) && x$group == group_name, species_results_list)
  if (length(group_res) == 0) return(NULL)

  # Extract vectors
  msy_vals   <- sapply(group_res, function(x) x$MSY)
  bbmsy_vals <- sapply(group_res, function(x) x$last.B_Bmsy)
  ffmsy_vals <- sapply(group_res, function(x) x$last.F_Fmsy)
  r_vals     <- sapply(group_res, function(x) x$r)
  k_vals     <- sapply(group_res, function(x) x$k)
  n_years    <- sapply(group_res, function(x) x$n_years)

  # Remove NAs for stats
  msy_clean   <- msy_vals[!is.na(msy_vals)]
  bbmsy_clean <- bbmsy_vals[!is.na(bbmsy_vals)]
  ffmsy_clean <- ffmsy_vals[!is.na(ffmsy_vals)]

  # Status classification per species
  statuses <- sapply(group_res, function(x) classify_stock_status(x$last.B_Bmsy, x$last.F_Fmsy))
  status_table <- table(statuses)
  total_n <- length(group_res)
  status_n <- function(name) {
    if (name %in% names(status_table)) as.numeric(status_table[[name]]) else 0
  }

  # Weighted mean (by time series length) — re-normalize weights after removing NAs
  bbmsy_valid <- !is.na(bbmsy_vals)
  ffmsy_valid <- !is.na(ffmsy_vals)
  if (sum(bbmsy_valid) > 0) {
    w_b <- n_years[bbmsy_valid] / sum(n_years[bbmsy_valid])
    weighted_mean_bbmsy <- sum(bbmsy_vals[bbmsy_valid] * w_b)
  } else {
    weighted_mean_bbmsy <- NA_real_
  }
  if (sum(ffmsy_valid) > 0) {
    w_f <- n_years[ffmsy_valid] / sum(n_years[ffmsy_valid])
    weighted_mean_ffmsy <- sum(ffmsy_vals[ffmsy_valid] * w_f)
  } else {
    weighted_mean_ffmsy <- NA_real_
  }

  list(
    group          = group_name,
    n_species      = total_n,
    species_names  = sapply(group_res, function(x) x$stock),
    msy = list(
      values       = msy_vals,
      total        = if (length(msy_clean) > 0) sum(msy_clean) else 0,
      median       = if (length(msy_clean) > 0) median(msy_clean) else NA_real_,
      mean         = if (length(msy_clean) > 0) mean(msy_clean) else NA_real_,
      q25          = if (length(msy_clean) > 0) quantile(msy_clean, 0.25, na.rm = TRUE) else NA_real_,
      q75          = if (length(msy_clean) > 0) quantile(msy_clean, 0.75, na.rm = TRUE) else NA_real_,
      min          = if (length(msy_clean) > 0) min(msy_clean) else NA_real_,
      max          = if (length(msy_clean) > 0) max(msy_clean) else NA_real_
    ),
    bbmsy = list(
      values       = bbmsy_vals,
      median       = if (length(bbmsy_clean) > 0) median(bbmsy_clean) else NA_real_,
      mean         = if (length(bbmsy_clean) > 0) mean(bbmsy_clean) else NA_real_,
      weighted_mean= weighted_mean_bbmsy,
      q25          = if (length(bbmsy_clean) > 0) quantile(bbmsy_clean, 0.25, na.rm = TRUE) else NA_real_,
      q75          = if (length(bbmsy_clean) > 0) quantile(bbmsy_clean, 0.75, na.rm = TRUE) else NA_real_
    ),
    ffmsy = list(
      values       = ffmsy_vals,
      median       = if (length(ffmsy_clean) > 0) median(ffmsy_clean) else NA_real_,
      mean         = if (length(ffmsy_clean) > 0) mean(ffmsy_clean) else NA_real_,
      weighted_mean= weighted_mean_ffmsy,
      q25          = if (length(ffmsy_clean) > 0) quantile(ffmsy_clean, 0.25, na.rm = TRUE) else NA_real_,
      q75          = if (length(ffmsy_clean) > 0) quantile(ffmsy_clean, 0.75, na.rm = TRUE) else NA_real_
    ),
    r = list(values = r_vals, median = if (length(r_vals[!is.na(r_vals)]) > 0) median(r_vals, na.rm = TRUE) else NA_real_),
    k = list(values = k_vals, median = if (length(k_vals[!is.na(k_vals)]) > 0) median(k_vals, na.rm = TRUE) else NA_real_),
    status = list(
      table         = as.list(status_table),
      healthy_pct   = status_n("Healthy") / total_n * 100,
      depleted_pct  = status_n("Depleted") / total_n * 100,
      overfishing_pct = (status_n("Overfishing") + status_n("Overfished")) / total_n * 100,
      recovering_pct  = status_n("Recovering") / total_n * 100,
      underfished_pct = status_n("Underfished") / total_n * 100
    )
  )
}

# ============================================================
# NARRATIVE GENERATOR
# ============================================================

generate_group_narrative <- function(group_stats) {
  if (is.null(group_stats)) return("No data available.")

  n <- group_stats$n_species
  g <- group_stats$group

  msy_total <- group_stats$msy$total
  msy_med   <- group_stats$msy$median
  bbmsy_med <- group_stats$bbmsy$median
  ffmsy_med <- group_stats$ffmsy$median

  healthy_n   <- round(group_stats$status$healthy_pct / 100 * n)
  depleted_n  <- round(group_stats$status$depleted_pct / 100 * n)
  overfish_n  <- round(group_stats$status$overfishing_pct / 100 * n)
  recover_n   <- round(group_stats$status$recovering_pct / 100 * n)

  # Status dominant
  pct_healthy <- group_stats$status$healthy_pct
  pct_depleted <- group_stats$status$depleted_pct
  pct_over <- group_stats$status$overfishing_pct

  if (pct_healthy >= 50) {
    overall <- "kondisi relatif baik"
  } else if (pct_depleted >= 40) {
    overall <- "kondisi kritis"
  } else if (pct_over >= 40) {
    overall <- "pemanfaatan berlebihan"
  } else {
    overall <- "kondisi bervariasi"
  }

  # F/Fmsy interpretation
  if (ffmsy_med > 1.0) {
    ffmsy_interp <- paste0("rata-rata pemanfaatan melebihi tingkat lestari (F/Fmsy = ", 
                           round(ffmsy_med, 2), "), menunjukkan tekanan eksploitasi yang tinggi")
  } else if (ffmsy_med > 0.8) {
    ffmsy_interp <- paste0("rata-rata pemanfaatan mendekati tingkat lestari (F/Fmsy = ", 
                           round(ffmsy_med, 2), "), perlu monitoring ketat")
  } else {
    ffmsy_interp <- paste0("rata-rata pemanfaatan masih di bawah tingkat lestari (F/Fmsy = ", 
                           round(ffmsy_med, 2), ")")
  }

  narrative <- paste0(
    g, " (", n, " spesies):\n\n",
    "Kondisi Umum: Kelompok ", g, " menunjukkan ", overall, ". ",
    "Dari ", n, " spesies yang dianalisis, ", healthy_n, " spesies (", 
    round(pct_healthy, 1), "%) dalam kondisi sehat, ",
    depleted_n, " spesies (", round(pct_depleted, 1), "%) terdepleksi, ",
    overfish_n, " spesies (", round(pct_over, 1), "%) mengalami overfishing",
    if (recover_n > 0) paste0(", dan ", recover_n, " spesies (", round(group_stats$status$recovering_pct, 1), "%) dalam pemulihan") else "",
    ".\n\n",
    "Total MSY potensial kelompok: ", format(round(msy_total, 0), big.mark = ","), " ton/tahun ",
    "(median per spesies: ", format(round(msy_med, 0), big.mark = ","), " ton/tahun).\n\n",
    "Biomass relatif (median B/Bmsy): ", round(bbmsy_med, 2), ". ",
    ffmsy_interp, ".\n\n",
    "Rekomendasi: ",
    if (pct_depleted >= 30) "Beberapa spesies membutuhkan pengurangan upaya penangkapan dan langkah pemulihan stok. " else "",
    if (pct_healthy >= 50) "Sebagian besar spesies dapat dipertahankan pada level pemanfaatan saat ini. " else "",
    "Evaluasi berkala secara individual per spesies diperlukan untuk memastikan kelestarian."
  )

  return(narrative)
}

# ============================================================
# TABLE BUILDERS
# ============================================================

build_species_table <- function(species_results_list) {
  rows <- lapply(species_results_list, function(x) {
    if (is.null(x)) return(NULL)
    data.frame(
      Stock       = x$stock,
      Group       = x$group,
      MSY         = round(x$MSY, 3),
      r           = round(x$r, 4),
      k           = round(x$k, 3),
      B_Bmsy      = round(x$last.B_Bmsy, 3),
      B_Bmsy_LCL  = round(x$lcl.last.B_Bmsy, 3),
      B_Bmsy_UCL  = round(x$ucl.last.B_Bmsy, 3),
      F_Fmsy      = round(x$last.F_Fmsy, 3),
      F_Fmsy_LCL  = round(x$lcl.last.F_Fmsy, 3),
      F_Fmsy_UCL  = round(x$ucl.last.F_Fmsy, 3),
      Status      = classify_stock_status(x$last.B_Bmsy, x$last.F_Fmsy),
      Confidence  = classify_status_confidence(x$last.B_Bmsy, x$lcl.last.B_Bmsy,
                                               x$ucl.last.B_Bmsy, x$last.F_Fmsy,
                                               x$lcl.last.F_Fmsy, x$ucl.last.F_Fmsy),
      n_years     = x$n_years,
      Resilience  = x$resilience,
      btype       = x$btype,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

build_group_comparison_table <- function(group_stats_list) {
  rows <- lapply(group_stats_list, function(gs) {
    if (is.null(gs)) return(NULL)
    data.frame(
      Group            = gs$group,
      N_Species        = gs$n_species,
      Total_MSY        = round(gs$msy$total, 1),
      Median_MSY       = round(gs$msy$median, 1),
      Mean_MSY         = round(gs$msy$mean, 1),
      Median_B_Bmsy    = round(gs$bbmsy$median, 3),
      Weighted_B_Bmsy  = round(gs$bbmsy$weighted_mean, 3),
      Median_F_Fmsy    = round(gs$ffmsy$median, 3),
      Weighted_F_Fmsy  = round(gs$ffmsy$weighted_mean, 3),
      Pct_Healthy      = round(gs$status$healthy_pct, 1),
      Pct_Depleted     = round(gs$status$depleted_pct, 1),
      Pct_Overfishing  = round(gs$status$overfishing_pct, 1),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

# ============================================================
# VALIDATION HELPERS
# ============================================================

validate_single_stock <- function(catch_df, id_df, stock) {
  checks <- list()

  add <- function(name, status, rule, msg = "") {
    checks[[length(checks) + 1]] <<- list(
      Stock = stock, Check = name, Status = status, Rule = rule, Details = msg
    )
  }

  # Stock exists in both and exactly once in metadata.
  c_sub <- catch_df[catch_df$Stock == stock, ]
  i_sub <- id_df[id_df$Stock == stock, ]

  if (nrow(c_sub) == 0) {
    add("Catch Data", "ERROR", "Stok harus ada di file catch", "Tidak ditemukan di catch file")
    return(checks)
  }
  if (nrow(i_sub) == 0) {
    add("ID Data", "ERROR", "Stok harus ada di file ID", "Tidak ditemukan di ID file")
    return(checks)
  }
  if (nrow(i_sub) > 1) {
    add("ID Uniqueness", "ERROR", "Tepat satu baris metadata per stok",
        paste(nrow(i_sub), "baris ditemukan"))
    return(checks)
  }

  add("Catch Data", "PASS", "Data catch tersedia", paste(nrow(c_sub), "tahun"))
  add("ID Data", "PASS", "Metadata tersedia", "")

  # Numeric domain checks. JAGS models catch with log(ct), therefore zero is invalid.
  yrs <- suppressWarnings(as.numeric(c_sub$yr))
  ct <- suppressWarnings(as.numeric(c_sub$ct))
  if (any(!is.finite(yrs)) || any(yrs != floor(yrs), na.rm = TRUE)) {
    add("Year Type", "ERROR", "Tahun harus bilangan bulat terbatas", "Nilai tahun invalid")
    return(checks)
  }
  if (any(yrs < 1950 | yrs > 2030)) {
    add("Year Range", "ERROR", "Engine mendukung tahun 1950-2030",
        paste(range(yrs), collapse = " - "))
  } else {
    add("Year Range", "PASS", "1950 <= tahun <= 2030", paste(range(yrs), collapse = " - "))
  }
  if (anyDuplicated(yrs)) {
    add("Stock-Year Uniqueness", "ERROR", "Satu observasi per stok-tahun",
        paste("Duplikat:", paste(unique(yrs[duplicated(yrs)]), collapse = ", ")))
  } else {
    add("Stock-Year Uniqueness", "PASS", "Satu observasi per stok-tahun", "")
  }
  yrs <- sort(yrs)
  if (length(yrs) > 1) {
    gaps <- diff(yrs)
    if (any(gaps != 1)) {
      add("Year Continuity", "ERROR", "Tahun harus berurutan", paste("Gap ditemukan:", paste(yrs[which(gaps != 1)], collapse = ", ")))
    } else {
      add("Year Continuity", "PASS", "Tahun berurutan", paste(min(yrs), "-", max(yrs)))
    }
  } else {
    add("Year Continuity", "ERROR", "Minimal 2 tahun data", "Hanya 1 tahun")
  }

  if (length(ct) < 6) {
    add("Series Length", "ERROR", "Minimal 6 tahun untuk retrospective 3-step",
        paste("Hanya", length(ct), "tahun"))
  } else {
    add("Series Length", "PASS", "Minimal 6 tahun", paste(length(ct), "tahun"))
  }
  if (any(!is.finite(ct))) {
    add("Catch Complete", "ERROR", "Catch harus numerik dan terbatas", "Ada NA/NaN/Inf/non-numerik")
  } else {
    add("Catch Complete", "PASS", "Catch lengkap", "")
  }
  if (any(ct <= 0, na.rm = TRUE)) {
    add("Catch Range", "ERROR", "Catch > 0 karena likelihood memakai log(catch)", "Catch nol/negatif ditemukan")
  } else {
    add("Catch Range", "PASS", "Catch positif", "")
  }
  if (length(ct) > 1 && all(is.finite(ct)) && all(ct > 0)) {
    adjacent_fold <- max(pmax(ct[-1] / ct[-length(ct)], ct[-length(ct)] / ct[-1]))
    if (adjacent_fold > 10) {
      add("Catch Discontinuity", "WARNING", "Perubahan tahunan idealnya diverifikasi jika >10x",
          paste("Maksimum", round(adjacent_fold, 1), "kali"))
    } else {
      add("Catch Discontinuity", "PASS", "Tidak ada lonjakan >10x", paste(round(adjacent_fold, 1), "kali"))
    }
  }
  add("Catch Unit", "WARNING", "Batch WPP711 mengasumsikan ct dalam kg",
      "Runner mengonversi kg ke ton sebelum CMSY++; verifikasi unit sumber")

  # BT if required
  meta_chr <- function(name) {
    if (!name %in% names(i_sub)) return(NA_character_)
    as.character(i_sub[[name]][1])
  }
  btype <- tolower(trimws(meta_chr("btype")))
  if (btype %in% c("cpue", "biomass")) {
    if (!"bt" %in% names(c_sub)) {
      add("Abundance Index", "ERROR", paste("BT wajib jika btype =", btype), "Kolom bt tidak ada")
    } else {
      bt <- suppressWarnings(as.numeric(c_sub$bt))
      valid_bt <- is.finite(bt) & bt > 0
      if (sum(valid_bt) < 3) {
      add("Abundance Index", "ERROR", paste("BT wajib jika btype =", btype), "Semua BT = NA")
      } else if (any(is.finite(bt) & bt <= 0)) {
        add("Abundance Index", "ERROR", "BT > 0", "BT nol/negatif")
      } else {
        add("Abundance Index", "PASS", "Minimal 3 BT positif", paste(sum(valid_bt), "nilai tersedia"))
      }
    }
  } else if (btype == "none") {
    add("Abundance Index", "PASS", "btype = None, BT diabaikan", "")
  } else {
    add("Abundance Index", "ERROR", "btype harus None, CPUE, atau biomass", paste("Invalid:", btype))
  }

  # Resilience
  res <- tolower(trimws(meta_chr("Resilience")))
  if (!(res %in% c("high", "medium", "low", "very low"))) {
    add("Resilience", "ERROR", "High/Medium/Low/Very low", paste("Invalid:", res))
  } else {
    add("Resilience", "PASS", "Valid", res)
  }

  # B/k priors
  meta_num <- function(name) {
    if (!name %in% names(i_sub)) return(NA_real_)
    suppressWarnings(as.numeric(i_sub[[name]][1]))
  }
  stb_l <- meta_num("stb.low")
  stb_h <- meta_num("stb.hi")
  endb_l <- meta_num("endb.low")
  endb_h <- meta_num("endb.hi")

  if (is.na(stb_l) && is.na(stb_h)) {
    add("Prior B/k Start", "WARNING", "Prior otomatis dipakai", "Belum ada prior ahli")
  } else if (xor(is.na(stb_l), is.na(stb_h))) {
    add("Prior B/k Start", "ERROR", "Isi kedua batas atau kosongkan keduanya", "")
  } else if (!is.na(stb_l) && !is.na(stb_h) && stb_l >= stb_h) {
    add("Prior B/k Start", "ERROR", "stb.low < stb.hi", "")
  } else if (!is.na(stb_l) && (stb_l < 0 || stb_l > 1)) {
    add("Prior B/k Start", "ERROR", "stb dalam 0-1", "")
  } else {
    add("Prior B/k Start", "PASS", "Valid", paste(stb_l, "-", stb_h))
  }

  if (is.na(endb_l) && is.na(endb_h)) {
    add("Prior B/k End", "WARNING", "Prior otomatis dipakai", "Belum ada prior ahli")
  } else if (xor(is.na(endb_l), is.na(endb_h))) {
    add("Prior B/k End", "ERROR", "Isi kedua batas atau kosongkan keduanya", "")
  } else if (!is.na(endb_l) && !is.na(endb_h) && endb_l >= endb_h) {
    add("Prior B/k End", "ERROR", "endb.low < endb.hi", "")
  } else if (!is.na(endb_l) && (endb_l < 0 || endb_h > 1)) {
    add("Prior B/k End", "ERROR", "endb dalam 0-1", "")
  } else {
    add("Prior B/k End", "PASS", "Valid", paste(endb_l, "-", endb_h))
  }

  return(checks)
}

# Normalize one stock into the exact schema/casing consumed by CMSY++16.R.
prepare_stock_inputs <- function(catch_df, id_df, stock, overrides = list(), catch_unit = "kg") {
  stk_catch <- catch_df[catch_df$Stock == stock, , drop = FALSE]
  stk_id <- id_df[id_df$Stock == stock, , drop = FALSE]
  if (nrow(stk_catch) == 0 || nrow(stk_id) != 1) {
    stop("Stock must have catch data and exactly one ID row: ", stock)
  }
  stk_catch$yr <- as.numeric(stk_catch$yr)
  stk_catch$ct <- as.numeric(stk_catch$ct)
  catch_unit <- tolower(trimws(catch_unit))
  if (!catch_unit %in% c("kg", "tonnes", "ton")) stop("catch_unit must be kg or tonnes")
  # CMSY++16.R expects tonnes and internally converts them to '000 tonnes.
  if (catch_unit == "kg") stk_catch$ct <- stk_catch$ct / 1000
  stk_catch <- stk_catch[order(stk_catch$yr), , drop = FALSE]
  catch_yrs <- stk_catch$yr

  fill <- function(df, col, value) {
    if (!col %in% names(df)) df[[col]] <- value
    missing <- is.na(df[[col]]) | trimws(as.character(df[[col]])) == ""
    df[[col]][missing] <- value
    df
  }
  stk_id <- fill(stk_id, "StartYear", min(catch_yrs))
  stk_id <- fill(stk_id, "EndYear", max(catch_yrs))
  stk_id <- fill(stk_id, "MinOfYear", min(catch_yrs))
  stk_id <- fill(stk_id, "MaxOfYear", max(catch_yrs))
  stk_id <- fill(stk_id, "Resilience", "Medium")
  stk_id <- fill(stk_id, "btype", "None")
  stk_id <- fill(stk_id, "e.creep", 2)
  stk_id <- fill(stk_id, "force.cmsy", FALSE)
  for (nm in c("r.low", "r.hi", "stb.low", "stb.hi", "int.yr", "intb.low",
               "intb.hi", "endb.low", "endb.hi")) stk_id <- fill(stk_id, nm, NA_real_)

  res_key <- tolower(trimws(as.character(stk_id$Resilience[1])))
  stk_id$Resilience <- unname(c(high = "High", medium = "Medium", low = "Low",
                                `very low` = "Very low")[res_key])
  type_key <- tolower(trimws(as.character(stk_id$btype[1])))
  stk_id$btype <- unname(c(none = "None", cpue = "CPUE", biomass = "biomass")[type_key])
  if (is.na(stk_id$Resilience[1]) || is.na(stk_id$btype[1])) stop("Invalid metadata for ", stock)

  defaults <- list(Continent = "Asia", Region = "Indonesia", Subregion = "WPP-711",
                   Name = stock, Source = "Batch import", Comment = "",
                   ScientificName = stock, SpecCode = NA, Flim = NA, Fpa = NA,
                   Blim = NA, Bpa = NA, Bmsy = NA, MSYBtrigger = NA, Fmsy = NA,
                   last_F = NA)
  for (nm in names(defaults)) stk_id <- fill(stk_id, nm, defaults[[nm]])
  if (!is.null(overrides$e_creep) && is.finite(as.numeric(overrides$e_creep)))
    stk_id$e.creep <- as.numeric(overrides$e_creep)
  if (!is.null(overrides$force_cmsy) && !is.na(overrides$force_cmsy))
    stk_id$force.cmsy <- as.logical(overrides$force_cmsy)
  list(catch = stk_catch, id = stk_id, source_catch_unit = catch_unit,
       engine_catch_unit = "tonnes")
}

# ============================================================
# `%||%` NULL COALESCING OPERATOR
# ============================================================
`%||%` <- function(a, b) if (!is.null(a)) a else b
