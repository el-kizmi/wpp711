# Helpers for building an annual standardized CPUE index from operation-level data.

cpue_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

canonicalize_cpue_data <- function(df, default_stock = NULL, default_year = NULL) {
  if (!is.data.frame(df) || nrow(df) == 0) stop("Data CPUE kosong.")

  original <- names(df)
  normalized <- make.unique(cpue_key(original), sep = "_")
  names(df) <- normalized

  aliases <- list(
    Stock = c("stock", "stok", "species", "spesies", "nama_ikan", "jenis_ikan"),
    yr = c("yr", "year", "tahun"),
    gear = c("nama_alat_tangkap", "alat_tangkap", "gear", "fishing_gear"),
    effort = c("jumlah_hari_operasi", "hari_operasi", "effort", "days", "days_fished"),
    gt = c("gt_kapal", "gt", "gross_tonnage", "ukuran_gt"),
    catch_kg = c("berat_ikan_kg", "berat_ikan", "catch_kg", "catch", "hasil_tangkapan_kg")
  )

  used <- character(0)
  for (target in names(aliases)) {
    hit <- aliases[[target]][aliases[[target]] %in% names(df)]
    if (length(hit)) {
      src <- hit[1]
      names(df)[names(df) == src] <- target
      used <- c(used, target)
    }
  }

  if (!"Stock" %in% names(df) && !is.null(default_stock) && nzchar(default_stock)) {
    df$Stock <- default_stock
  }
  if (!"yr" %in% names(df) && !is.null(default_year) && is.finite(default_year)) {
    df$yr <- as.integer(default_year)
  }

  required <- c("Stock", "yr", "gear", "effort", "gt", "catch_kg")
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    stop("Kolom/metadata wajib belum tersedia: ", paste(missing, collapse = ", "), ".")
  }

  df$Stock <- trimws(as.character(df$Stock))
  df$yr <- suppressWarnings(as.integer(df$yr))
  df$gear <- trimws(as.character(df$gear))
  df$effort <- suppressWarnings(as.numeric(df$effort))
  df$gt <- suppressWarnings(as.numeric(df$gt))
  df$catch_kg <- suppressWarnings(as.numeric(df$catch_kg))

  valid <- nzchar(df$Stock) & nzchar(df$gear) & is.finite(df$yr) &
    is.finite(df$effort) & df$effort > 0 & is.finite(df$gt) & df$gt > 0 &
    is.finite(df$catch_kg) & df$catch_kg >= 0
  removed <- sum(!valid)
  df <- df[valid, , drop = FALSE]
  if (!nrow(df)) stop("Tidak ada observasi CPUE valid setelah pemeriksaan nilai dan unit.")

  list(
    data = df,
    removed = removed,
    optional = setdiff(names(df), required)
  )
}

cpue_model_matrix <- function(model, newdata) {
  x <- model.matrix(delete.response(terms(model)), newdata,
                    contrasts.arg = model$contrasts)
  beta <- coef(model)
  keep <- names(beta)[is.finite(beta)]
  if (!length(keep)) stop("Model tidak memiliki koefisien yang dapat digunakan.")
  list(
    x = x[, keep, drop = FALSE],
    beta = beta[keep],
    vcov = vcov(model)[keep, keep, drop = FALSE]
  )
}

cpue_delta_prediction <- function(positive_model, encounter_model, newdata) {
  pos <- cpue_model_matrix(positive_model, newdata)
  eta_pos <- as.vector(pos$x %*% pos$beta)
  mu <- exp(eta_pos)

  if (is.null(encounter_model)) {
    estimate <- mean(mu)
    gradient <- colMeans(mu * pos$x)
    variance <- as.numeric(t(gradient) %*% pos$vcov %*% gradient)
  } else {
    enc <- cpue_model_matrix(encounter_model, newdata)
    eta_enc <- as.vector(enc$x %*% enc$beta)
    probability <- plogis(eta_enc)
    combined <- probability * mu
    estimate <- mean(combined)
    gradient_pos <- colMeans(combined * pos$x)
    gradient_enc <- colMeans((mu * probability * (1 - probability)) * enc$x)
    variance <- as.numeric(t(gradient_pos) %*% pos$vcov %*% gradient_pos) +
      as.numeric(t(gradient_enc) %*% enc$vcov %*% gradient_enc)
  }

  if (!is.finite(variance) || variance < 0) variance <- NA_real_
  c(estimate = estimate, se = sqrt(variance))
}

standardize_cpue_stock <- function(df, optional_covariates = character(0)) {
  stock <- unique(df$Stock)
  if (length(stock) != 1) stop("Pemodelan CPUE harus dilakukan per stok.")

  df$year_f <- factor(df$yr, levels = sort(unique(df$yr)))
  df$gear_f <- factor(df$gear)
  df$log_gt <- log(df$gt)
  df$cpue <- df$catch_kg / df$effort
  df$encounter <- as.integer(df$catch_kg > 0)

  optional_covariates <- intersect(optional_covariates, names(df))
  optional_covariates <- setdiff(optional_covariates,
                                 c("Stock", "yr", "gear", "effort", "gt", "catch_kg",
                                   "year_f", "gear_f", "log_gt", "cpue", "encounter"))
  accepted_optional <- character(0)
  warnings <- character(0)
  for (nm in optional_covariates) {
    values <- df[[nm]]
    if (is.character(values) || is.factor(values)) {
      values <- trimws(as.character(values))
      values[!nzchar(values)] <- NA_character_
      values <- factor(values)
      if (nlevels(values) > 30) {
        warnings <- c(warnings, paste("Kovariat", nm, "diabaikan karena memiliki lebih dari 30 level."))
        next
      }
      df[[nm]] <- values
    } else {
      df[[nm]] <- suppressWarnings(as.numeric(values))
    }
    if (length(unique(df[[nm]][!is.na(df[[nm]])])) >= 2) {
      accepted_optional <- c(accepted_optional, nm)
    } else {
      warnings <- c(warnings, paste("Kovariat", nm, "diabaikan karena tidak memiliki cukup variasi."))
    }
  }

  model_columns <- c("cpue", "encounter", "year_f", accepted_optional)
  if (nlevels(df$gear_f) > 1) model_columns <- c(model_columns, "gear_f")
  if (length(unique(df$log_gt)) > 1) model_columns <- c(model_columns, "log_gt")
  complete <- complete.cases(df[, unique(model_columns), drop = FALSE])
  if (sum(!complete)) warnings <- c(warnings, paste(sum(!complete), "baris diabaikan karena kovariat kosong."))
  model_data <- df[complete, , drop = FALSE]

  if (sum(model_data$encounter == 1) < 8) stop("Observasi tangkapan positif terlalu sedikit untuk GLM Gamma.")

  predictors <- character(0)
  if (nlevels(model_data$year_f) > 1) predictors <- c(predictors, "year_f")
  if (nlevels(droplevels(model_data$gear_f)) > 1) predictors <- c(predictors, "gear_f")
  if (length(unique(model_data$log_gt)) > 1) predictors <- c(predictors, "log_gt")
  predictors <- c(predictors, accepted_optional)
  rhs <- if (length(predictors)) paste(predictors, collapse = " + ") else "1"

  positive_model <- tryCatch(
    glm(as.formula(paste("cpue ~", rhs)), family = Gamma(link = "log"),
        data = model_data[model_data$encounter == 1, , drop = FALSE]),
    error = function(e) stop("GLM Gamma gagal: ", conditionMessage(e))
  )
  if (!isTRUE(positive_model$converged)) warnings <- c(warnings, "GLM Gamma tidak mencapai konvergensi penuh.")

  has_zeros <- any(model_data$encounter == 0)
  encounter_model <- NULL
  if (has_zeros && length(unique(model_data$encounter)) == 2) {
    encounter_model <- tryCatch(
      glm(as.formula(paste("encounter ~", rhs)), family = binomial(link = "logit"), data = model_data),
      error = function(e) NULL
    )
    if (is.null(encounter_model) || !isTRUE(encounter_model$converged)) {
      stop("Model encounter untuk tangkapan nol tidak konvergen; periksa pemisahan sempurna atau jumlah sampel.")
    }
  }

  years <- sort(unique(model_data$yr))
  predictions <- lapply(years, function(year) {
    reference <- model_data
    reference$yr <- year
    reference$year_f <- factor(rep(year, nrow(reference)), levels = levels(model_data$year_f))
    pred <- cpue_delta_prediction(positive_model, encounter_model, reference)
    actual <- model_data[model_data$yr == year, , drop = FALSE]
    data.frame(
      Stock = stock,
      yr = year,
      raw_index = unname(pred["estimate"]),
      raw_se = unname(pred["se"]),
      n = nrow(actual),
      zero_rate = mean(actual$catch_kg == 0),
      stringsAsFactors = FALSE
    )
  })
  index <- do.call(rbind, predictions)
  if (any(!is.finite(index$raw_index)) || any(index$raw_index <= 0)) stop("Prediksi indeks CPUE tidak valid.")

  normalizer <- exp(mean(log(index$raw_index)))
  index$bt <- index$raw_index / normalizer
  index$se <- index$raw_se / normalizer
  index$cv <- index$se / index$bt
  log_se <- sqrt(log1p(index$cv^2))
  index$lcl <- exp(log(index$bt) - 1.96 * log_se)
  index$ucl <- exp(log(index$bt) + 1.96 * log_se)
  index$model <- if (has_zeros) "Delta-Gamma" else "Gamma-log"
  index <- index[, c("Stock", "yr", "bt", "se", "cv", "lcl", "ucl", "n", "zero_rate", "model")]

  diagnostics <- data.frame(
    Stock = stock,
    years = length(years),
    observations = nrow(model_data),
    positive = sum(model_data$encounter == 1),
    zero_rate = mean(model_data$encounter == 0),
    gear_levels = nlevels(droplevels(model_data$gear_f)),
    model = if (has_zeros) "Delta-Gamma" else "Gamma-log",
    gamma_deviance_explained = if (positive_model$null.deviance > 0)
      1 - positive_model$deviance / positive_model$null.deviance else NA_real_,
    recommended_cv = median(index$cv[is.finite(index$cv)], na.rm = TRUE),
    ready_for_bsm = length(years) >= 3,
    formula = paste("CPUE ~", rhs),
    stringsAsFactors = FALSE
  )
  if (length(years) < 3) warnings <- c(warnings, "Indeks memiliki kurang dari 3 tahun dan belum dapat mengaktifkan BSM.")

  list(index = index, diagnostics = diagnostics, warnings = warnings,
       positive_model = positive_model, encounter_model = encounter_model)
}

standardize_cpue_dataset <- function(df, default_stock = NULL, default_year = NULL,
                                     optional_covariates = character(0)) {
  prepared <- canonicalize_cpue_data(df, default_stock, default_year)
  results <- list()
  errors <- character(0)
  warnings <- if (prepared$removed > 0) paste(prepared$removed, "baris tidak valid diabaikan.") else character(0)

  for (stock in unique(prepared$data$Stock)) {
    fit <- tryCatch(
      standardize_cpue_stock(prepared$data[prepared$data$Stock == stock, , drop = FALSE],
                             optional_covariates),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      errors <- c(errors, paste0(stock, ": ", conditionMessage(fit)))
    } else {
      results[[stock]] <- fit
      warnings <- c(warnings, paste0(stock, ": ", fit$warnings))
    }
  }
  if (!length(results)) stop(paste(errors, collapse = "\n"))

  list(
    index = do.call(rbind, lapply(results, `[[`, "index")),
    diagnostics = do.call(rbind, lapply(results, `[[`, "diagnostics")),
    warnings = warnings[nzchar(warnings)],
    errors = errors,
    cleaned = prepared$data,
    optional = prepared$optional,
    fits = results
  )
}

read_abundance_index <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- make.unique(cpue_key(names(df)), sep = "_")
  aliases <- list(stock = c("stock", "stok"), yr = c("yr", "year", "tahun"),
                  bt = c("bt", "cpue", "indeks", "index", "biomass", "biomassa"))
  for (target in names(aliases)) {
    hit <- aliases[[target]][aliases[[target]] %in% names(df)]
    if (length(hit)) names(df)[names(df) == hit[1]] <- target
  }
  missing <- setdiff(c("stock", "yr", "bt"), names(df))
  if (length(missing)) stop("File indeks harus memiliki Stock, yr, dan bt.")
  out <- data.frame(
    Stock = trimws(as.character(df$stock)),
    yr = suppressWarnings(as.integer(df$yr)),
    bt = suppressWarnings(as.numeric(df$bt)),
    stringsAsFactors = FALSE
  )
  valid <- nzchar(out$Stock) & is.finite(out$yr) & is.finite(out$bt) & out$bt > 0
  if (!all(valid)) stop(sum(!valid), " baris indeks tidak valid.")
  if (anyDuplicated(out[c("Stock", "yr")])) stop("File indeks memiliki duplikat Stock-tahun.")
  out
}

merge_abundance_index <- function(catch_df, index_df) {
  out <- catch_df
  if (!"bt" %in% names(out)) out$bt <- NA_real_
  keys <- paste(out$Stock, out$yr, sep = "\r")
  index_keys <- paste(index_df$Stock, index_df$yr, sep = "\r")
  matched <- match(keys, index_keys)
  use <- !is.na(matched)
  out$bt[use] <- index_df$bt[matched[use]]
  out
}
