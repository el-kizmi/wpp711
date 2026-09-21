source("R/cpue_helpers.R")

set.seed(17)
years <- 2018:2023
n_per_year <- 24
raw <- do.call(rbind, lapply(seq_along(years), function(i) {
  effort <- sample(1:5, n_per_year, replace = TRUE)
  gt <- sample(c(8, 12, 18), n_per_year, replace = TRUE)
  gear <- rep(c("Jaring", "Pancing"), length.out = n_per_year)
  rate <- exp(-0.35 + 0.12 * i + ifelse(gear == "Pancing", 0.2, 0) + 0.18 * log(gt))
  catch <- rgamma(n_per_year, shape = 8, scale = rate * effort / 8)
  catch[c(1, 13)] <- 0
  data.frame(Stock = "Stok_Uji_711", yr = years[i], nama_alat_tangkap = gear,
             jumlah_hari_operasi = effort, gt_kapal = gt, berat_ikan_kg = catch,
             musim = rep(c("Barat", "Timur"), length.out = n_per_year))
}))
raw$musim[2] <- ""

fit <- standardize_cpue_dataset(raw, optional_covariates = "musim")
stopifnot(nrow(fit$index) == length(years))
stopifnot(all(is.finite(fit$index$bt)), all(fit$index$bt > 0))
stopifnot(abs(exp(mean(log(fit$index$bt))) - 1) < 1e-8)
stopifnot(identical(fit$diagnostics$model, "Delta-Gamma"))
stopifnot(isTRUE(fit$diagnostics$ready_for_bsm))
stopifnot(fit$diagnostics$observations == nrow(raw) - 1)
stopifnot(any(grepl("kovariat kosong", fit$warnings, fixed = TRUE)))

catch <- data.frame(Stock = "Stok_Uji_711", yr = years, ct = seq(1000, 1500, length.out = length(years)))
merged <- merge_abundance_index(catch, fit$index)
stopifnot(all(is.finite(merged$bt)), nrow(merged) == nrow(catch))

legacy <- read.csv("CPUE/data cpue GLM.csv", check.names = FALSE, stringsAsFactors = FALSE)
legacy_fit <- standardize_cpue_dataset(legacy, default_stock = "Legacy_2025", default_year = 2025)
stopifnot(nrow(legacy_fit$index) == 1)
stopifnot(!isTRUE(legacy_fit$diagnostics$ready_for_bsm))

cat("CPUE standardization tests passed\n")
