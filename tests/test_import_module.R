.libPaths(c(normalizePath("R_library"), .libPaths()))
library(shiny)
library(DT)
source("R/mod_batch_import.R")

testServer(mod_batch_import_server, {
  session$setInputs(catch_file = list(
    name = "batch_catch_pelagis.csv", size = file.info("batch_catch_pelagis.csv")$size,
    type = "text/csv", datapath = normalizePath("batch_catch_pelagis.csv")
  ))
  session$setInputs(id_file = list(
    name = "batch_id_pelagis.csv", size = file.info("batch_id_pelagis.csv")$size,
    type = "text/csv", datapath = normalizePath("batch_id_pelagis.csv")
  ))
  session$flushReact()
  res <- session$returned
  stopifnot(nrow(res$catch_df()) > 0, nrow(res$id_df()) > 0)
  stopifnot(length(res$groups()) > 0, length(res$species_list()) > 0)
  stopifnot(setequal(unique(res$catch_df()$Stock), res$id_df()$Stock))
})

cat("Import module test passed\n")
