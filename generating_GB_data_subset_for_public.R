source("requirements.R")

GB_wide_full <-
  read_tsv("data/GB_wide/GB_wide_strict.tsv", show_col_types = F)

parameters <- data.table::fread(
  file.path("data", "GB_wide", "parameters.csv"),
  encoding = 'UTF-8',
  quote = "\"",
  header = TRUE,
  sep = ","
) %>%
  dplyr::filter(!is.na(boundness),!is.na(informativity))

relevant_parameters <- c("Language_ID", "na_prop", parameters$ID)

GB_wide_public <- GB_wide_full %>%
  dplyr::select(all_of(relevant_parameters)) %>%
  filter(na_prop <= 0.25) %>%
  dplyr::select(-na_prop) %>%
  write_tsv("data/GB_wide/GB_wide_subset.tsv")
