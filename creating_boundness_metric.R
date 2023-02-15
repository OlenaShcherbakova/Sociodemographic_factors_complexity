#boundness/fusion

#Script was written by Hedvig Skirgård

source("requirements.R")

OUTPUTDIR1 <- file.path('.', "output", "Bound_morph")
# create output dir if it does not exist.
if (!dir.exists(OUTPUTDIR1)) {
  dir.create(OUTPUTDIR1)
}

if (!file.exists(here(OUTPUTDIR1, "bound_morph_score.tsv"))) {
  GB_wide <-
    read_tsv(file.path("data", "GB_wide", "GB_wide_strict.tsv"),
             col_types = WIDE_COLSPEC)
  
  #read in sheet with scores for whether a feature denotes fusion
  GB_fusion_points <-
    data.table::fread(
      file.path("data", "GB_wide", "parameters.csv"),
      encoding = 'UTF-8',
      quote = "\"",
      header = TRUE,
      sep = ","
    ) %>%
    dplyr::select(Parameter_ID = ID, Fusion = boundness, informativity) %>%
    mutate(Fusion = as.numeric(Fusion))
  
  df_morph_count <- GB_wide %>%
    filter(na_prop <= 0.25) %>% #exclude languages with more than 25% missing data
    dplyr::select(-na_prop) %>%
    reshape2::melt(id.vars = "Language_ID") %>%
    dplyr::rename(Parameter_ID = variable) %>%
    inner_join(GB_fusion_points, by = "Parameter_ID") %>%
    filter(Fusion != 0) %>%
    filter(!is.na(value)) %>%
    mutate(value_weighted = if_else(Fusion == 0.5 &
                                      value == 1, 0.5, value)) %>% #replacing all instances of 1 for a feature that is weighted to 0.5 bound morph points to 0.5
    group_by(Language_ID) %>%
    dplyr::summarise(mean_morph = mean(value_weighted))  %>%
    dplyr::select(Language_ID, boundness = mean_morph)
  
  boundness_st = scale(df_morph_count$boundness)
  df_morph_count <- cbind(df_morph_count, boundness_st)
  
  df_morph_count  %>%
    write_tsv(file.path(OUTPUTDIR1, "bound_morph_score.tsv"))
  
}