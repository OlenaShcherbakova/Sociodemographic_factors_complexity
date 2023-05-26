source("requirements.R")

library(INLA)
inla.setOption(inla.mode = "experimental")

sample <- "reduced"

gb_1 <-
  read_tsv("data/GB_wide/GB_wide_strict.tsv", col_types = WIDE_COLSPEC) %>%
  dplyr::select(Language_ID, GB408, GB409)

gb_2 <-
  read_tsv("data/GB_wide/GB_wide_strict.tsv", col_types = WIDE_COLSPEC) %>%
  dplyr::select(Language_ID, GB091, GB092, GB093, GB094)

gb_3 <-
  read_tsv("data/GB_wide/GB_wide_strict.tsv", col_types = WIDE_COLSPEC) %>%
  dplyr::select(Language_ID, GB262, GB263, GB264)

if (sample == "reduced") {
  pop_file_fn <- "data_wrangling/pop_reduced.tsv"
} else {
  pop_file_fn <- "data_wrangling/pop_full.tsv"
}

gb_1_joined <- read_tsv(pop_file_fn, show_col_types = F) %>%
  full_join(gb_1,
            by = "Language_ID") %>%
  filter(!is.na(GB408), !is.na(GB409)) %>%
  dplyr::mutate(Erg = ifelse(GB409 == 1, 1, 0)) %>%
  dplyr::mutate(Nom = ifelse(GB408 == 1, 1, 0)) %>%
  dplyr::mutate(
    Alignment = case_when(
      Erg == 1 & Nom == 0 ~ "1",
      Nom == 1 & Erg == 0 ~ "0",
      Erg == 1 &
        Nom == 1 ~ "strange",
      Erg == 0 &
        Nom == 0 ~ "remove"
    )
  ) %>%
  filter(Alignment != "remove" & Alignment != "strange") %>%
  dplyr::mutate(Alignment = as.numeric(Alignment)) %>%
  filter(!is.na(L1_log10_st)) %>%
  filter(!duplicated(Language_ID))

gb_2_joined <- read_tsv(pop_file_fn, show_col_types = F) %>%
  full_join(gb_2,
            by = "Language_ID") %>%
  filter(!is.na(GB091),!is.na(GB092),!is.na(GB093),!is.na(GB094)) %>%
  dplyr::mutate(A_marking = ifelse(GB091 == 1 |
                                     GB092 == 1, 1, 0)) %>% 
  dplyr::mutate(P_marking = ifelse(GB093 == 1 |
                                     GB094 == 1, 1, 0)) %>% 
  dplyr::mutate(
    Person_marking = case_when(A_marking == 1 & P_marking == 0 ~ 0,
                               A_marking == 0 & P_marking == 0 ~ 0,
                               A_marking == 1 & P_marking == 1 ~ 1,
                               A_marking == 0 & P_marking == 1 ~ 1)
  ) %>% 
  filter(!is.na(L1_log10_st)) %>%
  filter(!duplicated(Language_ID))


gb_3_joined <- read_tsv(pop_file_fn, show_col_types = F) %>%
  full_join(gb_3,
            by = "Language_ID") %>%
  filter(!is.na(GB262), !is.na(GB263), !is.na(GB264)) %>%
  
  dplyr::mutate(Question_particle = ifelse(GB262 == 1 |
                                             GB263 == 1 |
                                             GB264 == 1, 1, 0)) %>%
  filter(!is.na(L1_log10_st)) %>%
  filter(!duplicated(Language_ID))

