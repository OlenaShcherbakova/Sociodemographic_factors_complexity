source("requirements.R")

library(INLA)
inla.setOption(inla.mode = "experimental")

sample <- "reduced"

wals <- read.delim(
  "https://raw.githubusercontent.com/cldf-datasets/wals/master/cldf/languages.csv",
  sep = ","
) %>%
  dplyr::select(ID, Name, Glottocode) %>%
  rename(Language_ID = ID) %>% #renaming the column to avoid problems
  left_join(
    read.delim(
      "https://raw.githubusercontent.com/cldf-datasets/wals/master/cldf/values.csv",
      sep = ","
    ) %>% dplyr::select(Language_ID, Parameter_ID, Value)
  ) %>%
  dplyr::select(-Language_ID) %>%
  rename(Language_ID = Glottocode)

wals_1 <- wals %>%
  filter(Parameter_ID == "98A") %>%
  filter(!is.na(Value)) %>%
  dplyr::mutate(Erg = ifelse(Value == 4, 1, 0)) %>%
  dplyr::mutate(Nom = ifelse(Value == 2 | Value == 3, 1, 0)) %>%
  dplyr::mutate(
    Alignment = case_when(
      Erg == 1 & Nom == 0 ~ "1",
      Nom == 1 & Erg == 0 ~ "0",
      Erg == 1 & Nom == 1 ~ "both",
      Erg == 0 &
        Nom == 0 ~ "remove"
    )
  ) %>%
  dplyr::select(Language_ID, Alignment) %>%
  filter(Alignment != "remove" & Alignment != "both")

wals_2 <- wals %>%
  filter(Parameter_ID == "102A") %>%
  filter(!is.na(Value)) %>%
  dplyr::mutate(Person_marking = ifelse(Value == 1 |
                                          Value == 2, 0, 1)) %>%
  dplyr::select(Language_ID, Person_marking) %>%
  filter(!is.na(Person_marking)) %>%
  filter(!duplicated(Language_ID))

wals_3 <- wals %>%
  filter(Parameter_ID == "92A") %>%
  filter(!is.na(Value)) %>%
  dplyr::mutate(Question_particle = ifelse(Value == 6, 0, 1)) %>%
  dplyr::select(Language_ID, Question_particle) %>%
  filter(!is.na(Question_particle)) %>%
  filter(!duplicated(Language_ID))

if (sample == "reduced") {
  pop_file_fn <- "data_wrangling/pop_reduced.tsv"
} else {
  pop_file_fn <- "data_wrangling/pop_full.tsv"
}

wals_1_joined <- read_tsv(pop_file_fn, show_col_types = F) %>%
  full_join(wals_1,
            by = "Language_ID") %>%
  filter(!is.na(Alignment),
         !is.na(L1_log10_st)) %>%
  filter(!duplicated(Language_ID))  %>% 
  mutate(Alignment = as.numeric(Alignment))

wals_2_joined <- read_tsv(pop_file_fn, show_col_types = F) %>%
  full_join(wals_2,
            by = "Language_ID") %>%
  filter(!is.na(Person_marking),
         !is.na(L1_log10_st)) %>%
  filter(!duplicated(Language_ID)) %>% 
  mutate(Person_marking = as.numeric(Person_marking))


wals_3_joined <- read_tsv(pop_file_fn, show_col_types = F) %>%
  full_join(wals_3,
            by = "Language_ID") %>%
  filter(!is.na(Question_particle),
         !is.na(L1_log10_st)) %>%
  filter(!duplicated(Language_ID)) %>% 
  mutate(Question_particle = as.numeric(Question_particle))
