source("requirements.R")

library(INLA)
inla.setOption(inla.mode = "experimental")

wals <- read.delim(
  "https://raw.githubusercontent.com/cldf-datasets/wals/master/cldf/languages.csv",
  sep = ","
) %>%
  dplyr::select(ID, ISO_639 = ISO_codes, Name, Glottocode) %>%
  rename(Language_ID = ID) %>% #renaming the column to avoid problems
  left_join(
    read.delim(
      "https://raw.githubusercontent.com/cldf-datasets/wals/master/cldf/values.csv",
      sep = ","
    ) %>% dplyr::select(Language_ID, Parameter_ID, Value)
  ) %>%
  dplyr::select(-Language_ID) %>%
  rename(Language_ID = Glottocode)

wals_selected <- wals %>%
  filter(
    Parameter_ID == "20A" |
      Parameter_ID == "26A" |
      Parameter_ID == "49A" |
      Parameter_ID == "28A" |
      Parameter_ID == "98A" |
      Parameter_ID == "22A" |
      Parameter_ID == "100A" |
      Parameter_ID == "102A" |
      Parameter_ID == "48A" |
      Parameter_ID == "29A" |
      Parameter_ID == "74A" |
      Parameter_ID == "75A" |
      Parameter_ID == "76A" |
      Parameter_ID == "77A" |
      Parameter_ID == "112A" |
      Parameter_ID == "34A" |
      Parameter_ID == "36A" |
      Parameter_ID == "92A" |
      Parameter_ID == "66A" |
      Parameter_ID == "67A" |
      Parameter_ID == "65A" |
      Parameter_ID == "70A" |
      Parameter_ID == "57A" |
      Parameter_ID == "59A" |
      Parameter_ID == "73A" |
      Parameter_ID == "38A" |
      Parameter_ID == "39A" |
      Parameter_ID == "41A" |
      Parameter_ID == "101A"
  ) %>% 
  pivot_wider(
    names_from = Parameter_ID,
    values_from = Value
  ) 

# Specify the range of columns
start_column <- "92A"
end_column <- "76A"

# Filter and gather the selected columns
wals_selected_na <- wals_selected %>%
  rowwise() %>%
  mutate(na_proportion = mean(is.na(c_across(starts_with(start_column):starts_with(end_column))))) %>% 
  filter(na_proportion <= 0.35)

wals_selected_na %>%
  write_csv("output_tables/WALS_high_coverage.csv")

