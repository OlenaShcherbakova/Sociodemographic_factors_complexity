#informativity

OUTPUTDIR2 <- file.path('.', "output", "Informativity")		
# create output dir if it does not exist.		
if (!dir.exists(OUTPUTDIR2)) { dir.create(OUTPUTDIR2) }	

if (!file.exists(here(OUTPUTDIR2, "informativity_score.tsv"))) { 

GB_wide <- read_tsv(file.path("data", "GB_wide", "GB_wide_strict.tsv"), col_types=WIDE_COLSPEC)	

#read in sheet with scores for whether a feature denotes informativity
GB_informativity_points <- read.csv(file.path("data", "GB_wide", "parameters.csv")) %>%		dplyr::select(Parameter_ID = ID, informativity) %>%
  mutate(informativity = replace(informativity, Parameter_ID == "GB177", "argumentanimacy")) #manually adding another parameter: assigning the parameter of GB177 ("Can the verb carry a marker of animacy of argument, unrelated to any gender/noun class of the argument visible in the NP domain?") feature to be informative

GB_informativity_points[GB_informativity_points == ""] <- NA

GB_informativity_points <-  GB_informativity_points %>%
  filter(!is.na(informativity))

#reading in glottolog cdf for language levelling
glottolog_df<- read_tsv("data_wrangling/glottolog_cldf_wide_df.tsv", col_types = cols()) %>% 
  dplyr::select(Language_ID = Glottocode, Language_level_ID)

#calculating na_prop only for boundess features
GB_informativity_vec <- GB_informativity_points%>% 
  filter(!is.na(informativity)) %>% 
  dplyr::select(Parameter_ID ) %>% 
  as.matrix() %>% 
  as.vector()

GB_wide[, GB_informativity_vec] %>%
  apply(1, function(x) mean(is.na(x))) -> GB_wide$na_prop_informativity

hist(GB_wide$na_prop_informativity)

GB_long_for_calc <- GB_wide  %>% 
  filter(na_prop_informativity <= 0.25 ) %>%
  dplyr::select(-c(na_prop, na_prop_informativity)) %>% 
  reshape2::melt() %>% 
  rename(Parameter_ID = variable) %>%
  inner_join(GB_informativity_points )

##informativity score
lg_df_informativity_score <- GB_long_for_calc %>% 
  mutate(value = if_else(Parameter_ID == "GB140", abs(value-1), value)) %>% # reversing GB140 because 0 is the informative state
  left_join(glottolog_df) %>% 
  group_by(Language_level_ID, informativity) %>% #grouping per language and per informativity category
  summarise(sum_informativity = sum(value, na.rm = T), #for each informativity cateogry for each langauge, how many are answered 1 ("yes")
            sum_na = sum(is.na(value))) %>% #how many of the values per informativity category are missing
  mutate(sum_informativity = ifelse(sum_na >= 1 & sum_informativity == 0, NA, sum_informativity)) %>% #if there is at least one NA and the sum of values for the entire category is 0, the informativity score should be NA because there could be a 1 hiding under the NA value
  mutate(informativity_score = ifelse(sum_informativity >= 1, 1, sum_informativity)) %>% 
  ungroup() %>% 
  group_by(Language_level_ID) %>% 
  summarise(`Informativity`= mean(informativity_score, na.rm = T)) %>% 
  dplyr::select(Language_ID = Language_level_ID, `Informativity`) 

lg_df_informativity_score  %>% 		
  write_tsv(here(OUTPUTDIR2, "informativity_score.tsv"))	}