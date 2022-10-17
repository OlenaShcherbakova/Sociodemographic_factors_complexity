#This script assigns all languages in glottolog_df to their nearest AUTOTYP area

#Script was written by X

source("requirements.R")

OUTPUTDIR_data_wrangling<- here("data_wrangling")		
# create output dir if it does not exist.		
if (!dir.exists(OUTPUTDIR_data_wrangling)) { dir.create(OUTPUTDIR_data_wrangling) }	

if (!file.exists(here(OUTPUTDIR_data_wrangling, "glottolog_AUTOTYPE_areas.tsv"))) { 

#GB langs for subsettting
GB_langs <- read_tsv("data/GB_wide/GB_wide_strict.tsv", col_types = WIDE_COLSPEC) %>% 
  dplyr::select(Language_ID)

#combining the tables languages and values from glottolog_df-cldf into one wide dataframe.
#this can be replaced with any list of Language_IDs, long and lat
  
glottolog_fn <- "data_wrangling/glottolog_cldf_wide_df.tsv"
if(!file.exists(glottolog_fn)){
  source("generating_GB_input_file.R")
}

glottolog_df <- read.delim(glottolog_fn , sep = "\t")%>% 
  dplyr::select(Language_ID, Longitude, Latitude) %>% 
  inner_join(GB_langs, by = "Language_ID")

##Adding in areas of linguistic contact from AUTOTYP

AUTOTYP <- read.delim("https://raw.githubusercontent.com/autotyp/autotyp-data/master/data/csv/Register.csv", sep = ",") %>% 
  dplyr::select(Language_ID = Glottocode, Area, Longitude, Latitude) %>% 
  group_by(Language_ID, Area) %>% #some lgs are assigned to more than one area, we level that out.
  sample_n(1)

#This next bit where we find the autotyp areas of languages was written by Seán Roberts
# We know the autotyp-area of langauges in autotyp and their long lat. We don't know the autotyp area of languages in Glottolog. We also can't be sure that the long lat of languoids with the same glottoids in autotyp and glottolog_df have the exact identical long lat. First let's make two datasets, one for autotyp languages (hence lgs where we know the area) and those that we wish to know about, the Glottolog ones.

lgs_with_known_area <- as.matrix(AUTOTYP[!is.na(AUTOTYP$Area),c("Longitude","Latitude")])
rownames(lgs_with_known_area) <- AUTOTYP[!is.na(AUTOTYP$Area),]$Language_ID

known_areas <- AUTOTYP %>% 
  dplyr::filter(!is.na(Area)) %>% 
  dplyr::select(Language_ID, Area) %>% 
  distinct() %>% 
  dplyr::select(AUTOTYP_Language_ID = Language_ID, everything())

rm(AUTOTYP)

lgs_with_unknown_area <- as.matrix(glottolog_df[,c("Longitude","Latitude")])
rownames(lgs_with_unknown_area) <- glottolog_df$Language_ID

# For missing, find area of closest langauge
atDist <- rdist.earth(lgs_with_known_area,lgs_with_unknown_area, miles = F)

rm(lgs_with_known_area, lgs_with_unknown_area)

df_matched_up <- as.data.frame(unlist(apply(atDist, 2, function(x){names(which.min(x))})), stringsAsFactors = F) %>% 
  rename(AUTOTYP_Language_ID = `unlist(apply(atDist, 2, function(x) {     names(which.min(x)) }))`)

glottolog_df_with_AUTOTYP <- df_matched_up %>% 
  tibble::rownames_to_column("Language_ID") %>%
  full_join(known_areas, by = "AUTOTYP_Language_ID") %>% 
  right_join(glottolog_df, by = "Language_ID") %>% 
  dplyr::select(-AUTOTYP_Language_ID) %>% 
  group_by(Language_ID) %>% #some lgs are assigned to more than one area, we level that out.
  sample_n(1) %>% 
  rename(AUTOTYP_area = Area) 

  glottolog_df_with_AUTOTYP  %>% 		
    write_tsv(here(OUTPUTDIR_data_wrangling, "glottolog_AUTOTYPE_areas.tsv")) }	
