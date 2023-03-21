source("requirements.R")

#creating basic dfs for general use

#optional script that generates grambank dataset, parameters file, and glottolog from git submodules -- the files that have already been made available by authors.
#this script is only runnable if the external data are correctly downloaded
#source("get_external_data.R")
#source("generating_GB_input_file.R")

if(!(file.exists("output/Bound_morph/bound_morph_score.tsv"))){
  cat("Calculating boundness score.\n")
  source("creating_boundness_metric.R")
}

if(!(file.exists("output/Informativity/informativity_score.tsv"))){
  cat("Calculating informativity score.\n")
  source("creating_informativity_score.R")
}

if(!(file.exists("data_wrangling/wrangled.tree"))){
  cat("Pruning EDGE-tree.\n")
  source("wrangling_tree.R")
}

AUTOTYP_areas_fn <- "data_wrangling/glottolog_AUTOTYPE_areas.tsv"
if(!(file.exists(AUTOTYP_areas_fn))){
  cat("Generating table of AUTOTYP-areas.\n")
  source("assigning_AUTOTYP_areas.R")
}

#GB langs for subsettting
GB_langs <- read_tsv("data/GB_wide/GB_wide_strict.tsv", col_types = WIDE_COLSPEC) %>% 
  dplyr::select(Language_ID)
