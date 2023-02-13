# script was written by Hedvig Skirgård

#Glottolog-cldf table for aggregating dialects
OUTPUTDIR_data_wrangling<- here("data_wrangling")		
# create output dir if it does not exist.		
if (!dir.exists(OUTPUTDIR_data_wrangling)) { dir.create(OUTPUTDIR_data_wrangling) }	

if (!file.exists(here(OUTPUTDIR_data_wrangling, "wrangled.tree"))) { 
  
  glottolog_df <- read_tsv("data_wrangling/glottolog_cldf_wide_df.tsv", col_types = cols()) %>%
    dplyr::select(Glottocode, Language_ID,  Language_level_ID) %>% 
    mutate(Language_level_ID = if_else(is.na(Language_level_ID), Glottocode, Language_level_ID))
  
  GB_languages <- read_tsv("data/GB_wide/GB_wide_strict.tsv",col_types = cols()) %>% 
    dplyr::select(Language_ID) #this column is already aggregated for dialects in make_wide.R
  
  #reading in tree
  EDGE_tree <- ape::read.nexus("data/phylogenies/EDGE6635-merged-relabelled.tree")
  
  #subsetting the tips to those in Grambank and such that there is only one per language_level_id, i.e. merging dialects and the like.
  to_keep <- EDGE_tree$tip.label %>% 
    as.data.frame() %>% 
    rename(tip.label = ".") %>% 
    separate(col = tip.label , into = c("Language_ID", "Name_EDGE"), remove = F, sep = 8) %>% 
    left_join(glottolog_df, by = "Language_ID") %>% 
    inner_join(GB_languages, by = "Language_ID") %>% 
    group_by(Language_level_ID) %>% 
    sample_n(1)
  
  #actually pruning the tree itself
  pruned_tree <- ape::keep.tip(EDGE_tree, to_keep$tip.label)
  
  #renaming tip labels to just glottocodes
  pruned_tree$tip.label <- pruned_tree$tip.label %>% 
    as.data.frame() %>% 
    rename(tip.label = ".") %>% 
    separate(col = tip.label , into = c("Language_ID", "Name_EDGE"), sep = 8) %>% 
    dplyr::select(Language_ID) %>% 
    .[,1]
  
  pruned_tree %>% 
    write.tree(file = here(OUTPUTDIR_data_wrangling, "wrangled.tree"))
}