source("requirements.R")

if(!dir.exists("./grambank-analysed/R_grambank/output")){
  dir.create("./grambank-analysed/R_grambank/output")}

#creating a full Grambank file: first, within the submodule itself, and next placing it in the data folder within the repository 

setwd("./grambank-analysed/R_grambank")

if(!(file.exists("./../../data/GB_wide/GB_wide_strict.tsv"))){
  cat("Generating GB_wide_strict.\n")
  
  source("make_wide.R")
  
  read_tsv("output/GB_wide/GB_wide_strict.tsv", show_col_types = F) %>% 
        write_tsv(file="../../data/GB_wide/GB_wide_strict.tsv")
}

#extracting glottolog: first, within the submodule itself, and next placing it in the data folder within the repository 

if(!(file.exists("../../../data_wrangling/glottolog_cldf_wide_df.tsv"))){
  cat("Generating glottolog table.\n")
  
  source("make_glottolog-cldf_table.R")
  
  read_tsv("output/non_GB_datasets/glottolog-cldf_wide_df.tsv", show_col_types = F) %>% 
    write_tsv(file="./../../data_wrangling/glottolog_cldf_wide_df.tsv") #taking the final dataframe cldf_wide_df from the previous script and saving it in our datafolder
}

if(!(file.exists("./../data/GB_wide/parameters.csv"))){
  cat("Generating parameters table.\n")
  
read_csv("../grambank/cldf/parameters.csv",show_col_types = F) %>%
    dplyr::select(ID, Name, Description, boundness, informativity) %>% 
      write.csv(file="../../data/GB_wide/parameters.csv")
}


setwd("./../")
