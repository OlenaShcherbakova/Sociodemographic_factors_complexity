if(!dir.exists("./grambank-analysed/R_grambank/output")){
  dir.create("./grambank-analysed/R_grambank/output")}

#creating a full Grambank file: first, within the submodule itself, and next placing it in the data folder within the repository 

setwd("./grambank-analysed/R_grambank")

if(!(file.exists("./../../data/GB_wide/GB_wide_strict.tsv"))){
  cat("Generating GB_wide_strict.\n")
  
  source("./make_wide.R")
  
  GB_wide <- read.csv("./output/GB_wide/GB_wide_strict.tsv", header = TRUE, sep = '\t', stringsAsFactors=FALSE) 
  
  GB_wide <- GB_wide %>%
    write_tsv(file="./../../data/GB_wide/GB_wide_strict.tsv")
}

#extracting glottolog: first, within the submodule itself, and next placing it in the data folder within the repository 

if(!(file.exists("./../../data_wrangling/glottolog_cldf_wide_df.tsv"))){
  cat("Generating glottolog table.\n")
  
  source("./make_glottolog-cldf_table.R")
  
  cldf_wide_df <- cldf_wide_df %>%
    write_tsv(file="./../../data_wrangling/glottolog_cldf_wide_df.tsv") #taking the final dataframe cldf_wide_df from the previous script and saving it in our datafolder
}

setwd("./../")

if(!(file.exists("./../data/GB_wide/parameters.csv"))){
  cat("Generating parameters table.\n")
  parameters <- read.csv("./grambank/cldf/parameters.csv", header = TRUE, stringsAsFactors=FALSE) %>%
    dplyr::select(ID, Name, Description, boundness, informativity)
  
  parameters[parameters == ""] <- NA
  
  parameters <- parameters %>%
    write.csv(file="./../data/GB_wide/parameters.csv")
}

setwd("./../")
