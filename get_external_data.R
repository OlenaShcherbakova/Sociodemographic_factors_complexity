#Script was written by Hedvig Skirgård

source("requirements.R")

#setting up a tempfile path where we can put the zipped files before unzipped to a specific location
filepath <- file.path(tempfile())

##grambank-analysed: downloading, zipping and moving
grambank_analysed_fn <- c("https://zenodo.org/record/7740822/files/grambank/grambank-analysed-v1.0.zip")

utils::download.file(file.path(grambank_analysed_fn), destfile = filepath)
utils::unzip(zipfile = filepath, exdir = "grambank-analysed")

#Zenodo locations contain a dir with the name of the repos and the commit in the release. This is not convenient for later scripts, so we move the contents up one level
old_fn <- "grambank-analysed/grambank-grambank-analysed-fcf971a/"
old_fn_files <- list.files(old_fn)
new_fn <- "grambank-analysed/"

file.copy(from = paste0(old_fn, old_fn_files),to = new_fn, recursive = T, overwrite = T)
#remove old dir
unlink(old_fn, recursive = T)

## dirs within grambank-analysed
# for the dirs within grambank-analysed we can fetch them with a for loop

fns_within_grambank_analysed_zip<- c("https://zenodo.org/record/7740140/files/grambank/grambank-v1.0.zip", "https://zenodo.org/record/5772649/files/glottolog/glottolog-cldf-v4.5.zip", "https://zenodo.org/record/6255206/files/autotyp-data-v1.0.1.zip")
exdir_names <- c("grambank-analysed/grambank", "grambank-analysed/glottolog-cldf", "grambank-analysed/autotyp-data")

commit_dir_names <- c("grambank-analysed/grambank/grambank-grambank-9e0f341/", "grambank-analysed/glottolog-cldf/glottolog-glottolog-cldf-6f1558e/", "grambank-analysed/autotyp-data/autotyp-data-1.0.1/")


for(n in 1:3){
  utils::download.file(file.path(fns_within_grambank_analysed_zip[n]), destfile = filepath)
  utils::unzip(zipfile = filepath, exdir = exdir_names[n])
  
  old_fn <- commit_dir_names[n]
  old_fn_files <- list.files(old_fn)
  new_fn <- exdir_names[n]
  
  file.copy(from = paste0(old_fn, old_fn_files),to = new_fn, recursive = T, overwrite = T)
  #remove old dir
  unlink(old_fn, recursive = T)
  
}


