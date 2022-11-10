# Please run this script first to make sure you have all the necessary packages 
# installed for running the rest of the scripts in this R project

.libPaths(c("rlib/", .libPaths()))

if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") }

pacman::p_load(
  here,
	tidyverse,
	reshape2,
	broom,
	glue,
	forcats,
	magrittr,
	stringr,
	purrr,
	rcompanion, 
	naniar,
	readr,
	tidyr,
	dplyr,
  openxlsx,
  jsonlite,
  readxl,
  flextable,
  officer,
  
	
	#INLA - related
	sp, 
	scico,
	BiocManager,
	foreach,
	
	MASS,
	matrixStats,
	
	#brms
#	brms,

	# imputation
	missForest,
	
	#plotting graphs
	scales,
	RColorBrewer,
	ggpubr,
	ggplot2,
	ggrepel,
	gplots,
	ggtree,
	ggridges,
	grid,
	gridExtra,
	scales,
	ggmap,
	psych, #for scatterplot matrix
	viridis,
	rlang,
	devtools,
	patchwork,
  ggnewscale,
  ggstance,
	
	#making maps
	mapdata,
	maptools,
	maps,
  geoR,
  geosphere,
  fields,
	
	# phylogenetic packages
	ape,
	phytools,
	nlme,
	caper,
  MCMCglmm,

	
	# testing
	assertthat,
	beepr
)

# quiet down, tidyverse:
options(tidyverse.quiet = TRUE)
options(warn.conflicts = FALSE)
options(stringsAsFactors = FALSE)

GRAMBANK_LANGUAGES <- file.path("../..", "cldf", "languages.csv")
GRAMBANK_VALUES <- file.path("../..", "cldf", "values.csv")
GRAMBANK_PARAMETERS <- file.path("../..", "cldf", "parameters.csv")
GRAMBANK_CODES <- file.path("../..", "cldf", "codes.csv")

# The columns specifier for readr to parse ../cldf/values.csv
VALUES_COLSPEC <- c(
  ID = col_character(),
  Language_ID = col_character(),
  Parameter_ID = col_character(),
  Value = col_character(),
  Code_ID = col_character(),
  Comment = col_character(),
  Source = col_character()
)

LANGUAGES_COLSPEC = c(
  ID = col_character(),
  Name = col_character(),
  Macroarea = col_character(),
  Latitude = col_double(),
  Longitude = col_double(),
  Glottocode = col_character(),
  ISO639P3code = col_character(),
  contributed_datapoints = col_character(),
  provenance = col_character(),
  Family_name = col_character(),
  Family_id = col_character()
)

PARAMETERS_COLSPEC = c(
    ID = col_character(),
    Name = col_character(),
    Description = col_character(),
    patron = col_character(),
    name_in_french = col_character(),
    Grambank_ID_desc = col_character(),
    bound_morphology = col_character()
)

CODES_COLSPEC = c(
    ID = col_character(),
    Parameter_ID = col_character(),
    Name = col_character(),
    Description = col_character()
)

WIDE_COLSPEC = c(
    .default = col_integer(),
    Language_ID = col_character(),
    na_prop = col_double()
)

#creating folders
OUTPUTDIR_models <- here("output_models")		
# create output dir if it does not exist.		
if (!dir.exists(OUTPUTDIR_models)) { dir.create(OUTPUTDIR_models) }	

OUTPUTDIR_tables <- here("output_tables")		
# create output dir if it does not exist.		
if (!dir.exists(OUTPUTDIR_tables)) { dir.create(OUTPUTDIR_tables) }	

OUTPUTDIR_output <- here("output")		
# create output dir if it does not exist.		
if (!dir.exists(OUTPUTDIR_output)) { dir.create(OUTPUTDIR_output) }	

OUTPUTDIR_data_wrangling<- here("data_wrangling")		
# create output dir if it does not exist.		
if (!dir.exists(OUTPUTDIR_data_wrangling)) { dir.create(OUTPUTDIR_data_wrangling) }	

#source custom functions to have them at hand
source("varcov.spatial_function.R")

#Adding a prior for modelling with INLA on precision/SD of random effects and likelihood: the probability of SD of each random effect and likelihood being > 1 is equal to 0.1
pcprior_hyper = list(prec =list(prior="pc.prec", param = c(1, 0.1)))
