#Running all scripts

#download packages and create folders 
#generate Glottolog table (based on Glottolog 4.4)
#calculate metric scores (based on Grambank 1.0)
#generate population table (all sociodemographic variables in one dataframe)
#wrangling EDGE tree
#generating AUTOTYP areas table
source("set_up_general.R") 

#setup for INLA analysis
source("install_and_load_INLA.R")
#choosing whether to use the full dataset (possible only for reviewers and if one has own access to Ethnologue and saved the dataset in the data folder on their own) or to only to the subset of Ethnologue with transformed variables made available in this repostiory after running create_pop_table.R

#sample <- "full"
sample <- "reduced" #default

source("make_ethnologue_SM_and_merging_tables.R")
source("create_pop_table.R")
source("set_up_inla.R")

#run all INLA models + extract main results tables 
#Note that previously "fusion" was called "boundness", and this is how it is referenced in all scripts
source("runs.R")

#conduct sensitivity testing + extract the corresponding table
source("runs_sensitivity.R")

#measure phylogenetic signal in two fusion and informativity
source("measuring_phylosignal.R")

#extract tables from INLA analyses
source("table_INLA_summary_all_models_SI.R")
source("variance_top_ranking_models.R")

#plotting
source("plot_social_effects_combined.R") #main results figure 
source("plot_maps_main.R") #maps of scores
source("plot_heatmap_B_I.R") #phylogenetic tree with a heatmap 
source("plot_map_Africa.R")
source("plot_map_Eurasia.R")
source("plot_heatmap_informativity_Uralic.R") #Uralic tree (informativity) + combined plot with two maps from above
source("plot_spatial_parameters_linear_distances.R") #SI figure for visualizing how covariance under different kappa and phi parameters corresponds to spatial distances
