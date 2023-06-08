#Running all scripts

#download packages and create folders 
#generate Glottolog table (based on Glottolog 4.4)
#calculate metric scores (based on Grambank 1.0)
#generate population table (all sociodemographic variables in one dataframe)
#wrangling EDGE tree
#generating AUTOTYP areas table
source("get_external_data.R")
source("generating_GB_input_file.R")
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

#predictors: random effects - phylogenetic and spatial (same scripts for "full" and "reduced" versions)
source("models_Boundness_phylogenetic_spatial.R")
source("models_Informativity_phylogenetic_spatial.R")

if(sample == "full"){
  
  #predictors: phylogenetic and spatial random effects + sociodemograhic variables as fixed effects
  source("models_Boundness_social.R")
  source("models_Informativity_social.R")
  
  #predictors: sociodemographic variables as fixed effects
  source("models_Boundness_social_only.R")
  source("models_Informativity_social_only.R") 
  
  #conduct sensitivity testing + extract the corresponding table
  source("runs_sensitivity.R") 
  
  #extract tables from INLA analyses
  source("table_INLA_summary_all_models_SI.R")
  source("variance_top_ranking_models.R")
  
  #plotting main results
  source("plot_social_effects_combined.R")
}

if(sample == "reduced"){
  
  #predictors: phylogenetic and spatial random effects + sociodemograhic variables as fixed effects 
  #(on reduced set of social variables: without log10 transformed L1 speakers)
  source("models_Boundness_reduced_social.R")
  source("models_Informativity_reduced_social.R")
  
  #predictors: sociodemographic variables as fixed effects
  source("models_Boundness_reduced_social_only.R")
  source("models_Informativity_reduced_social_only.R")
  
  #conduct sensitivity testing + extract the corresponding table
  source("runs_sensitivity_on_reduced.R")
  
  #extract tables from INLA analyses
  source("table_INLA_summary_all_models_SI_reduced.R")
  source("variance_top_ranking_models_reduced.R")
  
  #plotting main results
  source("plots_social_effects_combined_on_reduced.R")
}

#measure phylogenetic signal in two fusion and informativity
source("measuring_phylosignal.R")


#plotting
source("plot_maps_main.R") #maps of scores
source("plot_heatmap_B_I.R") #phylogenetic tree with a heatmap 
source("plot_map_Africa.R")
source("plot_map_Eurasia.R")
source("plot_heatmap_informativity_Uralic.R") #Uralic tree (informativity) + combined plot with two maps from above
source("plot_spatial_parameters_linear_distances.R") #SI figure for visualizing how covariance under different kappa and phi parameters corresponds to spatial distances

#additional analyses on WALS data
source("make_ethnologue_SM_for_morphological_complexity_reanalysis.R")
source("WALS_sparseness.R")
source("WALS_reanalysis_setup.R")
source("WALS_reanalysis_controlled_setup.R")
source("WALS_reanalysis_controlled_setup_high_coverage.R") #analysis + summary table
