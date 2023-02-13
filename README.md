# Sociodemographic_factors_complexity

Consult and run `all_scripts.R` that will execute all necessary scripts to:

- download packages and create folders 
- generate Glottolog table from Glottolog v.4.4 (Hammarström et al. 2021)
- calculate metric scores from Grambank v.1.0 (The Grambank Consortium 2022): fusion (previously: boundness) metric and informativity metric; both metrics designed by Hedvig Skirgård and Hannah J. Haynie
- generate population table (all sociodemographic variables in one dataframe): data from Ethnologue e24 (Eberhard et al. 2020) and Supplementary Materials in ```data\lang_endangerment_predictors.xlsx``` from Bromham et al. (2022). Based on data availability, within `set_up_inla.R`, it is necessary to specify whether `sample` is `"full"` (full access to both Ethnologue variables in transformed and non-transformed form and running all models; possible only for users with their own access to Ethnologue) and `"reduced"` (access to both Ethnologue variables - the number of L1 speakers and the proportion of L2 speakers - in transformed form (logged and standardized number of L1 speakers and the proportion of L2 speakers than than raw numbers) and running all models except for one including the interaction between the number of L1 speakers and L2 proportion; the dataset is already provided within the repository).
- wrangle EDGE tree
- generating AUTOTYP v.1.1.0 (Bickel et al. 2022) areas table
- prepare everything for and run INLA analysis, including sensitivity analyses
- measure phylogenetic signal in fusion and informativity
- generate tables from INLA analyses, including sensitivity analyses
- make plots


NB. The necessary files, such as metrics scores obtained from the Grambank dataset and parameters of metrics (these determine the inclusion of Grambank into the metrics), are already made available. But the scripts that generate these rely on the submodule ```grambank_analysed``` that incorporates data from Grambank v.1.0 and Glottolog v.4.4. These scripts can only be run when one has access to ```grambank-analysed``` repository and/or when this repository has been released and made publicly available. If this is the case, one needs to first clone the repository and then include and update the submodule by running ```git submodule update —init``` (More on using submodules: https://git-scm.com/book/en/v2/Git-Tools-Submodules#_cloning_submodules). 




References

Bickel, Balthasar, Johanna Nichols, Taras Zakharko, Alena Witzlack-Makarevich, Kristine Hildebrandt, Michael Rießler, Lennart Bierkandt, Fernando Zúñiga & John B Lowe. 2022. The AUTOTYP database (v1.1.0). https://doi.org/10.5281/zenodo.6793367.

Bromham, Lindell, Russell Dinnage, Hedvig Skirgård, Andrew Ritchie, Marcel Cardillo, Felicity Meakins, Simon Greenhill & Xia Hua. 2022. Global predictors of language endangerment and the future of linguistic diversity. Nature ecology & evolution 6(2). 163–173.

Eberhard, David M., Gary F. Simons & Charles D. Fennig (eds.). 2020. Ethnologue: Languages of the World. Dallas, Texas: SIL International. www.ethnologue.com.

Hammarström, Harald, Robert Forkel, Martin Haspelmath & Sebastian Bank. 2021. glottolog: Glottolog database 4.4. https://doi.org/10.5281/zenodo.4761960.

The Grambank Consortium (eds.). 2022. Grambank 1.0. Leipzig: Max Planck Institute for Evolutionary Anthropology. http://grambank.clld.org.





