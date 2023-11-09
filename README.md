---
editor_options: 
  markdown: 
    wrap: 72
---

# Code accompanying the paper *Societies of strangers do not speak grammatically simpler languages* by Olena Shcherbakova, Susanne Maria Michaelis, Hannah J. Haynie, Sam Passmore, Volker Gast, Russell D. Gray, Simon J. Greenhill, Damián E. Blasi, and Hedvig Skirgård

# Overview of structure

This project contains all data and all scripts for data-wrangling,
analysis and plotting.

## Data sources

The data that serves as the input for the analysis comes from Grambank,
(v1.0, Skirgård et al (in prep)), AUTOTYP (v1.01, Bickel et al (2022)),
Glottolog (v4.5), EDGE-tree (v1.0.0, Bouckaert et al (2023)), Ethnologue
(Eberhard et al 2020)), and WALS (Dryer & Haspelmath 2013).

With the exception of the Ethnologue data, all the data is available
openly via the science archive Zenodo and/or public GitHub repositories.
A modified version of the Ethnologue data is available in this
repository, it contains transformed population numbers that cannot be
transformed back into the raw numbers. The MCCT EDGE-tree is found in a
file inside grambank-analysed.

Zenodo locations:

-   Grambank (v.1.0) <https://doi.org/10.5281/zenodo.7740140>
-   Grambank-analysed (v1.0) <https://doi.org/10.5281/zenodo.7740822>
-   Glottolog-cldf (v4.5) <https://doi.org/10.5281/zenodo.5772649>
-   AUTOTYP (v1.0.1) <https://doi.org/10.5281/zenodo.6255206>

GitHub locations:

-   EDGE-tree (v1.0.0)
    <https://github.com/rbouckaert/global-language-tree-pipeline/tree/v1.0.0>
-   Grambank (v1.0) <https://github.com/grambank/grambank/tree/v1.0>
-   Grambank-analysed (v1.0)
    <https://github.com/grambank/grambank-analysed/tree/v1.0>
-   Glottolog-cldf (v4.5)
    <https://github.com/glottolog/glottolog-cldf/tree/v4.5>
-   AUTOTYP (v1.01)
    <https://github.com/autotyp/autotyp-data/tree/v1.0.1>

In this project, we fetch the data from the Zenodo locations by
downloading a zip file and expanding it. We have also made tables and
files available derived from these sources in this repos so that users
may run the analysis without engaging with fetching from Zenodo. The
scripts that generate these files are also found in this repository and
can be run by users if they would like.

## Running data-wrangling, analysis and plotting scripts

All scripts are written in R. The necessary scripts can be called
one-by-one in order or executed by running the script `all_scripts.R`.

Running `all_scripts.R` involves the following:

-   downloading, installing and loading necessary packages and create
    folders for output (see `requirements.R` & `install_and_load_INLA.R`
    for specific packages)
-   generating a table of languoids from Glottolog v.4.5
-   calculating metric scores from Grambank v.1.0: fusion metric and
    informativity metric; both metrics designed by Hedvig Skirgård and
    Hannah J. Haynie.
-   generating population table (all sociodemographic variables in one
    dataframe): data from Ethnologue e24 (Eberhard et al. 2020) and
    Supplementary Materials in `data\lang_endangerment_predictors.xlsx`
    from Bromham et al. (2022). Based on data availability, within
    `set_up_inla.R`, it is necessary to specify whether `sample` is
    `"full"` (full access to both Ethnologue variables in transformed
    and non-transformed form and running all models; possible only for
    users with their own access to Ethnologue) and `"reduced"` (access
    to both Ethnologue variables - the number of L1 speakers and the
    proportion of L2 speakers - in transformed form (logged and
    standardized number of L1 speakers and the proportion of L2 speakers
    than than raw numbers) and running all models except for one
    including the interaction between the number of L1 speakers and L2
    proportion; the dataset is already provided within the repository).
-   wrangling global phylogeny - EDGE-tree (v1.0.0, Bouckaert et al
    2023)
-   generating AUTOTYP-areas table (v.1.0.1, Bickel et al. 2020)
-   prepare everything for and run INLA analysis, including sensitivity
    analyses
-   measuring phylogenetic signal in fusion and informativity
-   generating tables from INLA analyses, including sensitivity analyses
-   make plots
-   running additional analyses on WALS-based morphological complexity
    scores used in Lupyan & Dale's (2010) study
    (`data/complexity_data_WALS.csv`) (obtained from Gary Lupyan,
    personal communication 02.06.2023)

Please note: the necessary files, such as metrics scores obtained from
the Grambank dataset and parameters of metrics (these determine the
inclusion of Grambank into the metrics), are already made available. The
script that generates these `generating_GB_input_file.R` relies on the
folder `grambank_analysed` which incorporates data from Grambank v.1.0,
AUTOTYP (v1.0.1) and Glottolog v.4.5. To run this script, one needs to
first clone the repository and then run the R-script
`get_external_data.R`.

## Spatial variance-covariance matrix

Since we divide `rdist.earth_dists` by 100 (originally calculated in
kilometers using `fields::rdist.earth`), the distances supplied to
`varcov.spatial` (from `geoR` package) are in 100's of kilometers rather
than kilometers. This approach allows to capture the spatial covariance
between languages based on the Euclidean distance between them.

`varcov.spatial(dists.lowertri = rdist.earth_dists[lower.tri(rdist.earth_dists)] / 100, cov.pars = phi_1, kappa = kappa)$varcov`

# References

R. Bouckaert, D. Redding, O. Sheehan, T. Kyritsis, R. Gray, K. E. Jones,
Q. Atkinson, Global language diversification is linked to socio-ecology
and threat status (2022), , <doi:10.31235/osf.io/f8tr6>.

Bickel, Balthasar, Johanna Nichols, Taras Zakharko, Alena
Witzlack-Makarevich, Kristine Hildebrandt, Michael Rießler, Lennart
Bierkandt, Fernando Zúñiga & John B Lowe. 2022. The AUTOTYP database
(v1.1.0). <https://doi.org/10.5281/zenodo.6793367>.

Bromham, Lindell, Russell Dinnage, Hedvig Skirgård, Andrew Ritchie,
Marcel Cardillo, Felicity Meakins, Simon Greenhill & Xia Hua. 2022.
Global predictors of language endangerment and the future of linguistic
diversity. Nature ecology & evolution 6(2). 163--173.

Dryer, Matthew & Martin Haspelmath (eds.). 2013. The World Atlas of
Language Structures Online. Leipzig: Max Planck Institute for
Evolutionary Anthropology. <http://wals.info>.

Eberhard, David M., Gary F. Simons & Charles D. Fennig (eds.). 2020.
Ethnologue: Languages of the World. Dallas, Texas: SIL International.
www.ethnologue.com.

Hammarström, Harald & Forkel, Robert & Haspelmath, Martin & Bank,
Sebastian. 2021. Glottolog 4.5. Leipzig: Max Planck Institute for
Evolutionary Anthropology. (Available online at <https://glottolog.org>)

Skirgård, H., Haynie, H. J., Blasi, D. E., Hammarström, H., Collins, J.,
Latarche, J., Lesage, J., Weber, T., Witzlack-Makarevich, A., Passmore,
S., Chira, A., Dinnage, R., Maurits, L., Dinnage, R., Dunn, M., Reesink,
G., Singer, R., Bowern, C., Epps, P., Hill, J., Vesakoski, O., Robbeets,
M., Abbas, K., Auer, D., Bakker, N., Barbos, G., Borges, R., Danielsen,
S., Dorenbusch, L., Dorn, E., Elliott, J., Falcone, G., Fischer, J.,
Ghanggo Ate, Y., Gibson, H., Göbel, H., Goodall, J., Gruner, V., Harvey,
A., Hayes, R., Heer, L., Herrera Miranda, R., Hübler, N.,
Huntington-Rainey, B., Ivani, J., Johns, M., Just, E., Kashima, E.,
Kipf, C., Klingenberg, J., König, N., Koti, K., Kowalik, R.,
Krasnoukhova, O., Lindvall, N., Lorenzen, M., Lutzenberger, H., Martins,
T., Mata German, C., Meer, S., Montoya Samamé, J., Müller, M.,
Muradoglu, S., Neely, K., Nickel, J., Norvik, M., Oluoch, C. A.,
Peacock, J., Pearey , I., Peck, N., Petit, S., Pieper, S., Poblete, M.,
Prestipino, D., Raabe, L., Raja, A., Reimringer, J., Rey, S., Rizaew,
J., Ruppert, E., Salmon, K., Sammet, J., Schembri, R., Schlabbach, L.,
Schmidt, F., Skilton, A., Smith, W. D., Sousa, H., Sverredal, K., Valle,
D., Vera, J., Voß, J., Witte, T., Wu, H., Yam, S., Ye 葉婧婷, J., Yong,
M., Yuditha, T., Zariquiey, R., Forkel, R., Evans, N., Levinson, S. C.,
Haspelmath, M., Greenhill, S. J., Atkinson, Q. D. and Gray, R. D. (in
prep) "Grambank reveals the importance of genealogical constraints on
linguistic diversity and highlights the impact of language loss".
Science Advances

Hedvig Skirgård; Hannah J. Haynie; Harald Hammarström; Damián E. Blasi;
Jeremy Collins; Jay Latarche; Jakob Lesage; Tobias Weber; Alena
Witzlack-Makarevich; Michael Dunn; Ger Reesink; Ruth Singer; Claire
Bowern; Patience Epps; Jane Hill; Outi Vesakoski; Noor Karolin Abbas;
Sunny Ananth; Daniel Auer; Nancy A. Bakker; Giulia Barbos; Anina Bolls;
Robert D. Borges; Mitchell Browen; Lennart Chevallier; Swintha
Danielsen; Sinoël Dohlen; Luise Dorenbusch; Ella Dorn; Marie Duhamel;
Farah El Haj Ali; John Elliott; Giada Falcone; Anna-Maria Fehn; Jana
Fischer; Yustinus Ghanggo Ate; Hannah Gibson; Hans-Philipp Göbel; Jemima
A. Goodall; Victoria Gruner; Andrew Harvey; Rebekah Hayes; Leonard Heer;
Roberto E. Herrera Miranda; Nataliia Hübler; Biu H. Huntington-Rainey;
Guglielmo Inglese; Jessica K. Ivani; Marilen Johns; Erika Just; Ivan
Kapitonov; Eri Kashima; Carolina Kipf; Janina V. Klingenberg; Nikita
König; Aikaterina Koti; Richard G. A. Kowalik; Olga Krasnoukhova; Kate
Lynn Lindsey; Nora L. M. Lindvall; Mandy Lorenzen; Hannah Lutzenberger;
Alexandra Marley; Tânia R. A. Martins; Celia Mata German; Suzanne van
der Meer; Jaime Montoya; Michael Müller; Saliha Muradoglu;
HunterGatherer; David Nash; Kelsey Neely; Johanna Nickel; Miina Norvik;
Bruno Olsson; Cheryl Akinyi Oluoch; David Osgarby; Jesse Peacock; India
O.C. Pearey; Naomi Peck; Jana Peter; Stephanie Petit; Sören Pieper;
Mariana Poblete; Daniel Prestipino; Linda Raabe; Amna Raja; Janis
Reimringer; Sydney C. Rey; Julia Rizaew; Eloisa Ruppert; Kim K. Salmon;
Jill Sammet; Rhiannon Schembri; Lars Schlabbach; Frederick W. P.
Schmidt; Dineke Schokkin; Jeff Siegel; Amalia Skilton; Hilário de Sousa;
Kristin Sverredal; Daniel Valle; Javier Vera; Judith Voß; Daniel
Wikalier Smith; Tim Witte; Henry Wu; Stephanie Yam; Jingting Ye 葉婧婷;
Maisie Yong; Tessa Yuditha; Roberto Zariquiey; Robert Forkel; Nicholas
Evans; Stephen C. Levinson; Martin Haspelmath; Simon J. Greenhill;
Quentin D. Atkinson; Russell D. Gray (2023) Grambank v1.0
<https://doi.org/10.5281/zenodo.7740140>

The Grambank Consortium (eds.). 2022. Grambank 1.0. Leipzig: Max Planck
Institute for Evolutionary Anthropology. <http://grambank.clld.org>.
