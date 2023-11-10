#plotting maps
source("set_up_inla.R")

metrics_joined <- metrics_joined %>%
  filter(!is.na(L1_log10_st)) %>%
  rename(L1_log_st = L1_log10_st) %>%
  mutate(L1_copy = L1_log_st) %>%
  # filter(!is.na(L2_prop)) %>%
  # dplyr::mutate(L2_prop  = scale(L2_prop)[, 1]) %>%
  # mutate(L2_copy = L2_prop) %>%
  filter(!is.na(neighboring_languages_st)) %>%
  filter(!is.na(Official)) %>%
  filter(!is.na(Education)) %>%
  filter(!is.na(boundness_st)) %>%
  filter(!is.na(informativity_st)) %>%
  filter(
    Macroarea == "Africa" |
      Macroarea == "Papunesia" |
      Macroarea == "Australia" | Macroarea == "Eurasia"
  ) %>%
  filter(180 > Longitude & Longitude > -24) %>%
  filter(80 > Latitude & Latitude > -45)

#an overview of area and family coverage
tab_areas <- as.data.frame(table(metrics_joined$AUTOTYP_area)) %>%
  arrange(desc(Freq))

tab_families <- as.data.frame(table(metrics_joined$Family_ID)) %>%
  arrange(desc(Freq))


#dropping tips not in Grambank
metrics_joined <-
  metrics_joined[metrics_joined$Language_ID %in% tree$tip.label,]
tree <- keep.tip(tree, metrics_joined$Language_ID)

x <-
  assert_that(all(tree$tip.label %in% metrics_joined$Language_ID), msg = "The data and phylogeny taxa do not match")

lakes <-
  map_data(
    "lakes",
    col = "white",
    border = "gray",
    margin = T,
    ylim = c(-45, 80),
    xlim = c(-30, 180),
    wrap = c(-25, 335)
  )

world <-
  map_data(
    "world",
    wrap = c(-25, 335),
    ylim = c(-45, 80),
    xlim = c(-30, 180),
    margin = T
  )

world <-
  subset(world,!(
    region %in% c(
      "USA",
      "Brazil",
      "Mexico",
      "Colombia",
      "Argentina",
      "Canada",
      "Peru",
      "Venezuela",
      "Chile",
      "Guatemala",
      "Ecuador",
      "Bolivia",
      "Cuba",
      "Honduras",
      "Paraguay",
      "Nicaragua",
      "El Salvador",
      "Costa Rica",
      "Panama",
      "Uruguay",
      "Jamaica",
      "Trinidad and Tobago",
      "Guyana",
      "Suriname",
      "Belize",
      "Barbados",
      "Saint Lucia",
      "Grenada",
      "Saint Vincent and the Grenadines",
      "Antigua and Barbuda",
      "Saint Kitts and Nevis",
      "Greenland"
    )
  ))

#shifting the longlat of the dataframe to match the pacific centered map
combination <- metrics_joined %>%
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude))

#duplicates?

combination %>%
  group_by(Language_ID) %>%
  mutate(dupe = n() > 1) -> combination_dup
dupes <- combination_dup[combination_dup$dupe == "TRUE", ]

#Basemap
basemap <- ggplot(combination) +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    colour = "gray87",
    fill = "gray87",
    size = 0.5
  ) +
  geom_polygon(
    data = lakes,
    aes(x = long, y = lat, group = group),
    colour = "gray87",
    fill = "white",
    size = 0.3
  )  +
  theme(
    panel.grid.major = element_blank(),
    #all of these lines are just removing default things like grid lines, axises etc
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )  +
  coord_map(
    projection = "vandergrinten",
    ylim = c(-45, 80),
    xlim = c(-30, 180)
  )




#plotting informativity map with cumstom labels
combination_shaped_0 <- combination %>%
  mutate(Uralic = case_when(Family_ID == "ural1272" ~ "yes",
                            Family_ID != "ural1272" ~ "no")) %>%
  mutate(langs_of_interest = case_when(Family_ID == "ural1272" ~ 1,
                                       Family_ID != "ural1272" ~ 0)) %>%
  mutate(langs_of_interest = as.factor(langs_of_interest)) %>%
  filter(langs_of_interest == "0")

combination_shaped_1 <- combination %>%
  mutate(Uralic = case_when(Family_ID == "ural1272" ~ "yes",
                            Family_ID != "ural1272" ~ "no")) %>%
  mutate(langs_of_interest = case_when(Family_ID == "ural1272" ~ 1,
                                       Family_ID != "ural1272" ~ 0)) %>%
  mutate(langs_of_interest = as.factor(langs_of_interest)) %>%
  filter(langs_of_interest == "1")

i_labelled <-
  basemap + geom_point(
    data = combination_shaped_0,
    aes(
      x = Longitude,
      y = Latitude,
      color = informativity_st,
      fill = informativity_st
    ),
    size = 1,
    pch = 21,
    # stroke=NA,
    position = position_jitter(
      width = 1,
      height = 1,
      seed = 123
    )
  ) +
  geom_point(
    data = combination_shaped_1,
    aes(x = Longitude, y = Latitude, fill = informativity_st),
    pch = 21,
    size = 2.5,
    colour = "black",
    position = position_jitter(
      width = 1,
      height = 1,
      seed = 123
    )
  ) +
  #scale_fill_gradient2(low = muted("blue"), mid = "white", high = muted("red"), midpoint=0) +
  #scale_color_gradient2(low = muted("blue"), mid = "white", high = muted("red"), midpoint=0, guide="none") +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  scale_color_viridis_c(option = "viridis",
                        guide = "none",
                        direction = -1) +
  scale_shape_manual(values = c(21, 24)) +
  theme(
    text = element_text(size = 20),
    legend.key.size = unit(0.5, units = "cm"),
    legend.position = "right"
  ) +
  guides(fill = guide_colourbar()) +
  labs(fill = "informativity") +
  geom_text_repel(
    aes(x = Longitude, y = Latitude, label = Name),
    #size=5,
    data  = subset(combination, (
      Family_ID == "ural1272" & informativity_st > -0.5
    )),
    size          = 5,
    box.padding   = 0.7, #0.7
    max.overlaps = Inf, #Inf
    point.padding = 0.1
  )
i_labelled

ggsave(
  file = "output_reanalysis/map_informativity_labelled.svg",
  plot = i_labelled,
  width = 10,
  height = 9, 
  dpi=300
)
