suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# script was written by Olena Shcherbakova and modified by Sam Passmore

effs_I <-
  read.csv("output_tables_reanalysis/ effects Informativity_reanalysis_social_models .csv")
effs_I$variable <- "informativity"

effs_B <-
  read.csv("output_tables_reanalysis/ effects Boundness_reanalysis_social_models .csv")
effs_B$variable <- "fusion"

effs_B <- effs_B %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)

effs_I <- effs_I %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)

effs_1 <- as.data.frame(rbind(effs_B, effs_I))

effs_1$control <- "yes"

effs_I <-
  read.csv("output_tables_reanalysis/ effects Informativity_reanalysis_social_only_models .csv")
effs_I$variable <- "informativity"

effs_B <-
  read.csv("output_tables_reanalysis/ effects Boundness_reanalysis_social_only_models .csv")
effs_B$variable <- "fusion"

effs_B <- effs_B %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)

effs_I <- effs_I %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)

effs_2 <- as.data.frame(rbind(effs_B, effs_I))
effs_2$control <- "no"

effs_1 <- effs_1 %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)

effs_2 <- effs_2 %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)

effs <- as.data.frame(rbind(effs_1, effs_2))

effs_main <- effs %>%
  rename(lower = 2,
         upper = 4,
         mean = 3) %>%
  filter(!grepl("nonlinear", model)) %>%
  filter(!grepl("SD", effect)) %>%
  filter(!grepl("Intercept", effect))


#removing "(linear)" part within the model column
effs_main$model <- gsub("(\\s*\\(\\w+\\))", "", effs_main$model)

#adding "combined" after L1/L1 proportion where applicable
effs_main$effect <-
  ifelse(
    grepl("L1 speakers\\+", effs_main$model),
    paste(effs_main$effect, "(combined)"),
    effs_main$effect
  )

# effs_main <- effs_main %>%
#   mutate(
#     effect = dplyr::recode(
#       effect,
#       "L2 proportion (combined)" = "L2 (combined)",
#       "L1*L2 proportion" = "L1*L2",
#       "L2 proportion" = "L2"
#     )
#   )

eff_main_plot_df = effs_main %>%
  as.data.frame() %>%
  mutate_if(is.character, as.factor) %>%
  mutate(effect = factor(
    effect,
    levels = c(
      "Education",
      "Neighbours",
      "Official status",
      #"L1*L2",
      "L1*Vehicularity",
      #"L2 (combined)",
      "Vehicularity (combined)",
      "L1 (combined)",
      #"L2",
      "Vehicularity",
      "L1"
    )
  )) %>%
  mutate(importance = case_when((lower < 0 &
                                   upper < 0)  ~ "negative",
                                (lower > 0 &
                                   upper > 0)  ~ "positive",
                                (lower < 0 & upper > 0) |
                                  (lower < 0 &
                                     upper == 0) |
                                  (lower == 0 & upper > 0) ~ "no"
  )) %>%
  mutate(importance = as.factor(importance)) %>%
  mutate(importance = factor(
    importance,
    levels = c("no", "positive", "negative"),
    ordered = TRUE
  )) %>% 
  mutate(control = if_else(control == "yes", "control", "no control")) %>% 
  mutate(control = factor(control, levels = c("control", "no control"), ordered = TRUE))

dodge_width <- 0.8

effs_main_plot_bw <- ggplot(eff_main_plot_df,
                            aes(
                              y = effect,
                              x = mean,
                              linetype = control,
                              color = importance
                            )) +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    width = 0.5,
    linewidth = 3,
    position = position_dodge(width = dodge_width)
  ) +
  geom_point(size = 10,
             position = position_dodge(width = dodge_width)) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_vline(
    aes(xintercept = 0),
    linetype = 2,
    linewidth = 1,
    alpha = 0.7
  ) +
  scale_color_manual(values = c("black", "red3", "steelblue")) +
  ylab("") +
  xlab("Estimate: 95% credible interval") +
  #  theme_light() +
  theme_minimal() +
  facet_grid(. ~ variable, scales = "free_x", space = "free") +
  theme(
    text = element_text(size = 55), # face = "bold"),
    #    legend.text = element_text(size = 25),
    axis.title = element_text(size = 50),
    #    legend.title = element_text(size = 25),
    #    strip.text.x = element_text(size = 25),
    legend.spacing.y = unit(2.7, 'cm'),
    legend.key.width = unit(2, 'cm'),
    legend.direction = "horizontal",
    legend.position = "top",
    legend.title = element_blank(),
    axis.line = element_line(linewidth = 1),
    strip.background = element_rect(color = "black", linewidth = 2),
    panel.spacing.x = unit(15, "mm"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "gray 50", fill = NA)
  ) +
  guides(color = "none")


effs_main_plot_bw

ggsave(
  file = "output_reanalysis/effects_plot.png",
  plot = effs_main_plot_bw,
  height = 24,
  width = 30,
  dpi = 400
)


effs_main_plot_bw
ggsave(
  file = "output_reanalysis/effects_plot.pdf",
  plot = effs_main_plot_bw,
  height = 22,
  width = 36,
  dpi = 600
)

effs_main_plot_bw
ggsave(
  file = "output_reanalysis/effects_plot.jpeg",
  plot = effs_main_plot_bw,
  height = 22,
  width = 36,
  dpi = 600
)
