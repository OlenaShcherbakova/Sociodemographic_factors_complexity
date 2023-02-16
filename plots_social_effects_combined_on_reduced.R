suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# script was written by Olena Shcherbakova and modified by Sam Passmore

effs_I <-
  read.csv("output_tables_reduced/ effects Informativity_social_models .csv")
effs_I$variable <- "informativity"

effs_B <-
  read.csv("output_tables_reduced/ effects Boundness_social_models .csv")
effs_B$variable <- "fusion"

effs_1 <- as.data.frame(rbind(effs_B, effs_I))

effs_1$control <- "yes"

effs_I <-
  read.csv("output_tables_reduced/ effects Informativity_social_only_models .csv")
effs_I$variable <- "informativity"

effs_B <-
  read.csv("output_tables_reduced/ effects Boundness_social_only_models .csv")
effs_B$variable <- "fusion"

effs_2 <- as.data.frame(rbind(effs_B, effs_I))
effs_2$control <- "no"

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

effs_main <- effs_main %>%
  mutate(effect = dplyr::recode(
    effect,
    "L2 proportion (combined)" = "L2 (combined)",
    "L2 proportion" = "L2"
  ))

eff_main_plot_df = effs_main %>%
  as.data.frame() %>%
  mutate_if(is.character, as.factor) %>%
  mutate(effect = factor(
    effect,
    levels = c(
      "Education",
      "Neighbours",
      "Official status",
      "L2 (combined)",
      "L1 (combined)",
      "L2",
      "L1"
    )
  )) %>%
  mutate(importance = case_when((lower < 0 &
                                   upper < 0)  ~ "negative",
                                (lower > 0 &
                                   upper > 0)  ~ "positive",
                                (lower < 0 & upper > 0) |
                                  (lower < 0 &
                                     upper == 0) | (lower == 0 & upper > 0) ~ "no"
  )) %>%
  mutate(importance = as.factor(importance)) %>%
  mutate(importance = factor(
    importance,
    levels = c("no", "positive", "negative"),
    ordered = TRUE
  )) %>%
  mutate(control = factor(control, levels = c("yes", "no"), ordered = TRUE))

effs_main_plot_bw = ggplot(eff_main_plot_df,
                           aes(
                             y = effect,
                             x = mean,
                             linetype = control,
                             color = importance
                           )) +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    width = 0.6,
    size = 2.5,
    position = position_dodge(w = 0.8)
  ) +
  geom_point(size = 10,
             position = position_dodge(w = 0.8)) +
  geom_line(position = position_dodge(w = 0.8)) +
  geom_vline(aes(xintercept = 0),
             lty = 2) +
  scale_color_manual(values = c("black", "red3", "steelblue")) +
  ylab("") +
  xlab("Estimate: 95% credible interval") +
  theme_classic() +
  facet_grid(. ~ variable, scales = "free_x", space = "free") +
  theme(
    axis.text = element_text(size = 65),
    legend.text = element_text(size = 65),
    axis.title = element_text(size = 65),
    legend.title = element_text(size = 65),
    strip.text.x = element_text(size = 65),
    legend.spacing.y = unit(2.7, 'cm'),
    legend.key.size = unit(2, 'cm'),
    legend.direction = "horizontal",
    legend.position = "top",
    panel.spacing.x = unit(15, "mm")
  ) +
  guides(color = "none")

effs_main_plot_bw
ggsave(
  file = "output_reduced/effects_plot.svg",
  plot = effs_main_plot_bw,
  height = 22,
  width = 36,
  dpi = 300
)

effs_main_plot_bw
ggsave(
  file = "output_reduced/effects_plot.jpeg",
  plot = effs_main_plot_bw,
  height = 22,
  width = 36,
  dpi = 300
)
