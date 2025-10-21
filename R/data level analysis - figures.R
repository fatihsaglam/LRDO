library(openxlsx)
library(ggplot2)
library(tidyverse)

data <- read.xlsx(xlsxFile = "Results tables new/Plot tables.xlsx")

data_long <- data %>%
  pivot_longer(
    cols = c(NORES, ROS, SMOTE, RWO, ROSE, GSMOTE, SMOTEWB, SyMProD, GQEO, EBSMOTE, deepSMOTE, LRMV),
    names_to = "Resampler",
    values_to = "Rank"
  ) %>%
  select(Metric, Dataset, Resampler, Rank, n, p, imb_rate, dimensionality)

data_long$Resampler <- factor(data_long$Resampler,
                              levels = c("NORES", "ROS", "SMOTE", "RWO", "ROSE", "GSMOTE", "SMOTEWB", "SyMProD", "GQEO", "EBSMOTE", "deepSMOTE", "LRMV"),
                              labels = c("NORES", "ROS", "SMOTE", "RWO", "ROSE", "GSMOTE", "SMOTEWB", "SyMProD", "GQEO", "EBSMOTE", "deepSMOTE", "LRDO"))

plt_n_vs_rank <- ggplot(data = data_long) +
  geom_point(mapping = aes(
    x = n, 
    y = Rank,
    fill = Resampler
  ), shape = 21, stroke = 0.25, size = 2) +
  geom_smooth(mapping = aes(
    x = n, 
    y = Rank,
    color = Resampler
  ), se = FALSE, method = "gam", formula = y ~ s(x, k = 5), linewidth = 1) +
  scale_color_manual(values = rev(ggsci::pal_d3(palette = "category20")(12)), guide = guide_legend(nrow = 1)) +
  scale_fill_manual(values = rev(ggsci::pal_d3(palette = "category20")(12))) +
  facet_grid(cols = vars(Metric)) + 
  scale_x_log10() +
  labs(x = expression("Dataset size " ~ (italic(n))), y = "Average rank (lower is better)",
         color = "Resampler", fill = "Resampler") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, colour = NA),
        legend.position = "top", 
        legend.key.size = unit(0, "cm"))



plt_p_vs_rank <- ggplot(data = data_long) +
  geom_point(mapping = aes(
    x = p, 
    y = Rank,
    fill = Resampler
  ), shape = 21, stroke = 0.25, size = 2) +
  geom_smooth(mapping = aes(
    x = p, 
    y = Rank,
    color = Resampler
  ), se = FALSE, method = "gam", formula = y ~ s(x, k = 5), linewidth = 1) +
  scale_color_manual(values = rev(ggsci::pal_d3(palette = "category20")(12)), guide = guide_legend(nrow = 1)) +
  scale_fill_manual(values = rev(ggsci::pal_d3(palette = "category20")(12))) +
  facet_grid(cols = vars(Metric)) + 
  scale_x_log10() +
  labs(x = expression("Number of features " ~ (italic(p))), y = "Average rank (lower is better)",
       color = "Resampler", fill = "Resampler") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, colour = NA),
        legend.position = "top", 
        legend.key.size = unit(0, "cm"))




plt_imb_rate_vs_rank <- ggplot(data = data_long) +
  geom_point(mapping = aes(
    x = imb_rate, 
    y = Rank,
    fill = Resampler
  ), shape = 21, stroke = 0.25, size = 2) +
  geom_smooth(mapping = aes(
    x = imb_rate, 
    y = Rank,
    color = Resampler
  ), se = FALSE, method = "gam", formula = y ~ s(x, k = 5), linewidth = 1) +
  scale_color_manual(values = rev(ggsci::pal_d3(palette = "category20")(12)), guide = guide_legend(nrow = 1)) +
  scale_fill_manual(values = rev(ggsci::pal_d3(palette = "category20")(12))) +
  facet_grid(cols = vars(Metric)) + 
  scale_x_log10() +
  labs(x = expression("Class imbalance ratio " ~ (italic(n)[maj] / italic(n)[min])), y = "Average rank (lower is better)",
       color = "Resampler", fill = "Resampler") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, colour = NA),
        legend.position = "top", 
        legend.key.size = unit(0, "cm"))


plt_dimensionality_vs_rank <- ggplot(data = data_long) +
  geom_point(mapping = aes(
    x = dimensionality, 
    y = Rank,
    fill = Resampler
  ), shape = 21, stroke = 0.25, size = 2) +
  geom_smooth(mapping = aes(
    x = dimensionality, 
    y = Rank,
    color = Resampler
  ), se = FALSE, method = "gam", formula = y ~ s(x, k = 5), linewidth = 1) +
  scale_color_manual(values = rev(ggsci::pal_d3(palette = "category20")(12)), guide = guide_legend(nrow = 1)) +
  scale_fill_manual(values = rev(ggsci::pal_d3(palette = "category20")(12))) +
  facet_grid(cols = vars(Metric)) + 
  scale_x_log10() +
  labs(x = expression("Dimensionality " ~ (italic(p)/italic(n))), y = "Average rank (lower is better)",
       color = "Resampler", fill = "Resampler") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, colour = NA),
        legend.position = "top", 
        legend.key.size = unit(0, "cm"))


ggsave(filename = "figs/plt_n_vs_rank.jpeg", plot = plt_n_vs_rank, device = "jpeg", 
       width = 8*1.3 ,height = 3*1.3, dpi = 300)
ggsave(filename = "figs/plt_p_vs_rank.jpeg", plot = plt_p_vs_rank, device = "jpeg", 
       width = 8*1.3 ,height = 3*1.3, dpi = 300)
ggsave(filename = "figs/plt_imb_rate_vs_rank.jpeg", plot = plt_imb_rate_vs_rank, device = "jpeg", 
       width = 8*1.3 ,height = 3*1.3, dpi = 300)
ggsave(filename = "figs/plt_dimensionality_vs_rank.jpeg", plot = plt_dimensionality_vs_rank, device = "jpeg", 
       width = 8*1.3 ,height = 3*1.3, dpi = 300)
