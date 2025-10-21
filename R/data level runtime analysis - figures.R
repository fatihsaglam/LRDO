library(openxlsx)
library(ggplot2)
library(tidyverse)

data <- read.xlsx(xlsxFile = "Results tables new/Plot tables.xlsx", sheet = "runtime")

data_long <- data %>%
  pivot_longer(
    cols = c(NORES, ROS, SMOTE, RWO, ROSE, GSMOTE, SMOTEWB, SyMProD, GQEO, EBSMOTE, deepSMOTE, LRMV),
    names_to = "Resampler",
    values_to = "Runtime"
  ) %>%
  select(Dataset, Resampler, Runtime, n, p, IR, dimensionality) %>%
  filter(Resampler != "NORES")

data_long$Resampler <- factor(data_long$Resampler, levels = c("ROS", "SMOTE", "RWO", "ROSE", "GSMOTE", "SMOTEWB", "SyMProD", "GQEO", "EBSMOTE", "deepSMOTE", "LRMV"),
                              labels = c("ROS", "SMOTE", "RWO", "ROSE", "GSMOTE", "SMOTEWB", "SyMProD", "GQEO", "EBSMOTE", "deepSMOTE", "LRDO"))
data_long$Runtime <- data_long$Runtime + 1e-10


plt_n_vs_runtime <- ggplot(data = data_long) +
  geom_point(mapping = aes(
    x = n, 
    y = Runtime,
    fill = Resampler
  ), shape = 21, stroke = 0.25, size = 2) +
  geom_smooth(mapping = aes(
    x = n, 
    y = Runtime,
    color = Resampler
  ), se = FALSE, method = "gam", formula = y ~ s(x, k = 5), linewidth = 1) +
  scale_color_manual(values = rev(ggsci::pal_d3(palette = "category20")(12)), guide = guide_legend(nrow = 1)) +
  scale_fill_manual(values = rev(ggsci::pal_d3(palette = "category20")(12))) +
  scale_x_log10() +
  labs(x = expression("Dataset size " ~ (italic(n))), y = "Average runtime (lower is better)",
       color = "Resampler", fill = "Resampler", caption = "a)") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, colour = NA),
        legend.position = "top", 
        legend.key.size = unit(0, "cm"), plot.caption = element_text(size = 11, hjust = 0.5))

plt_p_vs_runtime <- ggplot(data = data_long) +
  geom_point(mapping = aes(
    x = p, 
    y = Runtime,
    fill = Resampler
  ), shape = 21, stroke = 0.25, size = 2) +
  geom_smooth(mapping = aes(
    x = p, 
    y = Runtime,
    color = Resampler
  ), se = FALSE, method = "gam", formula = y ~ s(x, k = 5), linewidth = 1) +
  scale_color_manual(values = rev(ggsci::pal_d3(palette = "category20")(12)), guide = guide_legend(nrow = 1)) +
  scale_fill_manual(values = rev(ggsci::pal_d3(palette = "category20")(12))) +
  scale_x_log10() +
  labs(x = expression("Number of features " ~ (italic(p))), y = "Average runtime (lower is better)",
       color = "Resampler", fill = "Resampler", caption = "b)") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, colour = NA),
        legend.position = "top", 
        legend.key.size = unit(0, "cm"), plot.caption = element_text(size = 11, hjust = 0.5))


plt_imb_rate_vs_runtime <- ggplot(data = data_long) +
  geom_point(mapping = aes(
    x = IR, 
    y = Runtime,
    fill = Resampler
  ), shape = 21, stroke = 0.25, size = 2) +
  geom_smooth(mapping = aes(
    x = IR, 
    y = Runtime,
    color = Resampler
  ), se = FALSE, method = "gam", formula = y ~ s(x, k = 5), linewidth = 1) +
  scale_color_manual(values = rev(ggsci::pal_d3(palette = "category20")(12)), guide = guide_legend(nrow = 1)) +
  scale_fill_manual(values = rev(ggsci::pal_d3(palette = "category20")(12))) +
  scale_x_log10() +
  labs(x = expression("Class imbalance ratio " ~ (italic(n)[maj] / italic(n)[min])), y = "Average runtime (lower is better)",
       color = "Resampler", fill = "Resampler", caption = "c)") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, colour = NA),
        legend.position = "top", 
        legend.key.size = unit(0, "cm"), plot.caption = element_text(size = 11, hjust = 0.5))


plt_dimensionality_vs_runtime <- ggplot(data = data_long) +
  geom_point(mapping = aes(
    x = dimensionality, 
    y = Runtime,
    fill = Resampler
  ), shape = 21, stroke = 0.25, size = 2) +
  geom_smooth(mapping = aes(
    x = dimensionality, 
    y = Runtime,
    color = Resampler
  ), se = FALSE, method = "gam", formula = y ~ s(x, k = 5), linewidth = 1) +
  scale_color_manual(values = rev(ggsci::pal_d3(palette = "category20")(12)), guide = guide_legend(nrow = 1)) +
  scale_fill_manual(values = rev(ggsci::pal_d3(palette = "category20")(12))) +
  scale_x_log10() +
  labs(x = expression("Dimensionality " ~ (italic(p)/italic(n))), y = "Average runtime (lower is better)",
       color = "Resampler", fill = "Resampler", caption = "d)") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, colour = NA),
        legend.position = "top", 
        legend.key.size = unit(0, "cm"), plot.caption = element_text(size = 11, hjust = 0.5))

plt_combined <- ggpubr::ggarrange(
  plt_n_vs_runtime, plt_p_vs_runtime, plt_imb_rate_vs_runtime, plt_dimensionality_vs_runtime,
  nrow = 1, ncol = 4, common.legend = TRUE, legend = "top"
)

ggsave(filename = "figs/plt_properties_vs_runtime.jpeg", plot = plt_combined, device = "jpeg", 
       width = 12*1.2, height = 3*1.2, dpi = 300)
