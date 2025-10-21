library(openxlsx)
library(ggplot2)

data_plt <- read.xlsx(xlsxFile = "Results tables new/Plot tables.xlsx")

data_plt$Metric <- as.factor(data_plt$Metric)

data_plt2 <- reshape2:::melt.data.frame(data = data_plt, measure.vars = 3:14)

plt_boxPerMetric <- ggplot(data_plt2) +
  stat_boxplot(mapping = aes(
    x = variable,
    y = value
  ), geom = "errorbar", width = 0.25) +
  geom_boxplot(mapping = aes(
    x = variable,
    y = value
  ), show.legend = FALSE, fill = "orange") +
  geom_jitter(mapping = aes(
    x = variable,
    y = value
  ), width = 0.1, alpha = 0.5) +
  facet_wrap(Metric ~., nrow = 3) +
  ggsci::scale_fill_d3(
    name = "Method",
    palette = "category20",
    guide = guide_legend(nrow = 1, position = "top")
  ) +
  scale_x_discrete(name = "", guide = guide_axis(angle = 45)) +
  scale_y_continuous(name = "Rank") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, colour = NA))
plt_boxPerMetric

ggsave(filename = "plt_boxPerMetric.jpeg", plot = plt_boxPerMetric, device = "jpeg", height = 7, width = 6, dpi = 300)
