# --- Libraries ---
library(ggplot2)
library(ggforce)
library(ggsci)
library(patchwork)
library(FNN)
library(dplyr)

# --- Source function ---
source("resamplerCls_LRDO.R")
source("resamplers.R")

# --- Data ---
set.seed(1)
x <- imbalance::banana[, 1:2]
y <- imbalance::banana[, 3]
data <- data.frame(x1 = x[, 1], x2 = x[, 2], Class = y)


plt_list <- lapply(1:length(resamplerCls_all), \(i_resampler) {
  print(names(resamplerCls_all)[i_resampler])
  resampler <- resamplerCls_all[[i_resampler]]$fun
  
  m_res <- resampler(x = x, y = y)
  
  ggplot(data, aes(x1, x2, fill = Class)) +
    geom_point(shape = 21, stroke = 0.2, size = 2) +
    {
      if (is.null(m_res$x_syn))
      {}
      else 
      {geom_point(data = as.data.frame(m_res$x_syn), aes(x = m_res$x_syn[,1], y = m_res$x_syn[,2]), color = "green3", shape = 3, size = 1.6, inherit.aes = FALSE)}
    } +
    ggsci::scale_fill_d3(labels = "") +
    labs(title = names(resamplerCls_all)[i_resampler],
         # caption = paste0(letters[i_resampler],")"), x = "x₁", y = "x₂") +
         caption = paste0(letters[i_resampler],")"), x = NULL, y = NULL) +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      plot.caption = element_text(size = 10, hjust = 0.5, face = "bold"),
      plot.title = element_text(size = 11, hjust = 0.5),
      axis.title = element_text(size = 10)
    )
})


plt_dataGenComparison_all <- ggpubr::ggarrange(
  plotlist = plt_list, 
  nrow = 3, ncol = 4, common.legend = TRUE
)

ggsave(filename = "figs/plt_dataGenComparison_all.jpeg", plot = plt_dataGenComparison_all, width = 9, height = 7)
