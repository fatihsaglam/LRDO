# --- Libraries ---
library(ggplot2)
library(ggforce)
library(ggsci)
library(patchwork)
library(FNN)
library(dplyr)

# --- Source function ---
source("resamplerCls_LRDO.R")

# --- Data ---
set.seed(1)
x <- imbalance::banana[, 1:2]
y <- imbalance::banana[, 3]
data <- data.frame(x1 = x[, 1], x2 = x[, 2], Class = y)

# --- Helper: COMEDIAN covariance ---
covComed <- function(X, n.iter = 2) {
  COM <- function(X) {
    p <- ncol(X)
    med <- robustbase::colMedians(X)
    Y <- sweep(X, 2, med, "-")
    COM <- matrix(0, p, p)
    madY <- apply(Y, 2, mad)
    for (i in 1:p) {
      for (j in seq_len(i)) {
        COM[i, j] <- COM[j, i] <- median(Y[, i] * Y[, j]) / (madY[i] * madY[j])
      }
    }
    1.4826^2 * COM
  }
  rho <- COM(X)
  eig <- eigen(rho)
  mu <- robustbase::colMedians(X)
  Sigma <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)
  list(mu = mu, Sigma = Sigma)
}

# --- Helper: ellipse parameters from mean/cov ---
ellipse_data <- function(mu, Sigma) {
  eig <- eigen(Sigma)
  scale_factor <- sqrt(qchisq(0.68, df = 2))  # 68% probability contour
  data.frame(
    x0 = mu[1],
    y0 = mu[2],
    a = scale_factor * sqrt(eig$values[1]),
    b = scale_factor * sqrt(eig$values[2]),
    angle = atan2(eig$vectors[2, 1], eig$vectors[1, 1])
  )
}

# --- Run LRDO with various settings ---
m_LRDO_k5_ovRate1  <- resamplerCls_LRDO(x = x, y = y, k = 5,  ovRate = 1,  giveEllipses = TRUE)
m_LRDO_k5_ovRate10 <- resamplerCls_LRDO(x = x, y = y, k = 5,  ovRate = 10, giveEllipses = TRUE)
m_LRDO_k15_ovRate10<- resamplerCls_LRDO(x = x, y = y, k = 15, ovRate = 10, giveEllipses = TRUE)

# --- Convert ellipse lists ---
ellipse_data_frame <- function(Elist) {
  tmp <- t(sapply(Elist, \(m) ellipse_data(m$mu, m$Sigma)))
  tmp <- as.data.frame(tmp)
  for (i in 1:ncol(tmp)) tmp[, i] <- unlist(tmp[, i])
  tmp
}
data_ellipses_k5  <- ellipse_data_frame(m_LRDO_k5_ovRate1$Ellipses)
data_ellipses_k15 <- ellipse_data_frame(m_LRDO_k15_ovRate10$Ellipses)

# --- Colors ---
cols <- ggsci::pal_d3()(2)
names(cols) <- c("Majority", "Minority")

# --- Base style ---
base_theme <- theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0.5, face = "bold"),
    plot.title = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 10)
  )

# --- Panels ---
plt_nores <- ggplot(data, aes(x1, x2, fill = Class)) +
  geom_point(shape = 21, stroke = 0.2, size = 2) +
  ggsci::scale_fill_d3(labels = names(cols)) +
  labs(title = "Original dataset (no resampling)", caption = "a)", x = "x₁", y = "x₂") +
  base_theme

plt_k5_circles <- ggplot(data, aes(x1, x2, fill = Class)) +
  geom_point(shape = 21, stroke = 0.2, size = 2) +
  geom_ellipse(data = data_ellipses_k5, aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle),
               inherit.aes = FALSE, alpha = 0) +
  ggsci::scale_fill_d3(labels = names(cols)) +
  labs(title = "Local robust covariance regions (k = 5)", caption = "b)", x = "x₁", y = "x₂") +
  base_theme

plt_k5_ovRate1 <- ggplot(data, aes(x1, x2, fill = Class)) +
  geom_point(shape = 21, stroke = 0.2, size = 2) +
  geom_point(data = data.frame(m_LRDO_k5_ovRate1$x_syn),
             aes(x = At1, y = At2), color = "green3", shape = 3, size = 1.6, inherit.aes = FALSE) +
  ggsci::scale_fill_d3(labels = names(cols)) +
  labs(title = "LRDO synthetic samples (k = 5, ovRate = 1)",
       caption = "c)", x = "x₁", y = "x₂") +
  base_theme

plt_k5_ovRate10 <- ggplot(data, aes(x1, x2, fill = Class)) +
  geom_point(shape = 21, stroke = 0.2, size = 2) +
  geom_point(data = data.frame(m_LRDO_k5_ovRate10$x_syn),
             aes(x = At1, y = At2), color = "green3", shape = 3, size = 1.6, inherit.aes = FALSE) +
  geom_ellipse(data = data_ellipses_k5, aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle),
               inherit.aes = FALSE, alpha = 0, linewidth = 0.25) +
  ggsci::scale_fill_d3(labels = names(cols)) +
  labs(title = "LRDO synthetic samples (k = 5, ovRate = 10)",
       caption = "d)", x = "x₁", y = "x₂") +
  base_theme

plt_k15_circles <- ggplot(data, aes(x1, x2, fill = Class)) +
  geom_point(shape = 21, stroke = 0.2, size = 2) +
  geom_ellipse(data = data_ellipses_k15, aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle),
               inherit.aes = FALSE, alpha = 0, linewidth = 0.25) +
  ggsci::scale_fill_d3(labels = names(cols)) +
  labs(title = "Local robust covariance regions (k = 15)", caption = "e)", x = "x₁", y = "x₂") +
  base_theme

plt_k15_ovRate10 <- ggplot(data, aes(x1, x2, fill = Class)) +
  geom_point(shape = 21, stroke = 0.2, size = 2) +
  geom_point(data = data.frame(m_LRDO_k15_ovRate10$x_syn),
             aes(x = At1, y = At2), color = "green3", shape = 3, size = 1.6, inherit.aes = FALSE) +
  geom_ellipse(data = data_ellipses_k15, aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle),
               inherit.aes = FALSE, alpha = 0, linewidth = 0.25) +
  ggsci::scale_fill_d3(labels = names(cols)) +
  labs(title = "LRDO synthetic samples (k = 15, ovRate = 10)",
       caption = "f)", x = "x₁", y = "x₂") +
  base_theme

# --- Combine panels ---
# final_plot <- (
#   plt_nores + plt_k5_circles + plt_k5_ovRate1 +
#     plt_k5_ovRate10 + plt_k15_circles + plt_k15_ovRate10
# ) + plot_layout(ncol = 3, byrow = TRUE)

final_plot <- ggpubr::ggarrange(
  plt_nores, plt_k5_circles, plt_k5_ovRate1, 
  plt_k5_ovRate10, plt_k15_circles, plt_k15_ovRate10,
  nrow = 2, ncol = 3, common.legend = TRUE
)

# --- Save & display ---
ggsave("figs/LRDO behavior.jpeg", final_plot, device = "jpeg", width = 9*1.3, height = 6*1.3, dpi = 300)
final_plot
