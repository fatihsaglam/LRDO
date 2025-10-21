source(file = "resamplerCls_EBSMOTE.R")
source(file = "resamplerCls_LRDO.R")
source(file = "resamplerCls_SyMProD.R")
source(file = "resamplerCls_deepSMOTE.R")
source(file = "resamplerCls_GQEO.R")

resamplerCls_all <- list(
  NORES = list(
    fun = function(x, y, ...) {list(x_new = x, y_new = y)},
    hypGrid = expand.grid(dummy = 1)[, 0]
  ),
  ROS = list(
    fun = SMOTEWB::ROS,
    hypGrid = expand.grid(
      ovRate = c(0.1, 0.25, 0.5, 1, 1.5, 2)
    )
  ),
  SMOTE = list(
    fun = SMOTEWB::SMOTE,
    hypGrid = expand.grid(
      k = c(3, 5, 7, 9),
      ovRate = c(0.1, 0.25, 0.5, 1, 1.5, 2)
    )
  ),
  RWO = list(
    fun = SMOTEWB::RWO,
    hypGrid = expand.grid(
      ovRate = c(0.1, 0.25, 0.5, 1, 1.5, 2)
    )
  ),
  ROSE = list(
    fun = SMOTEWB::ROSE,
    hypGrid = expand.grid(
      h = c(0.5, 1, 1.5),
      ovRate = c(0.1, 0.25, 0.5, 1, 1.5, 2)
    )
  ),
  GSMOTE = list(
    fun = SMOTEWB::GSMOTE,
    hypGrid = expand.grid(
      k = c(3, 5, 7, 9),
      # alpha_sel = c("majority", "minority", "combined"),
      alpha_trunc = c(-1, -0.5, 0, 0.5, 1),
      alpha_def = c(-1, -0.5, 0, 0.5, 1),
      ovRate = c(0.1, 0.25, 0.5, 1, 1.5, 2)
    )
  ),
  SMOTEWB = list(
    fun = SMOTEWB::SMOTEWB,
    hypGrid = expand.grid(
      n_weak_classifier = c(50, 100),
      ovRate = c(0.1, 0.25, 0.5, 1, 1.5, 2)
    )
  ),
  SyMProD = list(
    fun = resamplerCls_SyMProD,
    hypGrid = expand.grid(
      NT = c(1, 2, 3, 4),
      CT = c(0, 0.5, 1),
      K = c(3, 5, 7),
      M = c(3, 5, 7),
      ovRate = c(0.1, 0.25, 0.5, 1, 1.5, 2)
    )
  ),
  GQEO = list(
    fun = resamplerCls_GQEO,
    hypGrid = expand.grid(
      k = c(3, 5, 7, 9),
      ovRate = c(0.1, 0.25, 0.5, 1, 1.5, 2)
    )
  ),
  EBSMOTE = list(
    fun = resamplerCls_EBSMOTE,
    hypGrid = expand.grid(
      m_alpha = c(3, 5, 7, 9),
      m_beta = c(3, 5, 7, 9),
      lambda = c(2.5, 3, 3.5, 4),
      v_alpha = c(3, 5, 7, 9),
      w_alpha = c(3, 5, 7, 9),
      expand_alpha = c(-1, -0.5, 0, 0.5, 1),
      ovRate = c(0.1, 0.25, 0.5, 1, 1.5, 2)
    )
  ),
  deepSMOTE = list(
    fun = resamplerCls_deepSMOTE,
    hypGrid = expand.grid(
      k = c(3, 5, 7, 9),
      epochs = c(50, 100, 200),
      batch_size = c(32L, 64L),
      hidden_dim = c(8L, 16L, 32L, 64L),
      latent_dim = c(8L, 16L, 32L, 64L),
      ovRate = c(0.1, 0.25, 0.5, 1, 1.5, 2)
    )
  ),
  LRDO = list(
    fun = resamplerCls_LRDO,
    hypGrid = expand.grid(
      k = c(3, 5, 7, 9),
      # type = c("type 1", "type 2"),
      ovRate = c(0.1, 0.25, 0.5, 1, 1.5, 2)
    )
  )
)