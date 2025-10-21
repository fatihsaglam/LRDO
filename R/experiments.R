library(jsonlite)
library(openxlsx)
library(kernlab)

# === Load your components ===
source("metrics.R")        # metricCls_all list: each metric function(test_y, pred, prob)
source("resamplers.R")     # resamplerCls_all list: each $fun + $hypGrid
source("classifiers.R")    # classifiers_all list: each $train + $predict
source("preprocess_dataset.R")  # preprocess_dataset(df, min_class_samples, var_threshold)

datasets    <- readRDS("datasets_classification_binary.rds")
resamplers  <- resamplerCls_all
classifiers <- classifiers_all
main_metric <- metricCls_all$ROCAUC

set.seed(42)

# === Helper: Stratified K-fold ===
make_stratified_folds <- function(y, K = 10L, seed = 42) {
  set.seed(seed)
  y <- factor(y)
  lvl <- levels(y)
  idx_list <- vector("list", K)
  for (i in 1:K) idx_list[[i]] <- integer(0)
  for (li in seq_along(lvl)) {
    ids <- which(y == lvl[li])
    ids <- sample(ids, length(ids))
    splits <- split(ids, rep(1:K, length.out = length(ids)))
    for (k in 1:K) idx_list[[k]] <- c(idx_list[[k]], splits[[k]])
  }
  for (k in 1:K) idx_list[[k]] <- sort(idx_list[[k]])
  idx_list
}

# === Helper: Random search tuning with 3-fold CV ===
tune_resampler_random <- function(x, y, res_entry, n_iter = 10, seed = 42) {
  set.seed(seed)
  hyp_grid <- res_entry$hypGrid
  if (nrow(hyp_grid) == 0L) return(list())  # no tunables
  
  # 3-fold stratified CV indices (fixed for this tuning run)
  folds <- make_stratified_folds(y, K = 3L, seed = seed)
  
  best_score  <- -Inf
  best_params <- list()
  
  cat("|")  # progress bar
  for (iter in 1:n_iter) {
    cat("=")
    
    # sample one row from the grid
    params <- as.list(hyp_grid[sample(seq_len(nrow(hyp_grid)), 1L), , drop = FALSE])
    
    # evaluate params via 3-fold CV
    fold_scores <- numeric(length(folds))
    for (k in seq_along(folds)) {
      val_idx   <- folds[[k]]
      train_idx <- setdiff(seq_len(nrow(x)), val_idx)
      
      train_x <- x[train_idx, , drop = FALSE]
      train_y <- y[train_idx]
      val_x   <- x[val_idx, , drop = FALSE]
      val_y   <- y[val_idx]
      
      # resample on training fold
      res <- tryCatch(
        do.call(res_entry$fun, c(list(train_x, train_y), params)),
        error = function(e) NULL
      )
      if (is.null(res)) { fold_scores[k] <- NA_real_; next }
      
      # CART train on resampled data
      clf   <- classifiers$CART
      model <- tryCatch(clf$train(res$x_new, res$y_new), error = function(e) NULL)
      if (is.null(model)) { fold_scores[k] <- NA_real_; next }
      
      # predict on validation fold
      pred <- tryCatch(clf$predict(model, val_x), error = function(e) NULL)
      if (is.null(pred)) { fold_scores[k] <- NA_real_; next }
      
      # metric for this fold
      fold_scores[k] <- tryCatch(
        main_metric(val_y, prob = pred$prob),
        error = function(e) NA_real_
      )
    } # folds
    
    mean_score <- mean(fold_scores, na.rm = TRUE)
    if (is.finite(mean_score) && mean_score > best_score) {
      best_score  <- mean_score
      best_params <- params
    }
  } # iterations
  cat("|\n")
  
  best_params
}

# === Prepare empty results dataframe ===
metric_names <- c(names(metricCls_all), "Runtime")
resampler_names <- names(resamplers)
results_df <- expand.grid(
  dataset    = names(datasets),
  classifier = names(classifiers),
  metric     = metric_names,
  stringsAsFactors = FALSE
)
results_df$best_params <- NA_character_
for (rn in resampler_names) results_df[[rn]] <- NA_real_

results_df <- openxlsx::read.xlsx(xlsxFile = "results_all.xlsx")

set.seed(42)

# === Main loop ===
for (i_dataset in 1:length(datasets)) {
  ds_name <- names(datasets)[i_dataset]
  cat("\n=== Dataset:", ds_name, "| Dataset No:", i_dataset, "===\n")
  
  # ---- Preprocess ----
  data <- preprocess_dataset(
    datasets[[i_dataset]],
    min_class_samples = 11,
    var_threshold = 1e-4
  )
  if (is.null(data)) {
    cat(" Skipped: not enough samples or zero-var features.\n")
    next
  }
  
  x <- data[, -ncol(data), drop = FALSE]
  y <- data[,  ncol(data)]
  stopifnot(is.factor(y))
  
  # ---- Tune resamplers ----
  best_params_all <- list()
  for (i_res in seq_along(resamplers)) {
    res_name <- names(resamplers)[i_res]
    cat(" Tuning:", res_name)
    if (res_name == "NORES") {
      best_params_all[[res_name]] <- list()
    } else {
      best_params_all[[res_name]] <- tune_resampler_random(
        x, y, res_entry = resamplers[[i_res]], n_iter = 20
      )
    }
  }
  
  # ---- Create folds ----
  folds <- make_stratified_folds(y, K = 10L, seed = 123)
  
  # ---- Initialize storage ----
  runtimes_list <- setNames(vector("list", length(resamplers)), names(resamplers))
  metric_accum <- lapply(metric_names, function(mn) {
    setNames(lapply(names(resamplers), function(rn) {
      setNames(vector("list", length(classifiers)), names(classifiers))
    }), names(resamplers))
  })
  names(metric_accum) <- metric_names
  
  # ---- Fold loop ----
  for (k in seq_along(folds)) {
    train_idx <- setdiff(seq_len(nrow(x)), folds[[k]])
    test_idx  <- folds[[k]]
    test_x <- x[test_idx, , drop = FALSE]
    test_y <- y[test_idx]
    
    # --- Resampler loop ---
    for (i_res in seq_along(resamplers)) {
      res_name <- names(resamplers)[i_res]
      cat("Fold: ", k, "| Trainig:", res_name, "|")
      params <- best_params_all[[res_name]]
      
      # Resample train set
      t_res <- system.time({
        resampled_obj <- tryCatch(
          do.call(resamplers[[i_res]]$fun,
                  c(list(x[train_idx, , drop = FALSE], y[train_idx]), params)),
          error = function(e) NULL
        )
      })["elapsed"]
      
      runtimes_list[[res_name]] <- c(runtimes_list[[res_name]], as.numeric(t_res))
      if (is.null(resampled_obj)) stop("Resampler null")
      
      train_x_res <- resampled_obj$x_new
      train_y_res <- resampled_obj$y_new
      
      # --- Classifier loop ---
      for (i_clf in 1:length(classifiers)) {
      # for (i_clf in seq_along(classifiers)) {
        cat("=")
        clf_name <- names(classifiers)[i_clf]
        clf <- classifiers[[i_clf]]
        
        model <- tryCatch(clf$train(train_x_res, train_y_res), error = function(e) NULL)
        if (is.null(model)) stop("Model null")
        
        pred <- tryCatch(clf$predict(model, test_x), error = function(e) NULL)
        if (is.null(pred)) stop("Model null")
        
        # --- Metric loop ---
        for (mn in metric_names) {
          if (mn == "Runtime") next  # Runtime handled later
          
          score <- tryCatch(
            metricCls_all[[mn]](test_y, pred = pred$pred, prob = pred$prob),
            error = function(e) NA_real_
          )
          metric_accum[[mn]][[res_name]][[clf_name]] <- 
            c(metric_accum[[mn]][[res_name]][[clf_name]], score)
        }
      }
      cat("|\n")
    }
  }
  
  # ---- Store results in results_df ----
  for (mn in metric_names) {
    for (i_clf in seq_along(classifiers)) {
      clf_name <- names(classifiers)[i_clf]
      row_idx <- which(results_df$dataset == ds_name &
                         results_df$classifier == clf_name &
                         results_df$metric == mn)
      
      if (mn == "Runtime") {
        for (rn in names(resamplers)) {
          results_df[[rn]][row_idx] <- mean(runtimes_list[[rn]], na.rm = TRUE)
        }
      } else {
        for (rn in names(resamplers)) {
          scores <- metric_accum[[mn]][[rn]][[clf_name]]
          print(scores)
          results_df[[rn]][row_idx] <- mean(scores, na.rm = TRUE)
        }
      }
      
      # Store best_params JSON only once per classifier/dataset
      if (mn == metric_names[1]) {
        results_df$best_params[row_idx] <- jsonlite::toJSON(best_params_all, auto_unbox = TRUE)
      }
    }
  }
  
  # ---- Save after each dataset ----
  write.xlsx(results_df,
             file = "results.xlsx",
             asTable = TRUE)
}

