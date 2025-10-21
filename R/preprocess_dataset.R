preprocess_dataset <- function(data, min_class_samples = 8, var_threshold = 1e-4) {
  # Separate features & target
  x <- data[, -ncol(data), drop = FALSE]
  y <- data[, ncol(data)]
  stopifnot(is.factor(y))
  
  # 1. Skip dataset if any class has fewer than min_class_samples
  class_counts <- table(y)
  if (any(class_counts < min_class_samples)) {
    message("Skipping dataset: class size below ", min_class_samples)
    return(NULL)
  }
  
  # 2. Remove features with variance <= threshold **per class**
  keep_features <- rep(TRUE, ncol(x))
  
  for (cls in levels(y)) {
    idx <- which(y == cls)
    # Calculate variance for this class
    vars <- apply(x[idx, , drop = FALSE], 2, var)
    # Mark as FALSE if variance <= threshold (or NA)
    keep_features <- keep_features & !is.na(vars) & vars > var_threshold
  }
  
  if (!any(keep_features)) {
    message("Skipping dataset: all features removed after variance filter.")
    return(NULL)
  }
  
  x <- x[, keep_features, drop = FALSE]
  
  # 3. Standardize features (center & scale) based on whole dataset
  means <- apply(x, 2, mean)
  sds <- apply(x, 2, sd)
  
  # Avoid division by zero (though we filtered for low variance already)
  sds[sds == 0] <- 1
  
  x <- sweep(x, 2, means, FUN = "-")
  x <- sweep(x, 2, sds, FUN = "/")
  
  # Return processed dataset
  return(data.frame(x, y = y))
}
