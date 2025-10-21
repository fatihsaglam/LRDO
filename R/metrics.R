metricCls_BACC <- function(truth, pred, prob = NULL, ...) {
  class_names <- levels(truth)
  k_classes <- length(class_names)
  n <- length(truth)
  n_classes <- sapply(class_names, function(m) {
    sum(truth == m)
  })
  mean(sapply(class_names, function(m) {
    ii <- truth == m
    sum(truth[ii] == pred[ii])
  })/n_classes)
}

metricCls_MCC <- function(truth, pred, prob = NULL, ...) {
  class_names <- levels(truth)
  k_classes <- length(class_names)
  n <- length(truth)
  n_classes <- sapply(class_names, function(m) {
    sum(truth == m)
  })
  mean(sapply(class_names, function(m) {
    truth_temp <- as.character(truth)
    pred_temp <- as.character(pred)
    
    truth_temp[!(truth == m)] <- "rrest"
    pred_temp[!(pred_temp == m)] <- "rrest"
    
    truth_temp <- factor(truth_temp, levels = c(m, "rrest"))
    pred_temp <- factor(pred_temp, levels = c(m, "rrest"))
    
    mlr::measureMCC(truth = truth_temp, response = pred_temp, negative = "rrest", positive = m)
  }))
}

metricCls_F1 <- function(truth, pred, prob = NULL, ...) {
  class_names <- levels(truth)
  k_classes <- length(class_names)
  n <- length(truth)
  n_classes <- sapply(class_names, function(m) {
    sum(truth == m)
  })
  mean(sapply(class_names, function(m) {
    truth_temp <- as.character(truth)
    pred_temp <- as.character(pred)
    
    truth_temp[!(truth == m)] <- "rrest"
    pred_temp[!(pred_temp == m)] <- "rrest"
    
    truth_temp <- factor(truth_temp, levels = c(m, "rrest"))
    pred_temp <- factor(pred_temp, levels = c(m, "rrest"))
    
    mlr::measureF1(truth = truth_temp, response = pred_temp, positive = m)
  }))
}

metricCls_GMEAN <- function(truth, pred, prob = NULL, ...) {
  class_names <- levels(truth)
  k_classes <- length(class_names)
  n <- length(truth)
  n_classes <- sapply(class_names, function(m) {
    sum(truth == m)
  })
  mean(sapply(class_names, function(m) {
    truth_temp <- as.character(truth)
    pred_temp <- as.character(pred)
    
    truth_temp[!(truth == m)] <- "rrrest"
    pred_temp[!(pred_temp == m)] <- "rrrest"
    
    truth_temp <- factor(truth_temp, levels = c(m, "rrrest"))
    pred_temp <- factor(pred_temp, levels = c(m, "rrrest"))
    
    mlr::measureGMEAN(truth = truth_temp, response = pred_temp, negative = "rrrest", positive = m)
  }))
}

metricCls_ROCAUC <- function(truth, pred = NULL, prob = NULL, ...) {
  if (is.null(prob)) {
    if (is.null(pred)) {
      stop("Either pred or prob must be provied")
    }
    
    class_names <- levels(truth)
    k_class <- length(class_names)
    prob <- matrix(data = 0, nrow = length(pred), ncol = k_class)
    pred_num <- as.numeric(pred)
    for (i in 1:length(pred)) {
      prob[i,pred_num[i]] <- 1
    }
  }
  yardstick::roc_auc_vec(truth = truth, estimate = prob, estimator = "macro")
}

metricCls_PRAUC <- function(truth, pred = NULL, prob = NULL, ...) {
  if (is.null(prob)) {
    if (is.null(pred)) {
      stop("Either pred or prob must be provied")
    }
    
    class_names <- levels(truth)
    k_class <- length(class_names)
    prob <- matrix(data = 0, nrow = length(pred), ncol = k_class)
    pred_num <- as.numeric(pred)
    for (i in 1:length(pred)) {
      prob[i,pred_num[i]] <- 1
    }
  }
  yardstick::pr_auc_vec(truth = truth, estimate = prob, estimator = "macro")
}

metricCls_IAM <- function(truth, pred, prob = NULL, ...) {
  class_names <- levels(truth)
  k_classes <- length(class_names)
  C <- table(truth, pred)
  
  mean(sapply(1:k_classes, function(m) {
    (C[m,m] - max(sum(C[-m,m]),sum(C[m,-m])))/max(sum(C[m,]), sum(C[,m]))
  }))
}


metricCls_all <- list(
  BACC = metricCls_BACC,
  MCC = metricCls_MCC,
  F1 = metricCls_F1,
  GMEAN = metricCls_GMEAN,
  ROCAUC = metricCls_ROCAUC,
  PRAUC = metricCls_PRAUC,
  IAM = metricCls_IAM
)