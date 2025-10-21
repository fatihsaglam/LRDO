
classifiers_all <- list(
  NB = list(
    train = function(x, y, ...) {
      naivebayes::naive_bayes(x, y, ...)
    },
    predict = function(m, x) {
      pred <- predict(object = m, newdata = x)
      prob <- predict(object = m, newdata = x, type = "prob")
      return(list(pred = pred, prob = prob))
    }
  ),
  LogReg = list(
    train = function(x, y) {
      x <- as.data.frame(x)
      var_names <- colnames(x)
      k_class <- length(levels(y))
      dat <- data.frame(x, y = y)
      m <-
        glm(
          formula = y ~ .,
          data = dat,
          family = "binomial",
          control = glm.control(maxit = 1000)
        )
      m$var_names <- var_names
      m$class_names <- levels(y)
      return(m)
    },
    predict = function(m, x) {
      x <- as.data.frame(x)
      colnames(x) <- m$var_names
      prob <- predict(m, as.data.frame(x), type = "response")
      pred <-
        factor(
          ifelse(prob < 0.5, m$class_names[1], m$class_names[2]),
          levels = m$class_names,
          labels = m$class_names
        )
      prob <- cbind(1 - prob, prob)
      dimnames(prob)[[2]] <- m$class_names
      return(list(pred = pred, prob = prob))
    }
  ),
  # SVMLinear = list(
  #   train = function(x, y, ...) {
  #     var_names <- colnames(x)
  #     tol <- 1e-3
  #     repeat {
  #       m <- try(kernlab::ksvm(
  #         x = as.matrix(x),
  #         y = y,
  #         kernel = "vanilladot",
  #         kpar = list(),
  #         prob.model = TRUE,
  #         tol = tol,
  #         ...
  #       ),
  #       silent = TRUE)
  #       if (("try-error" %in% class(m)))
  #       {
  #         tol <- tol  *  10
  #         if (tol > 1)
  #         {
  #           tol <- 1
  #         }
  #       } else {
  #         if (all(sapply(m@prob.model, function(mm) length(mm) == 2))) {
  #           break
  #         }
  #       }
  #     }
  #     attr(m, "var_names") <- var_names
  #     attr(m, "class_names") <- levels(y)
  #     return(m)
  #   },
  #   predict = function(m, x) {
  #     prob <-
  #       predict(object = m,
  #               newdata = as.matrix(x),
  #               type = "probabilities")
  #     pred <- factor(attr(m, "class_names")[max.col(prob)], 
  #                    levels = attr(m, "class_names"), 
  #                    labels = attr(m, "class_names"))
  #     results <- list(pred = pred,
  #                     prob = prob)
  #     return(results)
  #   }
  # ),
  # 
  SVMRadial = list(
    train = function(x, y, ...) {
      var_names <- colnames(x)
      tol <- 1e-3
      repeat {
        m <- try(kernlab::ksvm(
          x = as.matrix(x),
          y = y,
          kernel = "rbfdot",
          kpar = "automatic",
          prob.model = TRUE,
          tol = tol
        ),
        silent = TRUE)
        if (("try-error" %in% class(m)))
        {
          tol <- tol  *  10
          if (tol > 1)
          {
            tol <- 1
          }
        } else {
          if (all(sapply(m@prob.model, function(mm) length(mm) == 2))) {
            break
          }
        }
      }
      attr(m, "var_names") <- var_names
      attr(m, "class_names") <- levels(y)
      return(m)
    },
    predict = function(m, x) {
      prob <-
        predict(object = m,
                newdata = as.matrix(x),
                type = "probabilities")
      pred <- factor(attr(m, "class_names")[max.col(prob)], levels = attr(m, "class_names"), 
                     labels = attr(m, "class_names"))
      results <- list(pred = pred,
                      prob = prob)
      return(results)
    }
  ),
  KNN = list(
    train = function(x, y, ...) {
      caret::knn3(x, y, ...)
    },
    predict = function(m, x) {
      pred <- predict(object = m,
                      newdata = x,
                      type = "class")
      prob <-
        predict(object = m,
                newdata = as.matrix(x),
                type = "prob")
      results <- list(pred = pred,
                      prob = prob)
      return(results)
    }
  ),
  CART = list(
    train = function(x, y, ...) {
      x <- as.data.frame(x)
      var_names <- colnames(x)
      dat <- data.frame(x, y = y)
      m <- rpart::rpart(formula = y ~ ., data = dat, ...)
      m$var_names <- var_names
      m$class_names <- levels(y)
      return(m)
    },
    predict = function(m, x) {
      colnames(x) <- m$var_names
      pred <-
        predict(object = m,
                newdata = as.data.frame(x),
                type = "class")
      prob <-
        predict(object = m,
                newdata = as.data.frame(x),
                type = "prob")
      
      results <- list(pred = pred,
                      prob = prob)
      return(results)
    }),
  RF = list(
    train = function(x, y, ...) {
      var_names <- colnames(x)
      
      m <- ranger::ranger(
        x = x,
        y = y,
        probability = TRUE,
        ...
      )
      m$var_names <- var_names
      m$class_names <- levels(y)
      return(m)
    },
    predict = function(m, x) {
      colnames(x) <- m$var_names    
      prob <-
        predict(object = m,
                data = x,
                type = "response")$predictions
      pred <-
        factor(m$class_names[max.col(prob)],
               levels = m$class_names,
               labels = m$class_names)
      
      results <- list(pred = pred,
                      prob = prob)
      return(results)
    }
  ),
  MARS = list(
    train = function(x, y, ...) {
      var_names <- colnames(x)
      m <-
        earth::earth(
          x = x,
          y = y,
          nk = 100,
          nprune = 100,
          degree = 1,
          glm = list(family = "binomial"),
          ...
        )
      m$var_names <- var_names
      m$class_names <- levels(y)
      return(m)
    },
    predict = function(m, x) {
      pred <- predict(object = m,
                      newdata = x,
                      type = "class")
      pred <- factor(pred, levels = m$class_names)
      prob <-
        predict(object = m,
                newdata = as.matrix(x),
                type = "response")
      
      M_prob <- matrix(data = NA, nrow = length(pred), ncol = length(m$class_names))
      M_prob[,which(m$class_names %in% colnames(prob))] <- prob
      M_prob[,-which(m$class_names %in% colnames(prob))] <- 1 - prob
      prob <- M_prob
      colnames(prob) <- m$class_names
      results <- list(pred = pred,
                      prob = prob)
      return(results)
    }
  ),
  # --- inside your classifiers <- list( ... ), replace NNET with this ----
  # NNET = list(
  #   train = function(x, y,
  #                    hidden_dim = 32L,
  #                    epochs = 100L,
  #                    batch_size = 64L,
  #                    lr = 1e-3,
  #                    weight_decay = 0,
  #                    activation = "relu",
  #                    verbose = FALSE) {
  #     
  #     library(reticulate)
  #     torch <- import("torch")
  #     nn <- import("torch.nn", convert = TRUE)
  #     
  #     # Ensure numeric matrix and factor y
  #     x <- as.matrix(x)
  #     storage.mode(x) <- "double"
  #     p <- ncol(x)
  #     class_names <- levels(y)
  #     stopifnot(length(class_names) == 2L)
  #     class_pos <- names(which.min(table(y)))
  #     y_bin <- as.integer(y == class_pos)  # class_names[2] is "positive"
  #     
  #     # Model ---------------------------------------------------------------
  #     Module <- nn$Module
  #     Net <- PyClass("Net", inherit = Module, defs = list(
  #       `__init__` = function(self, p, h) {
  #         super()$`__init__`()
  #         self$fc1 <- nn$Linear(as.integer(p), as.integer(h))
  #         self$fc2 <- nn$Linear(as.integer(h), 1L)
  #         if (activation == "lrelu") {
  #           self$act <- nn$LeakyReLU(0.2)
  #         } else {
  #           self$act <- nn$ReLU()
  #         }
  #         NULL
  #       },
  #       forward = function(self, x) {
  #         x <- self$act(self$fc1(x))
  #         x <- self$fc2(x)  # logits
  #         x
  #       }
  #     ))
  #     
  #     net <- Net(p, hidden_dim)
  #     
  #     # Optim & loss --------------------------------------------------------
  #     optim <- torch$optim$RMSprop(net$parameters(), lr = lr)
  #     crit  <- nn$BCEWithLogitsLoss()
  #     
  #     # Tensors -------------------------------------------------------------
  #     x_t <- torch$tensor(x, dtype = torch$float32)
  #     y_t <- torch$tensor(matrix(y_bin, ncol = 1L), dtype = torch$float32)
  #     
  #     # batching helper
  #     create_batches <- function(n, bs) {
  #       idx <- sample.int(n)
  #       split(idx, ceiling(seq_along(idx)/bs))
  #     }
  #     
  #     # Train ---------------------------------------------------------------
  #     n <- nrow(x)
  #     for (epoch in seq_len(epochs)) {
  #       batches <- create_batches(n, batch_size)
  #       running <- 0
  #       for (ids in batches) {
  #         xb <- x_t[ids - 1, ]
  #         yb <- y_t[ids - 1, ]
  #         optim$zero_grad()
  #         logits <- net(xb)
  #         loss <- crit(logits, yb)
  #         loss$backward()
  #         optim$step()
  #         running <- running + loss$item()
  #       }
  #       if (verbose && (epoch %% 10L == 0L)) {
  #         cat(sprintf("epoch %d | loss %.5f\n", epoch, running/length(batches)))
  #       }
  #     }
  #     
  #     # store needed info
  #     attr(net, "var_names")  <- colnames(x)
  #     attr(net, "class_names") <- class_names
  #     return(net)
  #   },
  #   
  #   predict = function(m, x) {
  #     library(reticulate)
  #     torch <- import("torch")
  #     
  #     x <- as.matrix(x)
  #     storage.mode(x) <- "double"
  #     if (!is.null(attr(m, "var_names"))) {
  #       colnames(x) <- attr(m, "var_names")
  #     }
  #     class_names <- attr(m, "class_names")
  #     
  #     x_t <- torch$tensor(x, dtype = torch$float32)
  #     logits <- m(x_t)
  #     probs_pos <- torch$sigmoid(logits)$squeeze(1L)$cpu()$detach()$numpy()
  #     
  #     prob <- cbind(probs_pos, 1 - probs_pos)
  #     dimnames(prob)[[2]] <- class_names
  #     
  #     pred <- factor(class_names[max.col(prob)],
  #                    levels = class_names, labels = class_names)
  #     
  #     list(pred = pred, prob = prob)
  #   }
  # ),
  NNET2 = list(
    train = function(x, y,
                     size = 5L,        # hidden units
                     decay = 0,        # L2 penalty
                     maxit = 200L,
                     MaxNWts = 10000L,
                     trace = FALSE) {
      
      stopifnot(is.factor(y))
      x <- as.matrix(x)
      var_names  <- colnames(x)
      class_names <- levels(y)
      class_pos <- names(which.min(table(y)))
      k <- length(class_names)
      
      # Build target for nnet
      if (k == 2L) {
        # binary: 0/1 with logistic loss
        y_target <- as.numeric(y == class_pos)
        softmax <- FALSE; entropy <- TRUE
      } else {
        # multiclass: one-hot with softmax
        y_target <- nnet::class.ind(y)
        softmax <- TRUE; entropy <- TRUE
      }
      
      fit <- nnet::nnet(
        x = x,
        y = y_target,
        size = as.integer(size),
        decay = decay,
        maxit = as.integer(maxit),
        MaxNWts = as.integer(MaxNWts),
        linout = FALSE,
        entropy = entropy,
        softmax = softmax,
        trace = trace
      )
      
      attr(fit, "var_names")   <- var_names
      attr(fit, "class_pos") <- class_pos
      attr(fit, "class_names") <- class_names
      fit
    },
    
    predict = function(m, x) {
      x <- as.matrix(x)
      if (!is.null(attr(m, "var_names"))) {
        colnames(x) <- attr(m, "var_names")
      }
      class_names <- attr(m, "class_names")
      class_pos <- attr(m, "class_pos")
      k <- length(class_names)
      
      pr <- predict(m, x, type = "raw")
      
      
      
      if (k == 2L) {
        prob <- matrix(NA, nrow = nrow(x), ncol = 2)
        prob[,class_names == class_pos] <- pr
        prob[,class_names != class_pos] <- 1 - pr
        dimnames(prob)[[2]] <- class_names
      } else {
        prob <- as.matrix(pr)
        if (is.null(colnames(prob)) || !identical(colnames(prob), class_names)) {
          colnames(prob) <- class_names  
        }
      }
      
      pred <- factor(class_names[max.col(prob, ties.method = "first")],
                     levels = class_names, labels = class_names)
      
      list(pred = pred, prob = prob)
    }
  ),
  GBM = list(
    train = function(x, y, ...) {
      library(xgboost)
      y_num <- as.integer(y) - 1L
      dtrain <- xgb.DMatrix(data = as.matrix(x), label = y_num)
      params <- list(
        objective = "binary:logistic",
        eval_metric = "logloss"
      )
      model <- xgb.train(params = params, data = dtrain, nrounds = 100, ...)
      attr(model, "class_names") <- levels(y)
      attr(model, "var_names")   <- colnames(x)
      model
    },
    predict = function(m, x) {
      library(xgboost)
      prob_pos <- predict(m, newdata = as.matrix(x))
      class_names <- attr(m, "class_names")
      prob <- cbind(1 - prob_pos, prob_pos)
      dimnames(prob)[[2]] <- class_names
      pred <- factor(class_names[max.col(prob)], levels = class_names)
      list(pred = pred, prob = prob)
    }
  ),
  LDA = list(
    train = function(x, y, ...) {
      m <- MASS::lda(x, y, ...)
      attr(m, "class_names") <- levels(y)
      m
    },
    predict = function(m, x) {
      p <- predict(m, newdata = x)
      prob <- p$posterior
      pred <- p$class
      list(pred = pred, prob = prob)
    }
  )
)
