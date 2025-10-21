library(openxlsx)
library(ggplot2)
library(readxl)

data_results <-
  read_excel(path = "results_all NNET added.xlsx")
data_info <- attr(readRDS(file = "datasets_classification_binary.rds"), "info")

s_classifier <- c("CART","GBM","KNN", "LogReg", "MARS", "NNET2", "RF", "SVM")
s_dataset <- c(
  "ecoli_0_1_4_6_vs_5",
  "ecoli_0_1_4_7_vs_2_3_5_6",
  "ecoli_0_1_4_7_vs_5_6",
  "ecoli_0_1_vs_2_3_5",
  "ecoli_0_1_vs_5",
  "ecoli_0_2_3_4_vs_5",
  "ecoli_0_3_4_7_vs_5_6",
  "ecoli_0_3_4_vs_5",
  "ecoli_0_4_6_vs_5",
  "ecoli_0_vs_1",
  "ecoli1",
  "ecoli2",
  "ecoli3",
  "ecoli4",
  "Ionosphere",
  "Sonar",
  "vehicle0",
  "vehicle1",
  "segmentationData",
  "khan1",
  "pulsar_stars",
  "spam",
  "vowel0",
  "Vowel3",
  "Fishing1",
  "Fishing2",
  "Fishing4",
  "abalone_19_vs_10_11_12_13",
  "abalone_21_vs_8",
  "abalone_3_vs_11",
  "abalone19",
  "abalone9_18",
  "glass_0_1_2_3_vs_4_5_6",
  "glass0",
  "glass1",
  "glass2",
  "glass4",
  "yeast_1_2_8_9_vs_7",
  "yeast_1_4_5_8_vs_7",
  "yeast_0_3_5_9_vs_7_8",
  "yeast4",
  "yeast5",
  "yeast6",
  "led7digit_0_2_4_5_6_7_8_9_vs_1",
  "car_vgood",
  "kddcup_rootkit_imap_vs_back",
  "kr_vs_k_three_vs_eleven",
  "kr_vs_k_zero_one_vs_draw",
  "kr_vs_k_zero_vs_fifteen",
  "poker_8_9_vs_6",
  "poker_8_vs_6",
  "winequality_red_4"
)
s_methods <- c(
  "NORES",
  "ROS",
  "SMOTE",
  "RWO",
  "ROSE",
  "GSMOTE",
  "SMOTEWB",
  "SyMProD",
  "GQEO",
  "EBSMOTE",
  "deepSMOTE",
  "LRDO"
)



s_metric <- c("ROCAUC")
dat <-
  data_results[data_results$classifier %in% s_classifier &
                 data_results$metric %in% s_metric &
                 data_results$dataset %in% s_dataset,
               c("classifier", "metric", "dataset", s_methods)]

ddat <- as.matrix(dat[, -c(1:3)])
rownames(ddat) <- 1:nrow(ddat)

m_friedman <- PMCMRplus::friedmanTest(y = ddat)

dat <-
  data_results[data_results$classifier %in% s_classifier &
                 data_results$metric %in% s_metric &
                 data_results$dataset %in% s_dataset, c("classifier", "metric", "dataset", s_methods)]

ddat <- as.matrix(dat[, -c(1:3)])
rownames(ddat) <- 1:nrow(ddat)

m_friedman <- PMCMRplus::friedmanTest(y = ddat)
print(m_friedman)

ddat_melted <- reshape2::melt(ddat)
colnames(ddat_melted) <- c("i", "methods", "perf")

cat("\n------------\n Conover test:\n")
print(PMCMRplus::frdAllPairsConoverTest(ddat,
                                        p.adjust.method = "holm"))

cat("\n------------\n Nemenyi test:\n")
print(PMCMRplus::frdAllPairsNemenyiTest(ddat,
                                        p.adjust.method = "holm"))



s_metric <- c("BACC")
dat <-
  data_results[data_results$classifier %in% s_classifier &
                 data_results$metric %in% s_metric &
                 data_results$dataset %in% s_dataset,
               c("classifier", "metric", "dataset", s_methods)]

ddat <- as.matrix(dat[, -c(1:3)])
rownames(ddat) <- 1:nrow(ddat)

m_friedman <- PMCMRplus::friedmanTest(y = ddat)

dat <-
  data_results[data_results$classifier %in% s_classifier &
                 data_results$metric %in% s_metric &
                 data_results$dataset %in% s_dataset, c("classifier", "metric", "dataset", s_methods)]

ddat <- as.matrix(dat[, -c(1:3)])
rownames(ddat) <- 1:nrow(ddat)

m_friedman <- PMCMRplus::friedmanTest(y = ddat)
print(m_friedman)

ddat_melted <- reshape2::melt(ddat)
colnames(ddat_melted) <- c("i", "methods", "perf")

cat("\n------------\n Conover test:\n")
print(PMCMRplus::frdAllPairsConoverTest(ddat,
                                        p.adjust.method = "holm"))

cat("\n------------\n Nemenyi test:\n")
print(PMCMRplus::frdAllPairsNemenyiTest(ddat,
                                        p.adjust.method = "holm"))



s_metric <- c("GMEAN")
dat <-
  data_results[data_results$classifier %in% s_classifier &
                 data_results$metric %in% s_metric &
                 data_results$dataset %in% s_dataset,
               c("classifier", "metric", "dataset", s_methods)]

ddat <- as.matrix(dat[, -c(1:3)])
rownames(ddat) <- 1:nrow(ddat)

m_friedman <- PMCMRplus::friedmanTest(y = ddat)

dat <-
  data_results[data_results$classifier %in% s_classifier &
                 data_results$metric %in% s_metric &
                 data_results$dataset %in% s_dataset, c("classifier", "metric", "dataset", s_methods)]

ddat <- as.matrix(dat[, -c(1:3)])
rownames(ddat) <- 1:nrow(ddat)

m_friedman <- PMCMRplus::friedmanTest(y = ddat)
print(m_friedman)

ddat_melted <- reshape2::melt(ddat)
colnames(ddat_melted) <- c("i", "methods", "perf")

cat("\n------------\n Conover test:\n")
print(PMCMRplus::frdAllPairsConoverTest(ddat,
                                        p.adjust.method = "holm"))

cat("\n------------\n Nemenyi test:\n")
print(PMCMRplus::frdAllPairsNemenyiTest(ddat,
                                        p.adjust.method = "holm"))
