# Load packages
library(xlsx)

# Function that will calculate all performace metrics
calculate_performance_metrics <- function(TP, FP, TN, FN) {
  
  # Expected Positives and Expected Negatives
  EP <- TP + FN
  EN <- TN + FP

  # Observed Positives and Observed Negatives
  OP <- TP + FP
  ON <- TN + FN

  # Prevalence
  Prevalence <- EP / (EP + EN)

  # Positive Predictive Value
  PPV <- TP / (TP + FP)

  # Negative Predictive Value
  NPV <- TN / (TN + FN)

  # True Positive Rate
  TPR <- TP / EP

  # True Negative Rate
  TNR <- TN / EN

  # Accuracy
  ACC <- (TP + TN) / (EP + EN)

  # F-Score
  FS <- 2 * PPV * TPR / (PPV + TPR)

  # Youden's J Statistic
  YJS <- TPR + TNR - 1

  # PC
  PC <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))

  return(list(
    TP = TP,
    FP = FP,
    TN = TN,
    FN = FN,
    EP = EP,
    EN = EN,
    OP = OP,
    ON = ON,
    Prevalence = round(Prevalence, 2),
    PPV = PPV,
    NPV = NPV,
    TPR = TPR,
    TNR = TNR,
    ACC = ACC,
    FS = FS,
    YJS = YJS,
    PC = PC
  ))
}


# Variable to collect the metrics of each situation
all_metrics <- data.frame() 

# Situation 1
# Low prevalence and silly negative method
# Bad for ACC and PPV
# Good for FS and YJS
TP <- 1
FP <- 0
TN <- 2000
FN <- 199
all_metrics <- rbind(all_metrics, calculate_performance_metrics(TP, FP, TN, FN))

# Situation 2
# High prevalence and silly positive method
# Bad for PPV, ACC and FS
# Good for YJS
TP <- 1999
FP <- 199
TN <- 1
FN <- 1
all_metrics <- rbind(all_metrics, calculate_performance_metrics(TP, FP, TN, FN))

# Situation 3
# Low prevalence and low PPV but high NPV
# Bad for ACC, YJS
# Good for FS and PPV
TP <- 10
FP <- 100
TN <- 1899
FN <- 1
all_metrics <- rbind(all_metrics, calculate_performance_metrics(TP, FP, TN, FN))

# Transpose the data frame and export it
all_metrics <- data.frame(t(all_metrics))
colnames(all_metrics) <- c("Situation_1", "Situation_2", "Situation_3")
all_metrics <- cbind(Metric = row.names(all_metrics), all_metrics)
xlsx::write.xlsx(x = all_metrics, file = "results.xlsx", col.names = TRUE, row.names = FALSE)
