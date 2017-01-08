# FogPredictor_investigate.r

# oad required packages
require(lubridate)
require(stringr)
require(foreach)
require(randomForest)
require(parallel)

station = "YPPH"

# sets working directory
master_dir = "/home/jim/Dropbox/R/FogPredictor"
setwd(master_dir)

# load data_train
load(file = paste("data_train", station, ".RData", sep = ""))


# ---------------------------------------------
# - Create formula for Outputs ~ Inputs

Outputs_f <- vector()
Output <- "FG_or_BR"
for(Output in Outputs) {
  Outputs_f_o <- unique(data_train[, Output])
  for(Output_f_o in Outputs_f_o) {
    data_train[, paste(Output, Output_f_o, sep = "")] <- as.numeric(ifelse(data_train[, Output] == Output_f_o, 1, 0))
    print(paste(Output, " ", Output_f_o, sep = ""))
    Outputs_f <- c(Outputs_f, paste(Output, Output_f_o, sep = ""))
  }
  data_train[, Output] <- factor(data_train[, Output])
}

data_train[, Output] <- as.numeric(data_train[, Output])



# -------------------------------------
# sets formulas used in machine learning algorithms

# sets inputs as Variables with lead time (or hour, minute)
require(stringr)
Inputs = unlist(lapply(X = Variables, FUN = function(Variable) na.omit(str_match(pattern = paste(".*", Variable, ".*", sep = ""), string = names(data_train)))))
Inputs_n = unlist(lapply(X = Variables_n, FUN = function(Variable) na.omit(str_match(pattern = paste(".*", Variable, ".*", sep = ""), string = names(data_train)))))
Inputs_f = unlist(lapply(X = Variables_f, FUN = function(Variable) na.omit(str_match(pattern = paste(".*", Variable, ".*", sep = ""), string = names(data_train)))))

# formula for factors, and for regular
formula_f <- as.formula(paste(paste(Outputs_f, collapse = "+"), "~", paste(Inputs, collapse = "+")))
formula_f_n <- as.formula(paste(paste(Outputs_f, collapse = "+"), "~", paste(Inputs_n, collapse = "+")))
formula <- as.formula(paste(paste(paste("factor(", Outputs, ")", sep = ""), collapse = "+"), "~", paste(Inputs, collapse = "+")))

formula
formula_f

# ---------------------------------------------
# - Train each classifier on complete training data set and save

print(table(data_train[, Outputs]))

# # artificial neural network
# require(neuralnet)
# setwd(master_dir)
# arnn <- neuralnet(formula_f_n, rep = 1, stepmax = 10^6, data = data_train)
# save(arnn, file = paste("arnn_metar", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = ""))

# random forest
require(randomForest)
rndf <- randomForest(as.formula(paste("factor(", Output, ")", "~", paste(Inputs, collapse = "+")))
  , data = data_train
  , replace = TRUE
  , keep.inbag = TRUE
  , keep.forest = TRUE
  , strata = factor(rep(unique(data_train[, Outputs]), nrow(data_train)))
  )
rndf
rndf.robust <- randomForest(as.formula(paste("factor(", Output, ")", "~", paste(Inputs, collapse = "+")))
  , data = data_train
  , ntree = 5000
  , sampsize = 25
  , mtry = 4
  , keep.inbag = TRUE
  , keep.forest = TRUE
  , replace = TRUE
  , strata = factor(rep(unique(data_train[, Outputs]), nrow(data_train)))
  )
rndf.robust
rndf.default <- randomForest(as.formula(paste("factor(", Output, ")", "~", paste(Inputs, collapse = "+")))
  , data = data_train
  , ntree = 5000
  , replace = TRUE
  , strata = factor(rep(unique(data_train[, Outputs]), nrow(data_train)))
  )
rndf.default


?randomForest
#plot crosvalidated predictive performance
require(pROC)
plot(roc(rndf$votes[,2], data_train$FG_or_BR), col = 4)

# print accuracy
print(auc(roc(rndf$votes[, 2], data_train$FG_or_BR)))

# forrest flor
require(forestFloor)
ff = forestFloor(rndf, data_train[, Inputs], binary_reg = TRUE, calc_np = TRUE)
Col = fcol(ff, cols = 1, outlier.lim = 2.5)
plot(ff,col = Col, plot_GOF = TRUE)