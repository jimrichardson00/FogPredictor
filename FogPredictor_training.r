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
formula_f_n

# ---------------------------------------------
# - Train each classifier on complete training data set and save

print(table(data_train[, Outputs]))

# ---------------------------
# artificial neural network
require(neuralnet)
setwd(master_dir)
arnn <- neuralnet(formula_f_n, rep = 1, stepmax = 10^6, data = data_train)
save(arnn, file = paste("arnn", station, formatC(as.integer(lead_time), width = 2, flag = "0"), paste(Outputs, collapse = ""), ".RData", sep = ""))

require(pROC)
# plot(roc(response = data_train$FG_or_BR, predictor = compute(x = arnn, data_train[, Inputs_n])$net.result[, 2]), col = 1)
# auc(roc(response = data_train$FG_or_BR, predictor = compute(x = arnn, data_train[, Inputs_n])$net.result[, 2]))
roc_arnn <- roc(response = data_train$FG_or_BR, predictor = compute(x = arnn, data_train[, Inputs_n])$net.result[, 2])
save(roc_arnn, file = paste("roc_arnn", station, formatC(as.integer(lead_time), width = 2, flag = "0"), ".RData", sep = ""))

print(paste("arnn auc roc: ", auc(roc_arnn), sep = ""))

# ---------------------------
# random forest
require(randomForest)
rndf <- randomForest(as.formula(paste("factor(", Output, ")", "~", paste(Inputs, collapse = "+")))
  , data = data_train
  , replace = TRUE
  , strata = factor(rep(unique(data_train[, Outputs]), nrow(data_train)))
  )
rndf
paste("rndf", station, formatC(as.integer(lead_time), width = 2, flag = "0"), paste(Outputs, collapse = ""), ".RData", sep = "")
save(rndf, file = paste("rndf", station, formatC(as.integer(lead_time), width = 2, flag = "0"), paste(Outputs, collapse = ""), ".RData", sep = ""))

require(pROC)
# plot(roc(response = data_train$FG_or_BR, predictor = rndf$votes[, 2]), col = 4)
# auc(roc(response = data_train$FG_or_BR, predictor = rndf$votes[, 2]), col = 4)
roc_rndf <- roc(response = data_train$FG_or_BR, predictor = rndf$votes[, 2])
save(roc_rndf, file = paste("roc_rndf", station, formatC(as.integer(lead_time), width = 2, flag = "0"), ".RData", sep = ""))

print(paste("rndf auc roc: ", auc(roc_rndf), sep = ""))

# ---------------------------
# crude mode classifier

roc_mode <- roc(response = data_train$FG_or_BR, predictor = rep(0, nrow(data_train)))
save(roc_mode, file = paste("roc_mode", station, formatC(as.integer(lead_time), width = 2, flag = "0"), ".RData", sep = ""))

print(paste("mode auc roc: ", auc(roc_mode), sep = ""))

# ---------------------------------------------
# - Cross validation. For each i = 1,... N, randomly remove one night with fog, train classifier on the rest of the data and test on result, print results

# creates empty vector for each classifier to fill with percent correct values
per_correct_arnns <- vector()
per_correct_rndfs <- vector()
per_correct_nbays <- vector()
per_correct_modes <- vector()

# empty result list to fill
results <- vector("list", N)
results

# set parameters for artificial neural network
n_nodes <- ceiling(mean(c(length(Outputs_f), length(Inputs))))
n_layer <- 1

print("Starting: cross validation")
i <- 1 
require(foreach)
# results <- foreach(i = seq(1, N, 1)) %dopar% {
results <- for(i in seq(1, N, 1)) {

  print(paste("Cross validation: ", i, sep = ""))

  # train <- sample(seq(1, nrow(data_train), 1), size = ceiling(nrow(data_train)*(2/3)))
  # data_tra <- data_train[train, ]
  # head(data_tra)
  # data_tes <- data_train[-train, ]
  # head(data_tes)

  fog <- seq(1, nrow(data_train), 1)[data_train$FG_or_BR == "1"]
  fog_instance <- sample(fog, size = 1)
  train <- seq(1, nrow(data_train), 1)[!(seq(1, nrow(data_train), 1) == fog_instance)]

  data_tra <- data_train[train, ]
  head(data_tra)

  data_tes <- data_train[fog_instance, ]
  head(data_tes)

  tryCatch({

    # train artificial neural network on training data_train
    require(neuralnet)
    arnn <- neuralnet(formula_f_n, rep = 1, stepmax = 10^5, data = data_tra)
    # run classifier on test data, and store result
    arnn_result <- compute(x = arnn, data_tes[, Inputs_n])$net.result
    arnn_result <- as.data.frame(arnn_result)
    names(arnn_result) <- Outputs_f
    arnn_result <- apply(arnn_result, FUN = function(x) ifelse(x < 0, 0, ifelse(x > 1, 1, x)), MARGIN = 2)
    head(arnn_result)

    # random forest and naive bays
    rndf_result <- matrix(NA, nrow = nrow(data_tes), ncol = length(Outputs_f))
    rndf_result <- as.data.frame(rndf_result)
    names(rndf_result) <- Outputs_f
    head(rndf_result)

    nbay_result <- matrix(NA, nrow = nrow(data_tes), ncol = length(Outputs_f))
    nbay_result <- as.data.frame(nbay_result)
    names(nbay_result) <- Outputs_f
    head(nbay_result)

    o <- 1
    for(o in seq(1, length(Outputs), 1)) {

      Output <- Outputs[o]
      Outputs_f_o <- as.vector(Outputs_f[is.na(str_match(Outputs_f, Output)) == FALSE])
      Outputs_f_o

      # train random forest on training data
      require(randomForest)
      rndf <- randomForest(as.formula(paste("factor(", Output, ")", "~", paste(Inputs, collapse = "+")))
      , data = data_tra
      , replace = TRUE
      , strata = factor(rep(unique(data_tra[, Outputs]), nrow(data_tra)))
      )
      rndf
      # run classifier on test data, and store result
      output <- as.data.frame(predict(object = rndf, newdata = data_tes, type = "prob"))
      names(output) <- paste(Output, names(output), sep = "")
      for(Output_f_o in Outputs_f) {
        rndf_result[, Output_f_o] <- output[, Output_f_o]
      }
      head(rndf_result)

    }

  },
  error = function(e) {cat("ERROR :",conditionMessage(e), "\n")}
  )

  # sets P_var and P_val
  if(Outputs %in% P$P_Outputs) {
    P_var = as.character(P[Outputs == P$P_Outputs, "P_var"])
    P_val = as.numeric(P[Outputs == P$P_Outputs, "P_val"])
  } else {
    P_var = NA
    P_val = NA
  }

  # percent correct for each classifier
  per_correct_arnns <- c(per_correct_arnns, sum(arnn_result[2] > P_val))
  per_correct_rndfs <- c(per_correct_rndfs, sum(rndf_result[2] > P_val))
  # per_correct_nbays <- c(per_correct_nbays, sum(nbay_result[2] > P_val))
  # per_correct_modes <- c(per_correct_modes, sum(mode_result[2] > P_val))

}



per_correct_rndfs
# list containing percent correct for each iteration in each classifier
per_correct_resu <- list(
  per_correct_arnns = per_correct_arnns, 
  per_correct_rndfs = per_correct_rndfs,
  per_correct_nbays = per_correct_nbays, 
  per_correct_modes = per_correct_modes
  )

per_correct_rndfs

# print mean percent correct for each classifier
print(paste("per_correct_arnn: ", formatC(round(mean(per_correct_arnns), digits = 3), format = "f", digits = 3), sep = ""))
print(paste("per_correct_rndf: ", formatC(round(mean(per_correct_rndfs), digits = 3), format = "f", digits = 3), sep = ""))
print(paste("per_correct_nbay: ", formatC(round(mean(per_correct_nbays), digits = 3), format = "f", digits = 3), sep = ""))
print("")

per_correct_resu <- list(
  per_correct_arnns = per_correct_arnns, 
  per_correct_rndfs = per_correct_rndfs,
  per_correct_nbays = per_correct_nbays, 
  per_correct_modes = per_correct_modes
)

 # saves percent correct information
print(paste("per_correct", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = ""))
save(per_correct_resu, file = paste("per_correct_", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = ""))
