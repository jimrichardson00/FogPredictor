# load required packages
require(lubridate)
require(stringr)
require(foreach)
require(randomForest)
require(parallel)

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
formula_f

# ---------------------------------------------
# - Train each classifier on complete training data set and save

print(table(data_train[, Outputs]))

# artificial neural network
require(neuralnet)
setwd(master_dir)
arnn <- neuralnet(formula_f_n, rep = 1, stepmax = 10^6, data = data_train)
save(arnn, file = paste("arnn", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = ""))

require(pROC)
# plot(roc(response = data_train$FG_or_BR, predictor = compute(x = arnn, data_train[, Inputs_n])$net.result[, 2]), col = 1)
auc(roc(response = data_train$FG_or_BR, predictor = compute(x = arnn, data_train[, Inputs_n])$net.result[, 2]))

roc_arnn <- roc(response = data_train$FG_or_BR, predictor = compute(x = arnn, data_train[, Inputs_n])$net.result[, 2])
save(roc_arnn, file = paste("roc_arnn", station, lead_time, ".RData", sep = ""))

# random forest
require(randomForest)
rndf <- randomForest(as.formula(paste("factor(", Output, ")", "~", paste(Inputs, collapse = "+")))
  , data = data_train
  , replace = TRUE
  , strata = factor(rep(unique(data_train[, Outputs]), nrow(data_train)))
  )
rndf
paste("rndf", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = "")
save(rndf, file = paste("rndf", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = ""))

require(pROC)
# plot(roc(response = data_train$FG_or_BR, predictor = rndf$votes[, 2]), col = 4)
auc(roc(response = data_train$FG_or_BR, predictor = rndf$votes[, 2]), col = 4)

roc_rndf <- roc(response = data_train$FG_or_BR, predictor = rndf$votes[, 2])
save(roc_rndf, file = paste("roc_rndf", station, lead_time, ".RData", sep = ""))

score <- rndf$votes[, 2]
head(score)

cls <- data_train$FG_or_BR
head(cls)

pos = score[cls == 1]
neg = score[cls == 0]

set.seed(14)
p = replicate(50000, sample(pos, size=1) > sample(neg, size=1))
mean(p)

# # -------------------------------------------
# # run classifier on only fog data

# # subset to fog
# data_fog <- data_train[data_train$FG_or_BR == "1", ]

# # rndf result
# rndf_result <- matrix(NA, nrow = nrow(data_fog), ncol = length(Outputs_f))
# rndf_result <- as.data.frame(rndf_result)
# names(rndf_result) <- Outputs_f
# head(rndf_result)

# # run classifier on fog data, and store result
# output <- as.data.frame(predict(object = rndf, newdata = data_fog, type = "prob"))
# names(output) <- paste(Output, names(output), sep = "")
# for(Output_f_o in Outputs_f) {
#   rndf_result[, Output_f_o] <- output[, Output_f_o]
# }
# head(rndf_result[, 2])

# # calculate the percentage of fog nights correctly classified as fog
# sum(rndf_result[, 2] > 0.3)/length(rndf_result)
# # -------------------------------------------

# # naive bayes
# require(klaR)
# nbay <- NaiveBayes(formula = formula, 
#   data = data_train, 
#   prior = rep(1/length(unique(data_train[, Outputs])), length(unique(data_train[, Outputs])))
#   )
# save(nbay, file = paste("nbay", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = ""))

# crude mode
require(pROC)
# plot(roc(response = data_train$FG_or_BR, predictor = rep(0, nrow(data_train))))
auc(roc(response = data_train$FG_or_BR, predictor = rep(0, nrow(data_train))))

roc_mode <- roc(response = data_train$FG_or_BR, predictor = rep(0, nrow(data_train)))
save(roc_mode, file = paste("roc_mode", station, lead_time, ".RData", sep = ""))
roc_mode


# ---------------------------------------------
# - Cross validation. For each i = 1,... N, randomly split the data into two pieces, one consisting of 2/3 of the Set/Trap combinations and 1/3 of the Set/Trap combinations. Train each classifier on 2/3 data, and test on 1/3, and store the result

# empty result list to fill
results <- vector("list", N)
results

# set parameters for artificial neural network
n_nodes <- ceiling(mean(c(length(Outputs_f), length(Inputs))))
n_layer <- 1

print("Starting: cross validation")
i <- 1 
require(foreach)
results <- foreach(i = seq(1, N, 1)) %dopar% {

  print(paste("Cross validation: ", i, sep = ""))

  train <- sample(seq(1, nrow(data_train), 1), size = ceiling(nrow(data_train)*(2/3)))

  data_tra <- data_train[train, ]
  head(data_tra)

  data_tes <- data_train[-train, ]
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

    #   # train naive bayes on training data
    #   require(klaR)
    #   nbay <- NaiveBayes(formula = as.formula(paste("factor(", Output, ")", "~", paste(Inputs_n, collapse = "+")))
    #     , data = data_tra
    #     , prior = rep(1/length(unique(data_tra[, Outputs])), length(unique(data_tra[, Outputs])))
    #     )
    #   # run classifier on test data, and store result
    #   posterior <- as.data.frame(predict(object = nbay, newdata = data_tes[, Inputs])$posterior)
    #   class <- as.data.frame(predict(object = nbay, newdata = data_tes[, Inputs])$class)
    #   posterior[is.na(posterior[, names(posterior)[1]]) == TRUE, names(posterior)[1]] <- ifelse(class[is.na(posterior[, names(posterior)[1]]) == TRUE, 1] == names(posterior[1]), 1, 0) 
    #   posterior[is.na(posterior[, names(posterior)[2]]) == TRUE, names(posterior)[2]] <- ifelse(class[is.na(posterior[, names(posterior)[2]]) == TRUE, 1] == names(posterior[2]), 1, 0) 
    #   output <- as.data.frame(posterior)
    #   names(output) <- paste(Output, names(output), sep = "")
    #   for(Output_f_o in Outputs_f) {
    #     nbay_result[, Output_f_o] <- output[, Output_f_o]
    #   }
    #   nbay_result

  }

  # train crude mode classifier on training data
  data_tra_c <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, result = data_tra[, Outputs_f])
  mode_table <- apply(data_tra_c, MARGIN = 2, FUN = function(x) names(which.max(table(x))))
  # run crude mode classifier on test data, and store result
  mode_result <- matrix(rep(mode_table, nrow(data_tes)), nrow = nrow(data_tes), byrow = TRUE)
  mode_result <- as.data.frame(mode_result)
  names(mode_result) <- names(mode_table)
  head(mode_result)

  # sets P_var and P_val
  if(Outputs %in% P$P_Outputs) {
    P_var = as.character(P[Outputs == P$P_Outputs, "P_var"])
    P_val = as.numeric(P[Outputs == P$P_Outputs, "P_val"])
  } else {
    P_var = NA
    P_val = NA
  }

  # write results for each classifier to result
  results[[i]][["arnn_result"]] <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, P_var = P_var, P_val = P_val, result = arnn_result)
  results[[i]][["rndf_result"]] <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, P_var = P_var, P_val = P_val, result = rndf_result)
  # results[[i]][["nbay_result"]] <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, P_var = P_var, P_val = P_val, result = nbay_result)
  results[[i]][["mode_result"]] <- mode_result
  results[[i]][["data_result"]] <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, P_var = P_var, P_val = P_val, result = data_tes[, Outputs_f])

  results[[i]]

  },
  error = function(e) {cat("ERROR :",conditionMessage(e), "\n")}
  )
}
print("Finished: cross validation")

# index non null results as non_null. null results are as when the 2/3 training subset happens to have less than 2 unique classifications
non_null <- seq(1, N, 1)[unlist(lapply(1:N, FUN = function(i) is.null(results[[i]]) == FALSE))]
non_null

# subset results to non null results
results_nn <- vector("list", length(non_null))
for(i in 1:length(non_null)) {
  results_nn[[i]] <- results[[non_null[i]]]
}

# save results
print(paste("results", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = ""))
save(results, file = paste("results", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = ""))

# ---------------------------------------------
# - Print and store the average percent correct from the cross validation

# create empty list to fill with percent correct for each classifier
per_correct_resu <- list()
o <- 1
for(o in seq(1, length(Outputs), 1)) {

  Output <- Outputs[o]
  Output

  Outputs_f_o <- na.omit(str_match(Outputs_f, paste(Output, ".+", sep = "")))
  Outputs_f_o

  # calculate confusion matrix for each classifier
  confusion_arnn <- matrix(0, nrow = nrow(Outputs_f_o), ncol = nrow(Outputs_f_o))
  confusion_rndf <- matrix(0, nrow = nrow(Outputs_f_o), ncol = nrow(Outputs_f_o))
  confusion_nbay <- matrix(0, nrow = nrow(Outputs_f_o), ncol = nrow(Outputs_f_o))
  confusion_mode <- matrix(0, nrow = nrow(Outputs_f_o), ncol = nrow(Outputs_f_o))

  # sets initial percent correct as 0
  per_correct_arnn <- 0
  per_correct_rndf <- 0
  per_correct_nbay <- 0
  per_correct_mode <- 0

  # creates empty vector for each classifier to fill with percent correct values
  per_correct_arnns <- vector()
  per_correct_rndfs <- vector()
  per_correct_nbays <- vector()
  per_correct_modes <- vector()

  false_pos_arnns <- vector()
  false_pos_rndfs <- vector()
  false_pos_nbays <- vector()
  false_pos_modes <- vector()

  false_neg_arnns <- vector()
  false_neg_rndfs <- vector()
  false_neg_nbays <- vector()
  false_neg_modes <- vector()

  # sums up confusion matrices across each iteraion i, for each classifier
  # calculates percent correct for the confusion matrix in each iteration for each classifier and stores it in vector
  i <- 1
  for(i in 1:length(results_nn)){

    # confusion matrix for each classifier
    confusion_arnn <- confusion_arnn + confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "arnn_result")
    confusion_arnn
    confusion_rndf <- confusion_rndf + confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "rndf_result")
    confusion_rndf
    # confusion_nbay <- confusion_nbay + confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "nbay_result")
    confusion_mode <- confusion_mode + confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "mode_result")
    confusion_mode

    # percent correct for each classifier
    per_correct_arnns <- c(per_correct_arnns, per_correct(confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "arnn_result")))
    per_correct_arnns
    per_correct_rndfs <- c(per_correct_rndfs, per_correct(confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "rndf_result")))
    per_correct_rndfs
    # per_correct_nbays <- c(per_correct_nbays, per_correct(confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "nbay_result")))
    per_correct_modes <- c(per_correct_modes, per_correct(confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "mode_result")))
    per_correct_modes

    # percent correct for each classifier
    false_pos_arnns <- c(false_pos_arnns, confusion_arnn[2, 1]/sum(confusion_arnn))
    false_pos_arnns
    false_pos_rndfs <- c(false_pos_rndfs, confusion_rndf[2, 1]/sum(confusion_rndf))
    false_pos_nbays <- c(false_pos_nbays, confusion_nbay[2, 1]/sum(confusion_nbay))
    false_pos_modes <- c(false_pos_modes, confusion_mode[2, 1]/sum(confusion_mode))

    # percent correct for each classifier
    false_neg_arnns <- c(false_neg_arnns, confusion_arnn[1, 2]/sum(confusion_arnn))
    false_neg_arnns
    false_neg_rndfs <- c(false_neg_rndfs, confusion_rndf[1, 2]/sum(confusion_rndf))
    false_neg_nbays <- c(false_neg_nbays, confusion_nbay[1, 2]/sum(confusion_nbay))
    false_neg_modes <- c(false_neg_modes, confusion_mode[1, 2]/sum(confusion_mode))

  }

    # list containing percent correct for each iteration in each classifier
  per_confusion_resu <- list(
    per_confusion_arnns = confusion_arnn/sum(confusion_arnn), 
    per_confusion_rndfs = confusion_rndf/sum(confusion_rndf),
    per_confusion_nbays = confusion_nbay/sum(confusion_nbay), 
    per_confusion_modes = confusion_mode/sum(confusion_mode)
    )

  # list containing percent correct for each iteration in each classifier
  per_correct_resu <- list(
    per_correct_arnns = per_correct_arnns, 
    per_correct_rndfs = per_correct_rndfs,
    per_correct_nbays = per_correct_nbays, 
    per_correct_modes = per_correct_modes
    )

  # list containing percent correct for each iteration in each classifier
  false_pos_resu <- list(
    false_pos_arnns = false_pos_arnns, 
    false_pos_rndfs = false_pos_rndfs,
    false_pos_nbays = false_pos_nbays, 
    false_pos_modes = false_pos_modes
    )

  # list containing percent correct for each iteration in each classifier
  false_neg_resu <- list(
    false_neg_arnns = false_neg_arnns, 
    false_neg_rndfs = false_neg_rndfs,
    false_neg_nbays = false_neg_nbays, 
    false_neg_modes = false_neg_modes
    )

  # print mean percent correct for each classifier
  print(Outputs[o])
  print(paste("per_correct_arnn: ", formatC(round(mean(per_correct_arnns), digits = 3), format = "f", digits = 3), ", false_pos_arnn: ", formatC(round(mean(false_pos_arnns), digits = 3), format = "f", digits = 3), ", false_neg_arnn: ", formatC(round(mean(false_neg_arnns), digits = 3), format = "f", digits = 3), sep = ""))
  print(paste("per_correct_rndf: ", formatC(round(mean(per_correct_rndfs), digits = 3), format = "f", digits = 3), ", false_pos_rndf: ", formatC(round(mean(false_pos_rndfs), digits = 3), format = "f", digits = 3), ", false_neg_rndf: ", formatC(round(mean(false_neg_rndfs), digits = 3), format = "f", digits = 3), sep = ""))
  print(paste("per_correct_nbay: ", formatC(round(mean(per_correct_nbays), digits = 3), format = "f", digits = 3), ", false_pos_nbay: ", formatC(round(mean(false_pos_nbays), digits = 3), format = "f", digits = 3), ", false_neg_nbay: ", formatC(round(mean(false_neg_nbays), digits = 3), format = "f", digits = 3), sep = ""))
  print(per_confusion_resu)
  print("")

}

print(paste("per_confusion_resu", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = ""))
save(per_confusion_resu, file = paste("per_confusion_resu", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = ""))

# saves percent correct information
print(paste("per_correct_video", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = ""))
save(per_correct_resu, file = paste("per_correct_video", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = ""))

# saves false positive information
print(paste("false_pos_video", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = ""))
save(false_pos_resu, file = paste("false_pos_video", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = ""))

# saves false negative information
print(paste("false_neg_video", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = ""))
save(false_neg_resu, file = paste("false_neg_video", station, lead_time, paste(Outputs, collapse = ""), ".RData", sep = ""))

print(per_confusion_resu)