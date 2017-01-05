# load required packages
require(lubridate)
require(stringr)
require(foreach)
require(randomForest)

# set working directory
setwd("/home/jim/Dropbox/R/randomforest_fog")

# set number of cross validations
N <- 10

# source functions
source('Video_functions.r')

# sets cores as n_cores = number of available cores - 1
require(parallel)
n_cores = detectCores(all.tests = FALSE, logical = FALSE) - 1  # n_cores = # cores for parallel
require(doParallel)
registerDoParallel(cores = n_cores)

# read in data
metar <- read.csv(file = "YMHB.txt", header = TRUE, skip = 5)
metar <- data.frame(metar)

# create date, year, month etc columns
metar$valid <- as.POSIXct(metar$valid, tz = "UTC")
metar$date <- date(as.POSIXct(metar$valid, tz = "UTC"))
metar$year <- year(as.POSIXct(metar$valid, tz = "UTC"))
metar$month <- month(as.POSIXct(metar$valid, tz = "UTC"))
metar$day <- day(as.POSIXct(metar$valid, tz = "UTC"))
metar$hour <- hour(as.POSIXct(metar$valid, tz = "UTC"))
metar$minute <- minute(as.POSIXct(metar$valid, tz = "UTC"))

# set all entries as character
metar <- data.frame(lapply(metar, as.character), stringsAsFactors = FALSE)

# format hour and minute with leading zeroes
metar$hour <- formatC(as.integer(metar$hour), width = 2, flag = "0")
metar$minute <- formatC(as.integer(metar$minute), width = 2, flag = "0")

# # delete problem observations, subset to unique
# delete <- which(metar$metar %in% "YBBN 302000Z 05011KT 9999 FEW010 BKN060 BKN130 23/18 Q1012 RMK DISTANT SH TO W AND E INTER 2000/2300 3000 SHRA BKN010")
# metar <- metar[-delete, ]
metar <- unique(metar)
head(metar)

# all missing data as -9999
metar[metar == "M"] <- -9999

# subset to 00 and 30 minutes
metar <- metar[metar$minute %in% c("00", "30"), ]

# # restricts to dates with 48 exactly observations
# length(unique(metar$date))
# agg <- aggregate(metar$date, by = list(metar$date), FUN = length)
# dates_48_obs <- agg[agg$x == 48, ]$Group.1
# length(unique(dates_48_obs))
# metar <- metar[metar$date %in% dates_48_obs, ]

# sets Variables, numeric Variables_n, and factor Variables_f
Outputs <- "FG_or_BR"
Variables <- c("station", "tmpf", "dwpf", "relh", "drct", "sknt", "p01i", "alti", "mslp", "vsby", "gust", "skyc1", "skyc2", "skyc3", "skyc4", "skyl1", "skyl2", "skyl3", "skyl4", "presentwx", "year", "month", "day", "hour", "minute")
Variables_n <- c("tmpf", "dwpf", "relh", "drct", "sknt", "p01i", 
"alti", "mslp", "vsby", "gust", "skyl1", "skyl2", "skyl3", "skyl4")
Variables_f <- Variables[!(Variables %in% Variables_n)]

# subsets metar to Variables, and date
metar <- metar[, names(metar) %in% c(Variables, "date")]

FG_or_BR_f <- function(date) {
  date_f <- date(as.POSIXct(date, tz = "UTC"))
  presentwx_date <- metar[(metar$date == date & metar$hour >= 2) | (metar$date == as.character(date_f + 1) & metar$hour < 2), ]
  require(stringr)
  n_fg_br <- length(na.omit(c(as.vector(str_match(pattern = ".*FG.*", string = presentwx_date)), as.vector(str_match(pattern = ".*BR.*", string = presentwx_date)))))
  if(n_fg_br == 0){
    return(0)
  } else {
    return(1)
  }
}

# prints hours that fog occured at the airport
# use this to set current_hour_utc
metar$presentwx <- as.character(metar$presentwx)
FG_or_BR_presentwx <- c(as.vector(na.omit(str_match(pattern = ".*FG.*", string = metar$presentwx))), as.vector(na.omit(str_match(pattern = ".*BR.*", string = metar$presentwx))))
FG_or_BR_presentwx <- FG_or_BR_presentwx[!(FG_or_BR_presentwx %in% c(as.vector(na.omit(str_match(pattern = ".*RA.*", string = FG_or_BR_presentwx))), as.vector(na.omit(str_match(pattern = ".*DZ.*", string = FG_or_BR_presentwx)))))]
FG_or_BR_presentwx
t(as.matrix(table(metar[metar$presentwx %in% FG_or_BR_presentwx, ]$hour)))
# unique(sort(metar[metar$presentwx %in% FG_or_BR_presentwx, ]$hour))
# min(as.integer(metar[metar$presentwx %in% FG_or_BR_presentwx, "hour"]))
current_hour_utc <- 9
metar_time <- metar[as.integer(metar$hour) <= current_hour_utc & as.integer(metar$hour) >= current_hour_utc - 3, ]
metar_time <- metar_time[!(metar_time$presentwx %in% FG_or_BR_presentwx), ]

# recasts data with dates as rows and a column for each variable, hour, minute
require(reshape2)
m <- melt(metar, id.vars = c("date", "hour", "minute"))
head(m)
data_train <- dcast(data = m, formula = date ~ variable + hour + minute, value.var = "value", fun.aggregate = function(x) x[1], fill = "-9999")
data_train[is.na(data_train)] <- "-9999"

# adds column with 1 for FG/BR if occured between 02-02, 0 otherwise 
data_train$FG_or_BR <- unlist(lapply(data_train$date, FG_or_BR_f))
table(data_train$FG_or_BR)
unique(data_train$FG_or_BR)

# sets inputs as Variables with hour, minute
Inputs <- unlist(lapply(X = Variables, FUN = function(Variable) na.omit(str_match(pattern = paste(".*", Variable, ".*", sep = ""), string = names(data_train)))))
Inputs_n <- unlist(lapply(X = Variables_n, FUN = function(Variable) na.omit(str_match(pattern = paste(".*", Variable, ".*", sep = ""), string = names(data_train)))))
Inputs_f <- unlist(lapply(X = Variables_f, FUN = function(Variable) na.omit(str_match(pattern = paste(".*", Variable, ".*", sep = ""), string = names(data_train)))))

# sets numeric Inputs_n as numeric, factor Inputs_f as factor
data_train[, Inputs_n] <- lapply(data_train[, Inputs_n], as.numeric)
data_train[, Inputs_f] <- lapply(data_train[, Inputs_f], as.factor)

# ---------------------------------------------
# - Create formula for Outputs ~ Inputs

Outputs_f <- vector()
for(Output in Outputs) {
  Outputs_f_o <- unique(data_train[, Output])
  for(Output_f_o in Outputs_f_o) {
    data_train[, paste(Output, Output_f_o, sep = "")] <- as.numeric(ifelse(data_train[, Output] == Output_f_o, 1, 0))
    print(paste(Output, " ", Output_f_o, sep = ""))
    Outputs_f <- c(Outputs_f, paste(Output, Output_f_o, sep = ""))
  }
  data_train[, Output] <- factor(data_train[, Output])
}

# formula for factors, and for regular
formula_f <- as.formula(paste(paste(Outputs_f, collapse = "+"), "~", paste(Inputs, collapse = "+")))
formula_f_n <- as.formula(paste(paste(Outputs_f, collapse = "+"), "~", paste(Inputs_n, collapse = "+")))
formula <- as.formula(paste(paste(paste("factor(", Outputs, ")", sep = ""), collapse = "+"), "~", paste(Inputs, collapse = "+")))

formula
formula_f

# ---------------------------------------------
# - Cross validation. For each i = 1,... N, randomly split the data into two pieces, one consisting of 2/3 of the Set/Trap combinations and 1/3 of the Set/Trap combinations. Train each classifier on 2/3 data, and test on 1/3, and store the result

# empty result list to fill
results <- vector("list", N)

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
  write.csv(data_tra, "data_tra.csv")

  data_tes <- data_train[-train, ]
  head(data_tes)

  tryCatch({

    # train artificial neural network on training data_train
    require(neuralnet)
    formula_f
    arnn <- neuralnet(formula_f_n, rep = 1, stepmax = 10^5, data = data_tra)
    # run classifier on test data, and store result
    arnn_result <- compute(x = arnn, data_tes[, Inputs_n])$net.result
    arnn_result <- as.data.frame(arnn_result)
    names(arnn_result) <- Outputs_f
    arnn_result <- apply(arnn_result, FUN = function(x) ifelse(x < 0, 0, ifelse(x > 1, 1, x)), MARGIN = 2)
    arnn_result

    # random forest and naive bays
    rndf_result <- matrix(NA, nrow = nrow(data_tes), ncol = length(Outputs_f))
    rndf_result <- as.data.frame(rndf_result)
    names(rndf_result) <- Outputs_f
    rndf_result

    nbay_result <- matrix(NA, nrow = nrow(data_tes), ncol = length(Outputs_f))
    nbay_result <- as.data.frame(nbay_result)
    names(nbay_result) <- Outputs_f
    nbay_result

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
      rndf_result

    #   # train naive bayes on training data
    #   require(klaR)
    #   nbay <- NaiveBayes(formula = as.formula(paste("factor(", Output, ")", "~", paste(Inputs, collapse = "+")))
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
    mode_result

    # # sets P_var and P_val
    # if(Outputs %in% P$P_Outputs) {
    #   P_var = as.character(P[Outputs == P$P_Outputs, "P_var"])
    #   P_val = as.numeric(P[Outputs == P$P_Outputs, "P_val"])
    # } else {
    #   P_var = NA
    #   P_val = NA
    # }

    P_var = NA
    P_val = NA

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
print(paste("results", metar$station[1], paste(Outputs, collapse = ""), ".RData", sep = ""))
save(results, file = paste("results", metar$station[1], paste(Outputs, collapse = ""), ".RData", sep = ""))

# ---------------------------------------------
# - Print and store the average percent correct from the cross validation

# create empty list to fill with percent correct for each classifier
per_correct_resu <- list()
for(o in seq(1, length(Outputs), 1)) {

  Output <- Outputs[o]

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
  for(i in 1:length(results_nn)){

    # confusion matrix for each classifier
    confusion_arnn <- confusion_arnn + confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "arnn_result")
    confusion_rndf <- confusion_rndf + confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "rndf_result")
    # confusion_nbay <- confusion_nbay + confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "nbay_result")
    confusion_mode <- confusion_mode + confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "mode_result")

    # percent correct for each classifier
    per_correct_arnns <- c(per_correct_arnns, per_correct(confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "arnn_result")))
    per_correct_rndfs <- c(per_correct_rndfs, per_correct(confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "rndf_result")))
    # per_correct_nbays <- c(per_correct_nbays, per_correct(confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "nbay_result")))
    per_correct_modes <- c(per_correct_modes, per_correct(confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "mode_result")))

    # percent correct for each classifier
    false_pos_arnns <- c(false_pos_arnns, confusion_arnn[2, 1]/sum(confusion_arnn))
    false_pos_rndfs <- c(false_pos_rndfs, confusion_rndf[2, 1]/sum(confusion_rndf))
    false_pos_nbays <- c(false_pos_nbays, confusion_nbay[2, 1]/sum(confusion_nbay))
    false_pos_modes <- c(false_pos_modes, confusion_mode[2, 1]/sum(confusion_mode))

    # percent correct for each classifier
    false_neg_arnns <- c(false_neg_arnns, confusion_arnn[1, 2]/sum(confusion_arnn))
    false_neg_rndfs <- c(false_neg_rndfs, confusion_rndf[1, 2]/sum(confusion_rndf))
    false_neg_nbays <- c(false_neg_nbays, confusion_nbay[1, 2]/sum(confusion_nbay))
    false_neg_modes <- c(false_neg_modes, confusion_mode[1, 2]/sum(confusion_mode))

  }

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
  print(paste("per_correct_mode: ", formatC(round(mean(per_correct_modes), digits = 3), format = "f", digits = 3), ", false_pos_mode: ", formatC(round(mean(false_pos_modes), digits = 3), format = "f", digits = 3), ", false_neg_mode: ", formatC(round(mean(false_neg_modes), digits = 3), format = "f", digits = 3), sep = ""))
  print("")

}

# saves percent correct information
print(paste("per_correct_video", metar$station[1], paste(Outputs, collapse = ""), ".RData", sep = ""))
save(per_correct_resu, file = paste("per_correct_video", metar$station[1], paste(Outputs, collapse = ""), ".RData", sep = ""))

# saves false positive information
print(paste("false_pos_video", metar$station[1], paste(Outputs, collapse = ""), ".RData", sep = ""))
save(false_pos_resu, file = paste("false_pos_video", metar$station[1], paste(Outputs, collapse = ""), ".RData", sep = ""))

# saves false negative information
print(paste("false_neg_video", metar$station[1], paste(Outputs, collapse = ""), ".RData", sep = ""))
save(false_neg_resu, file = paste("false_neg_video", metar$station[1], paste(Outputs, collapse = ""), ".RData", sep = ""))

