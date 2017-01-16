# FogPredictor_data.r

if(file.exists(paste("data_train", station, lead_time, ".RData", sep = "")) == FALSE | load_data == FALSE) {

  print('Starting: calculate data_train from metar')

  # subset to minutes until fog > lead_time
  metar_leadtime <- metar[metar$minutes_until_fog >= lead_time*60 & metar$minutes_until_fog <= (lead_time + 3)*60, ]

  # recasts data with dates as rows and a column for each variable, hour, minute
  require(reshape2)
  m <- melt(metar_leadtime, id.vars = c("date", "minutes_until_fog"))
  data_train <- dcast(data = m, formula = date ~ variable + minutes_until_fog, value.var = "value", fun.aggregate = function(x) x[1], fill = "-9999")
  data_train[is.na(data_train)] <- "-9999"
  data_train <- data_train[!(data_train$date == "-9999"), ]

  # load fog info, includes first_fog_time, and FG_or_BR (1 if fog or mist, 0 otherwise)
  load(file = paste("fog_info", station, ".RData", sep = ""))
  head(fog_info)
  
  # merges fog_info with data_train
  require(data.table)
  data_train <- data.table(data_train)
  data_train <- data_train[fog_info, on= "date"]
  data_train <- as.data.frame(data_train)
  data_train[is.na(data_train)] <- "-9999"

  # sets inputs as Variables with lead time (or hour, minute)
  require(stringr)
  Inputs = unlist(lapply(X = Variables, FUN = function(Variable) na.omit(str_match(pattern = paste(".*", Variable, ".*", sep = ""), string = names(data_train)))))
  Inputs_n = unlist(lapply(X = Variables_n, FUN = function(Variable) na.omit(str_match(pattern = paste(".*", Variable, ".*", sep = ""), string = names(data_train)))))
  Inputs_f = unlist(lapply(X = Variables_f, FUN = function(Variable) na.omit(str_match(pattern = paste(".*", Variable, ".*", sep = ""), string = names(data_train)))))

  # sets numeric Inputs_n as numeric, factor Inputs_f as factor
  data_train[, Inputs_n] <- lapply(data_train[, Inputs_n], as.numeric)
  data_train[, Inputs_f] <- lapply(data_train[, Inputs_f], as.factor)

  head(data_train)
  head(data_train[is.na(data_train)])
  unique(data_train$FG_or_BR)
  unique(data_train$FG_or_BR1)
  unique(data_train$FG_or_BR0)

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

  # save data_train
  save(data_train, file = paste("data_train", station, formatC(as.integer(lead_time), width = 2, flag = "0"), ".RData", sep = ""))

  print("Finished: calculate data_train from metar")

  } else {

  # load data_train
  load(data_train, file = paste("data_train", station, formatC(as.integer(lead_time), width = 2, flag = "0"), ".RData", sep = ""))

}

