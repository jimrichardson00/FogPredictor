# FogPredictor_data.r

# set working directory
setwd("/home/jim/Dropbox/R/FogPredictor")

if(file.exists(paste("data_train", station, ".RData", sep = "")) == FALSE | load_data == FALSE) {

  print('Starting: calculate data_train from metar')

  # read in data
  metar <- read.csv(file = paste(station, ".txt", sep = ""), header = TRUE, skip = 5)
  metar <- data.frame(metar)

  # create date, year, month etc columns
  require(lubridate)
  metar$valid <- as.POSIXct(metar$valid, tz = "UTC", origin = "1970-01-01")
  metar$date <- date(as.POSIXct(metar$valid, tz = "UTC", origin = "1970-01-01"))
  metar$year <- year(as.POSIXct(metar$valid, tz = "UTC", origin = "1970-01-01"))
  metar$month <- month(as.POSIXct(metar$valid, tz = "UTC", origin = "1970-01-01"))
  metar$day <- day(as.POSIXct(metar$valid, tz = "UTC", origin = "1970-01-01"))
  metar$hour <- hour(as.POSIXct(metar$valid, tz = "UTC", origin = "1970-01-01"))
  metar$minute <- minute(as.POSIXct(metar$valid, tz = "UTC", origin = "1970-01-01"))

  # set all entries as character
  metar <- data.frame(lapply(metar, as.character), stringsAsFactors = FALSE)

  # format hour and minute with leading zeroes
  metar$hour <- formatC(as.integer(metar$hour), width = 2, flag = "0")
  metar$minute <- formatC(as.integer(metar$minute), width = 2, flag = "0")

  # # delete problem observations in YBBN
  # delete <- which(metar$metar %in% "YBBN 302000Z 05011KT 9999 FEW010 BKN060 BKN130 23/18 Q1012 RMK DISTANT SH TO W AND E INTER 2000/2300 3000 SHRA BKN010")
  # metar <- metar[-delete, ]

  # subset to unique
  metar <- unique(metar)

  # all missing data as -9999
  metar[metar == "M"] <- "-9999"
  metar[metar == "\\"] <- "-9999"

  # remove entries with date missing
  metar <- metar[!(metar$date == "-9999"), ]

  # subset to 00 and 30 minutes
  metar <- metar[metar$minute %in% c("00", "30"), ]

  # sets Variables, numeric Variables_n, and factor Variables_f
  Outputs <- "FG_or_BR"
  Variables <- c("tmpf", "dwpf", "relh", "drct", "sknt", "p01i", "alti", "mslp", "vsby", "gust", "skyc1", "skyc2", "skyc3", "skyc4", "skyl1", "skyl2", "skyl3", "skyl4", "presentwx", "year", "month", "day", "hour", "minute")
  Variables_n <- c("tmpf", "dwpf", "relh", "drct", "sknt", "p01i", "alti", "mslp", "vsby", "gust", "skyl1", "skyl2", "skyl3", "skyl4")
  Variables_f <- Variables[!(Variables %in% Variables_n)]

  # subsets metar to Variables, and date, and valid
  metar <- metar[, names(metar) %in% c(Variables, "date", "valid", "station")]

  require(parallel)
  first_fog_time_v <- as.vector(unlist(mclapply(X = unique(metar$date), 
        FUN =first_fog_time_f
        , mc.cores = n_cores
        , mc.silent = FALSE
        , mc.preschedule = TRUE
        )))

  # merge fog_start_time, FG_or_BR_v into metar
  require(data.table)
  metar <- as.data.table(metar)
  metar <- metar[data.table(date = unique(metar$date), first_fog_time = first_fog_time_v), on = "date"]
  metar <- as.data.frame(metar)
  metar$valid <- as.POSIXct(metar$valid, tz = "UTC", origin = "1970-01-01")
  metar$first_fog_time <- as.POSIXct(metar$first_fog_time, tz = "UTC", origin = "1970-01-01")

  # save metar
  save(metar, file = paste("metar_", station, ".RData", sep = ""))

  # # load metar
  # load(file = paste("metar_", station, ".RData", sep = ""))

  # create minutes until fog
  metar$minutes_until_fog <- as.numeric(ceiling((metar$first_fog_time - metar$valid)/60))
  head(metar)

  # subset to minutes until fog > lead_time
  metar_leadtime <- metar[metar$minutes_until_fog >= lead_time*60 & metar$minutes_until_fog <= (lead_time + 3)*60, ]

  # recasts data with dates as rows and a column for each variable, hour, minute
  require(reshape2)
  m <- melt(metar_leadtime, id.vars = c("date", "minutes_until_fog"))
  head(m)
  data_train <- dcast(data = m, formula = date ~ variable + minutes_until_fog, value.var = "value", fun.aggregate = function(x) x[1], fill = "-9999")
  data_train[is.na(data_train)] <- "-9999"
  data_train <- data_train[!(data_train$date == "-9999"), ]
  head(data_train)

  # adds column with 1 for FG/BR if occured between split_hourZ-split_hourZ, 0 otherwise 
  data_train$FG_or_BR <- unlist(mclapply(data_train$date
    , FUN = FG_or_BR_f
    , mc.cores = n_cores
    , mc.silent = FALSE
    , mc.preschedule = TRUE
    ))
  table(data_train$FG_or_BR)
  unique(data_train$FG_or_BR)

  # sets inputs as Variables with lead time (or hour, minute)
  require(stringr)
  Inputs = unlist(lapply(X = Variables, FUN = function(Variable) na.omit(str_match(pattern = paste(".*", Variable, ".*", sep = ""), string = names(data_train)))))
  Inputs_n = unlist(lapply(X = Variables_n, FUN = function(Variable) na.omit(str_match(pattern = paste(".*", Variable, ".*", sep = ""), string = names(data_train)))))
  Inputs_f = unlist(lapply(X = Variables_f, FUN = function(Variable) na.omit(str_match(pattern = paste(".*", Variable, ".*", sep = ""), string = names(data_train)))))

  # sets numeric Inputs_n as numeric, factor Inputs_f as factor
  data_train[, Inputs_n] <- lapply(data_train[, Inputs_n], as.numeric)
  data_train[, Inputs_f] <- lapply(data_train[, Inputs_f], as.factor)

  head(data_train)

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
  head(data_train)

  # save data_train
  save(data_train, file = paste("data_train", station, ".RData", sep = ""))

  print("Finished: calculate data_train from metar")

  } else {

  # load data_train
  load(file = paste("data_train", station, ".RData", sep = ""))

}

