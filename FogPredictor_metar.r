# FogPredictor_metar.r

if(file.exists(paste("metar", station, ".RData", sep = "")) == FALSE | load_metar == FALSE) {

  print('Starting: creating metar data')

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
  metar[is.na(metar)] <- "-9999"

  # add column with dir as NNW etc
  metar$drct_f <- unlist(lapply(metar$drct, FUN = dir_to_compass))

  # remove entries with date missing
  metar <- metar[!(metar$date == "-9999"), ]

  # subset to 00 and 30 minutes
  metar <- metar[metar$minute %in% c("00", "30"), ]

  # convert farenheit to celcius and calculate dewpoint depression
  require(weathermetrics)
  metar$tmpf_c <- ifelse(metar$tmpf == "-9999", "-9999", as.character((as.numeric(metar$tmpf) - 32)*(5/9)))
  metar$dwpf_c <- ifelse(metar$dwpf == "-9999", "-9999", as.character((as.numeric(metar$dwpf) - 32)*(5/9)))
  metar$dewptdep_c <- ifelse(metar$tmpf == "-9999" | metar$dwpf == "-9999", "-9999", as.character(1.0*(as.numeric(metar$tmpf_c) - as.numeric(metar$dwpf_c))))
  head(metar)

  # sets Variables, numeric Variables_n, and factor Variables_f
  Outputs <- "FG_or_BR"
  Variables <- c("tmpf_c", "dwpf_c", "dewptdep_c", "relh", "drct", "sknt", "p01i", "alti", "mslp", "vsby", "gust", "skyc1", "skyc2", "skyc3", "skyc4", "skyl1", "skyl2", "skyl3", "skyl4", "presentwx", "year", "month", "day", "hour", "minute")
  Variables_n <- c("tmpf_c", "dwpf_c", "dewptdep_c", "relh", "drct", "sknt", "p01i", "alti", "mslp", "vsby", "gust", "skyl1", "skyl2", "skyl3", "skyl4")
  Variables_f <- Variables[!(Variables %in% Variables_n)]

  # subsets metar to Variables, and date, and valid
  metar <- metar[, names(metar) %in% c(Variables, "date", "valid", "station")]

  if(file.exists(paste("fog_info", station, ".RData", sep = "")) == FALSE) {

    require(parallel)
    first_fog_time_v <- as.vector(unlist(mclapply(X = unique(metar$date), 
          FUN = first_fog_time_f
          , mc.cores = n_cores
          , mc.silent = FALSE
          , mc.preschedule = TRUE
          )))

    FG_or_BR_v <- as.vector(unlist(mclapply(X = unique(metar$date), 
          FUN = FG_or_BR_f
          , mc.cores = n_cores
          , mc.silent = FALSE
          , mc.preschedule = TRUE
          )))

    # create and save fog info
    fog_info <- data.table(date = unique(metar$date), first_fog_time = first_fog_time_v, FG_or_BR = FG_or_BR_v)
    save(fog_info, file = paste("fog_info", station, ".RData", sep = ""))

  } else {

    # load fog info
    load(file = paste("fog_info", station, ".RData", sep = ""))

  }


  # merge fog_start_time, FG_or_BR_v into metar
  require(data.table)
  metar <- as.data.table(metar)
  metar <- metar[fog_info[, c("date", "first_fog_time")], on = "date"]
  metar <- as.data.frame(metar)
  metar$valid <- as.POSIXct(metar$valid, tz = "UTC", origin = "1970-01-01")
  metar$first_fog_time <- as.POSIXct(metar$first_fog_time, tz = "UTC", origin = "1970-01-01")

  # create minutes until fog
  metar$minutes_until_fog <- as.numeric(ceiling((metar$first_fog_time - metar$valid)/60))
  head(metar)

  # save metar
  save(metar, file = paste("metar", station, ".RData", sep = ""))

  } else {

  # load metar
  load(file = paste("metar", station, ".RData", sep = ""))

}

