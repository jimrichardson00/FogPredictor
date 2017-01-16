# FogPredictor_master.r

# removes all variables
rm(list = ls())

# --------------------------------
# sets control parameters

# sets working directory
master_dir = "/home/jim/Dropbox/R/FogPredictor"
setwd(master_dir)

# sets station, YPPH for Perth, YBBN for Brisbane etc
station = "YPPH"

# P: sets the minimum cut-off for classification.
# classification returns a probablity for each class, there are two options:
#   1) default option is to classify video according to class with highest probability
#   2) can preset min cut off, for example can set P_var = "Clear", P_val = 0.2
#       then videos classified as Clear with probability >= 20% will be classified as Clear.
# option 2 is used to reduce false negatives
P = data.frame( 
  P_Outputs = c("FG_or_BR") 
  , P_var = c("1") 
  , P_val = c(0.5) 
  ) 

# sets cores as n_cores = number of available cores - 1
require(parallel)
n_cores = detectCores(all.tests = FALSE, logical = FALSE) - 1  # n_cores = # cores for parallel
require(doParallel)
registerDoParallel(cores = n_cores)

# sets Variables, numeric Variables_n, and factor Variables_f
presentwx = c("FG", "BR")
Outputs = paste(presentwx, collapse = "_or_")
Variables = c("tmpf", "dwpf", "relh", "drct", "sknt", "p01i", "alti", "mslp", "vsby", "gust", "skyc1", "skyc2", "skyc3", "skyc4", "skyl1", "skyl2", "skyl3", "skyl4", "presentwx", "year", "month", "day", "hour", "minute")
Variables_n = c("tmpf", "dwpf", "relh", "drct", "sknt", "p01i", "alti", "mslp", "vsby", "gust", "skyl1", "skyl2", "skyl3", "skyl4")
Variables_f = Variables[!(Variables %in% Variables_n)]

# ---------------------------------
# sources functions
print("Sourcing FogPredictor_functions.r")
source('FogPredictor_functions.r')

# ---------------------------------
# runs load metar
load_metar = TRUE
source("FogPredictor_metar.r")

# ---------------------------------
# runs load data

# sets split_hour
split_hour = 3
# sets lead time, in hours
lead_time = 2
# sets whether or not to load data_train, or calculate it from METAR
load_data = FALSE

# load data
print("Sourcing FogPredictor_data.r")
source("FogPredictor_data.r")

# # ---------------------------------
# # run training file

# set number of cross validations
N = 100
print("Sourcing FogPredictor_training.r")
source("FogPredictor_training.r")

# ---------------------------------
# loop on stations and lead_time
station = "YBBN"
lead_time = 1

stations = c("YPPH", "YBBN", "YMML", "YSSY", "YMHB", "YPAD")

for(station in stations) {

  print(paste("Station: ", station, sep = ""))

  print("Sourcing FogPredictor_metar.r")
  load_metar = TRUE
  source("FogPredictor_metar.r")

  for(lead_time in seq(1, 9, 1)) {

    # tryCatch({

    # # save data_train
    #   load(file = paste("data_train", station, lead_time, ".RData", sep = ""))
    #   # save data_train
    #   save(data_train, file = paste("data_train", station, formatC(as.integer(lead_time), width = 2, flag = "0"), ".RData", sep = ""))

    #   },
    #   error = function(e) {cat("ERROR :",conditionMessage(e), "\n")}
    #   )

    print(paste("Station: ", station, " Lead time: ", lead_time, sep = ""))

    print("Sourcing FogPredictor_data.r")
    load_data = TRUE
    source("FogPredictor_data.r")

    print("Sourcing FogPredictor_training.r")
    source("FogPredictor_training.r")

    print("")
  }
}

