# ------------------------------------------------------
# load video log data
setwd(master_dir)
if(mac == FALSE) {

  require(Hmisc)
  # STRS2012_analysis <- mdb.get("2012_Survey_Video_Analysis.mdb")
  STRS2013_analysis <- mdb.get("2013_Survey_Video_Analysis.mdb")
  SKBO2013_analysis <- mdb.get("2013_SKB_Video_Analysis.mdb")
  SKBO2014_analysis <- mdb.get("2014_SKB_Video_Analysis.mdb")

  require(plyr)
  Video_Analysis_Log <- rbind.fill(list(
    # data.frame(STRS2012_analysis$data, TripCode = rep("STRS2012", nrow(STRS2012_analysis$data))), 
    data.frame(STRS2013_analysis$data, TripCode = rep("STRS2013", nrow(STRS2013_analysis$data))), 
    data.frame(SKBO2013_analysis$data, TripCode = rep("SKBO2013", nrow(SKBO2013_analysis$data))), 
    data.frame(SKBO2014_analysis$data, TripCode = rep("SKBO2014", nrow(SKBO2014_analysis$data)))
    )
  )

  require(stringr)
  Video_Analysis_Log$Set <- as.integer(str_match(tolower(as.character(Video_Analysis_Log$TransectName)), "set(\\d+)")[, 2])
  Video_Analysis_Log$Set[is.na(Video_Analysis_Log$Set)] <- 0
  Video_Analysis_Log$Set <- formatC(Video_Analysis_Log$Set, width = 3, flag = "0")
  Video_Analysis_Log$Trap <- as.integer(str_match(tolower(as.character(Video_Analysis_Log$TransectName)), "trap(\\d+)")[, 2])
  Video_Analysis_Log$Trap[is.na(Video_Analysis_Log$Trap)] <- 0
  Video_Analysis_Log$Trap <- formatC(Video_Analysis_Log$Trap, width = 3, flag = "0")
  Video_Analysis_Log$Camera <- as.integer(str_match(tolower(as.character(Video_Analysis_Log$TransectName)), "camera(\\d+)")[, 2])
  Video_Analysis_Log$Camera[is.na(Video_Analysis_Log$Camera)] <- 0
  Video_Analysis_Log$Camera <- formatC(Video_Analysis_Log$Camera, width = 3, flag = "0")
  
  Video_Analysis_Log[Video_Analysis_Log$TripCode == "SKBO2014", "Trap"] <- "000"
  Video_Analysis_Log[Video_Analysis_Log$TripCode == "STRS2013", "Camera"] <- "000"
  Video_Analysis_Log[Video_Analysis_Log$TripCode == "STRS2014", "Camera"] <- "000"

  Video_Analysis_Log$Filenames <- paste(
    Video_Analysis_Log$TripCode, "_", 
    "S", Video_Analysis_Log$Set, 
    "T", Video_Analysis_Log$Trap,
    "C", Video_Analysis_Log$Camera, "_", 
    str_match(Video_Analysis_Log$FileName, "(.+)\\.MP4")[, 2]
   , sep = "")

  SpeciesIDs <- as.character(sort(unique(Video_Analysis_Log$SpeciesID)))
  SpeciesIDs

  require(parallel)
  SpeciesPresenceAbsence_l <- mclapply(Video_Analysis_Log$Filenames, FUN = function(Filename) 
    as.character(ifelse(SpeciesIDs %in% as.character(Video_Analysis_Log[Video_Analysis_Log$Filenames == Filename, "SpeciesID"]), "1", "0"))
  ) 

  SpeciesPresenceAbsence <- matrix(unlist(SpeciesPresenceAbsence_l), nrow = length(SpeciesPresenceAbsence_l), byrow = TRUE)
  SpeciesPresenceAbsence <- as.data.frame(SpeciesPresenceAbsence)
  names(SpeciesPresenceAbsence) <- paste("Species_", SpeciesIDs, sep = "")
  SpeciesPresenceAbsence$Filenames <- Video_Analysis_Log$Filenames

  Species <- as.vector(na.omit(str_match(names(SpeciesPresenceAbsence), "Species_.+")))
  Species
  SpeciesPresenceAbsence$Species_ <- ifelse(rowSums(SpeciesPresenceAbsence[, Species] == "1") == 0, "0", "1")

  require(plyr)
  d <- ddply(.data = Video_Analysis_Log, .variables = .(TransectName, FileName, Set, Trap, Camera, Filenames), .fun = summarise, maxID = max(ID))

  Video_Analysis_Log_m <- merge(x = Video_Analysis_Log, y = d, by.x = "Filenames", by.y = "Filenames", all = FALSE)
  Video_Analysis_Log_mu <- Video_Analysis_Log_m[Video_Analysis_Log_m$ID == Video_Analysis_Log_m$maxID, ]

  Video_Analysis_Log_mu[is.na(Video_Analysis_Log_mu) == TRUE | Video_Analysis_Log_mu == ""] <- "NA"
  for(name in names(Video_Analysis_Log_mu)){
    Video_Analysis_Log_mu[, name] <- as.character(Video_Analysis_Log_mu[, name])
  }

  Video_Analysis_Log_mu  <- merge(x = Video_Analysis_Log_mu, y = STRS2013_analysis$lu_sounds, by.x = "Sounds", by.y = "SoundsID", all.x = TRUE)
  Video_Analysis_Log_mu  <- merge(x = Video_Analysis_Log_mu, y = STRS2013_analysis$lu_substrate, by.x = "DominantSubstrate", by.y = "SubstrateId", all.x = TRUE)
  Video_Analysis_Log_mu  <- merge(x = Video_Analysis_Log_mu, y = unique(SpeciesPresenceAbsence), by.x = "Filenames", by.y = "Filenames", all.x = TRUE)

  Video_Analysis_Log_mu$SoundName <- gsub("/", "_", Video_Analysis_Log_mu$SoundName)

  Video_Analysis_Log_mu[is.na(Video_Analysis_Log_mu) == TRUE | Video_Analysis_Log_mu == ""] <- "NA"
  for(name in names(Video_Analysis_Log_mu)){
    Video_Analysis_Log_mu[, name] <- as.character(Video_Analysis_Log_mu[, name])
  }

  write.csv(Video_Analysis_Log_mu, "Video_Analysis_Log_mu.csv")

  data_analy <- Video_Analysis_Log_mu
  save(data_analy, file = "data_analy.RData")

} else {

  Video_Analysis_Log_mu <- read.csv("Video_Analysis_Log_mu.csv")

  Video_Analysis_Log_mu[is.na(Video_Analysis_Log_mu) == TRUE | Video_Analysis_Log_mu == ""] <- "NA"
  for(name in names(Video_Analysis_Log_mu)){
    Video_Analysis_Log_mu[, name] <- as.character(Video_Analysis_Log_mu[, name])
  }

  load(file = "data_analy.RData")

}


