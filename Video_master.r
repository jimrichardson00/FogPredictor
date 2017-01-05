#-------------------------------------------------------------
#
#  Author:    Jim Richardson
#  Project:   Video classification algorithm for ocean-floor fish trap video
#  Written:   Apr 2015
#
#  Goal:      Automatic classification of video taken from cameras attached
#             to fish traps placed on ocean floor. Algorithm has two stages:
#               1) Classify video into Clear/Cloudy
#               2) Classify clear video into Presence/Absence of biotic material
#
#-------------------------------------------------------------

#-------------------------------------------------------------
# control parameters

rm(list = ls())

# sets cores as n_cores = number of available cores - 1
require(parallel)
n_cores = detectCores(all.tests = FALSE, logical = FALSE) - 1  # n_cores = # cores for parallel
require(doParallel)
registerDoParallel(cores = n_cores)

Wgp = 1920  # width of gopro video frames in pixels
Hgp = 1080  # height of gopro video frames in pixels
W = 230   # width of resized video frame (after trap removal)
H = 177   # height of resized vidoe frame (after trap removal)
skip = 6  # number of seconds to skip on video (camera light takes ~6 seconds to turn on)
# x1, x2, y1, y2: inputs for trap removal
x1 = 0.26041666666
x2 = 0.73958333334
y1 = 0.34444444444
y2 = 1.0
# P: sets the minimum cut-off for classification.
# classification returns a probablity for each class, there are two options:
#   1) default option is to classify video according to class with highest probability
#   2) can preset min cut off, for example can set P_var = "Clear", P_val = 0.2
#       then videos classified as Clear with probability >= 20% will be classified as Clear.
# option 2 is used to reduce false negatives
P = data.frame( 
  P_Outputs = c("ImageClarity", "BioticMaterial") 
  , P_var = c(NA, NA) 
  , P_val = c(NA, NA) 
  ) 
# sets trip id (STRS, SKBO) and year (2013, 2014..). set year = "" for all data
year = "" 
# year = "STRS2013" 
# year = "STRS2014" 
# year = "SKBO2013" 
# year = "SKBO2014" 
mac = TRUE  # mac = TRUE/FALSE. TRUE for running on mac, avoids parallel processing (there is a bug when ran with parallel processing on mac) and sets working directory
# mac = FALSE
# if mac = TRUE/FALSE sets working directory
if(mac == TRUE) { 
  master_dir = "/Users/jimrichardson/Dropbox/REM/Tasks/video_cluster" # sets master working directory
  } else { 
  master_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster" # sets master working directory
} 
frame_dir = paste(master_dir, "/frame", year, sep = "") # sets frame directory (jpgs of video frames)
video_dir = paste(master_dir, "/video", year, sep = "") # sets video directory (contains video files)
proje_dir = paste(master_dir, "/proje", year, sep = "") # sets proje directory (contains projections of video frames onto principal components)
clust_dir = paste(master_dir, "/clust", year, sep = "") # sets clust directory (contains manually and automatically identified classifications)
eigen_dir = paste(master_dir, "/eigen", year, sep = "") # sets eigen directory (contains eigenvectors from pca as .jpg files)

# --------------------------------------------
# sources functions needed
setwd(master_dir)
source("Video_functions.r")

# --------------------------------------------
# opens dialogue box named 'Initial run' with the following options:
# Y - Yes it is the initial run. This option will deleted all .txt files, .RData files, and clust_dir
# N - No this is not the initial run. This will determine the current videos and the new video files, then adds the new video files (without rerunning the current videos)

require(tcltk)      # Load the TclTk package
tt <- tktoplevel()  # Create a new toplevel window
tktitle(tt) <- "Initial run"  # Give the window a title
done <- tclVar(0)   # tclVar() creates a Tcl variable
Y.but <- tkbutton(tt, text = "  Y  ",
  command = function() tclvalue(done) <- 1)
N.but <- tkbutton(tt, text = "  N  ", 
  command = function() tclvalue(done) <- 0)
Cancel.but <- tkbutton(tt, text = "Cancel",
  command = function() tclvalue(done) <- 2)
# Place the two buttons on the same row in their assigned window (tt)
tkgrid(Y.but, N.but, Cancel.but)
# Capture the event "Destroy" (e.g. Alt-F4 in Windows) and when this happens,
# assign 2 to done
tkbind(tt, "<Destroy>", function() tclvalue(done) <- 2)
tkfocus(tt)         # Place the focus to our tk window
# Do not proceed with the following code until the variable done is non-zero.
# (but other processes can still run, i.e., the system is not frozen)
tkwait.variable(done)
# The variable done is now non-zero, so we would like to record its value before
# destroying the window tt.  If we destroy it first, then done will be set to 2
# because of our earlier binding, but we want to determine whether the user
# pressed OK (i.e., see whether done is equal to 1)
doneVal <- as.integer(tclvalue(done))   # Get and coerce content of a Tcl variable
tkdestroy(tt)

# Test the result
if (doneVal == 2) {
  stop("Cancelled!")
} else if (doneVal == 0) {
  inital_run <- FALSE
} else if (doneVal == 1) {
  inital_run <- TRUE
}

if(inital_run == TRUE) {
  setwd(master_dir)
  source("Video_cleaning.r")
}

# --------------------------------------------
# The code will copy a video file GOPRdddd.MP4 with trip code STRS2013 (for example) and folder SetXXXTrapYYYCameraZZZ into video_dir with filename: STRS2013_SXXXTYYYCZZZ_GOPRdddd.MP4. 

# list of directories that contain the videos for each trip (stored in subfolders of the form SetXXXTrapYYYCameraZZZ):
from_dirs = c(
  "/Users/jimrichardson/Desktop/2013 survey video", 
  "/Users/jimrichardson/Desktop/2014 survey video", 
  "/Users/jimrichardson/Desktop/SKB 2013 video",
  "/Users/jimrichardson/Desktop/2014 SKB video"
  )
# list of trip codes for each trip. must match with from_dirs.
from_idxs = c(
  "STRS2013", 
  "STRS2014", 
  "SKBO2013", 
  "SKBO2014"
  )
setwd(master_dir)
# source("Video_prepfile.r")

# --------------------------------------------
# Calculates average frame for each video, skipping the first few seconds (can customize this with variable 'skip'). Writes frame to frame_dir as a jpg.

setwd(master_dir)
# source("Video_frames.r")

# ---------------------------------------------------------------
# overlays frame from each video to see the max non trap area
setwd(master_dir)
# source("Video_overlay.r")

# --------------------------------------------
# For new files in frame_dir; removes trap, resizes frame, copies the data from the resulting image into a matrix.
# If principal component analysis (ccipca) already exists from previous data:
  # Adds new data to current ccipca
# If no ccipca exists:
  # Runs a ccipca on this matrix.
# Writes ccipca info (mean, components, iteration, copy, amnesic) to .txt files. 
# Reads in .txt files to R and saves all info (including data matrix, mean, components, and projection of data to components) to .RData file

rerun = 0
fast = 0
setwd(master_dir)
source("Video_ccipca.r")

# ---------------------------------------------------
# Loads ccipca data from .RData file, subsets to new files, runs a k-means clustering algorithm on the reduced dimension data set. Clusters together frames which are close together in this space. k is currently set at k = 40. Copies all jpgs into (clust_dir)/new, and for each cluster group i, copies jpgs in group i to folder clust/new/i.

k = 40
setwd(master_dir)
source("Video_cluster.r")
graphics.off()

# ----------------------------------------
# Dialogue box
# Need to manually validate new frames in terms of their ImageClarity (Clear/Cloudy)
# Instructions are in Initial_instructions.txt

# - Copy jpgs designated as Clear to (clust_dir)/cur/Clear
# - Copy jpgs that have been validated as either Clear/Cloudy into (clust_dir)/cur/ImageClarity_validated
# - Delete all files from (clust_dir)/new

if(inital_run == TRUE) {
  require(tcltk)
  button <- tkmessageBox(title = 'Message',
    message = 
    'Validate clear frames
    Initial run only
      Check each jpg in (clust_dir)/new, and decide if image is Clear/Cloudy. The cluster groups can help with this, since you may only need to check each cluster group rather every single file. Clear images tend to cluster together.

      -Copy jpgs designated as Clear to (clust_dir)/cur/Clear
      -Copy jpgs that have been validated as either Clear/Cloudy into (clust_dir)/cur/ImageClarity_validated
      -Delete all files from (clust_dir)/new'
    ,
    type='ok')
  button <- tclvalue(button)
    if(button == 'ok'){
  }
}

# ----------------------------------------
# Run: Video_training_data.r, Video_training.r
# Creates data set with rows as images, columns as:
  # Principal components (normalized so var = 1) for all images : Princ1, Princ2,....,
  # Area data (normalized so var = 1) for Clear images (NA otherwise): Areas1, Areas2,....,
  # ImageClarity for validated images (NA otherwise) 
  # BioticMaterial for Clear, validated images (NA otherwise)
  # Species information and other analysis from manual video analysis 
# Subsets to images that have Clear/Cloudy validated, and cross validates classifier.
# Prints and saves results.

setwd(master_dir)

N = 10
Outputs <- c("ImageClarity")

source("Video_training_data.r")
source("Video_training.r")

# ----------------------------------------
# Run: Video_ImageClarity.r
# Reads in data_train and checks if there are any video files with ImageClarity = NA
# If ImageClarity = NA, video file has not been classfied, stores these files as newdata
# Loads each classifier and runs it on newdata
# Copies classifications for each video in newdata to (clust_dir)/new/Clear_(classifier_name)

setwd(master_dir)
source("Video_ImageClarity.r")

# ----------------------------------------
# if classifier does exist, make adjustment

if(inital_run == FALSE) {
  require(tcltk)
  button <- tkmessageBox(title = 'Message',
    message = 'Validate clear frames
    Adding new data
      Check each jpg in clust/new/Clear_rndf, clust/new/Clear_arnn, and decide if image is Clear/Cloudy. Check all other images to see if the classifier missed any.

      -Copy jpgs designated as Clear to clust/cur/Clear
      -Copy jpgs that have been validated as either Clear/Cloudy into clust/cur/ImageClarity_validated
      -Delete all files from clust/new'
      ,
    type='ok')
  button <- tclvalue(button)
  if(button == 'ok'){

    setwd(master_dir)

    N = 10
    Outputs <- c("ImageClarity")
    source("Video_training_data.r")
    source("Video_training.r")

  }
}

# ----------------------------------------
# run feature detection

setwd(master_dir)
source("Video_features.r")

# ----------------------------------------

# if no classifier exists, copy clear frames by hand
# if classifier does exist, make adjustment

if(inital_run == TRUE) {
  require(tcltk)
  button <- tkmessageBox(title = 'Message',
    message = 
    'Validate biotic frames
    Initial run only
      Within clear jpgs in (clust_dir)/Clear, decide if image has Presence/Absence of Biotic material. The folder (clust_dir)/Clear/Features can help with this since highlights blobs of distinct colour which can indicate the presence of Bioitic material.

      -Copy jpgs designated as Presence to (clust_dir)/cur/Clear/Presence
      -Copy jpgs that have been validated as either Presence/Absence into (clust_dir)/cur/Clear/IfBiotic_validated'
,
    type='ok')
  button <- tclvalue(button)
  if(button == 'ok'){
  }
}

# ----------------------------------------

# resave classifier with new data 
# or train video data

setwd(master_dir)

N = 10
Outputs <- c("BioticMaterial")

source("Video_training_data.r")
source("Video_training.r")

# ----------------------------------------
# if BioticMaterial rndf exists, run on new data

setwd(master_dir)
source("Video_BioticMaterial.r")

# ----------------------------------------

# if no classifier exists, copy clear frames by hand
# if classifier does exist, make adjustment

if(inital_run == FALSE) {
  require(tcltk)
  button <- tkmessageBox(title = 'Message',
    message = 
    'Validate biotic frames
    Adding new frames
      Check each jpg in clust/new/Biotic_rndf, clust/cur/Clear_arnn, and decide if image has Presence/Absence of Biotic material. Check all other Clear images in clust/new/Clear to see if the classifier missed any.

      -Copy jpgs designated as presence to clust/cur/Clear/Presence
      -Copy jpgs that have been validated as either Presence/Absence into clust/cur/Clear/BioticMaterial_validated
      -Delete all files from clust/new'
    ,
    type='ok')
  button <- tclvalue(button)
    if(button == 'ok'){

      setwd(master_dir)
      N = 10
      Outputs <- c("BioticMaterial")
      source("Video_training_data.r")
      source("Video_training.r")
      
  }
}


# -----------------------------------------

# setwd(master_dir)

# N = 10
# Outputs <- c("Species_")
# # Outputs <- c("Species_SAB")
# # Outputs <- c("Species_5AA")
# # Outputs <- c("Species_3S1")
# # Outputs <- c("Species_VSA")

# source("Video_training_data.r")
# source("Video_training.r")

# ------------------------------------------

# proje_dim = 790
proje_dim = as.integer(read.table("n_components.txt"))
setwd(master_dir)
source("Video_projection.r")

# # ----------------------------------------
# # Output Eigentraps

# n_pc = 790
n_pc = as.integer(read.table("n_components.txt"))
setwd(master_dir)
source("Video_eigenvectors.r")
