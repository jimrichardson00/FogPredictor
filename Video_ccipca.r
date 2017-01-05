# --------------------------------------------
# Output:
# - Loads principal component analysis (ccipca) if it exists, otherwise sets initial values. 
# - Checks frame_dir, and (clust_dir)/ImageClarity_validated.
# - Subsets to .jpg files that are in frame_dir, but not in (clust_dir)/Image_clarity_validated.
# - For these files:
	# Reads .jpg file into Python
  # Removes trap by selecting upper middle square of image. This is set in Video_master.r by the parameters x1, x2, y1, y2.
  # Resizes the image in the upper middle square to W x H. W, H are set in Video_master.r
  # Copies the data from the resulting image into a matrix.
# - If principal component analysis (ccipca) already exists from previous data:
    # Adds new data to current ccipca
# - If no ccipca exists:
    # Runs a ccipca on this matrix.
  # Writes ccipca info (mean, components, iteration, copy, amnesic) to .txt files. 
# - Reads in .txt files to R and saves all info (including data matrix, mean, components, and projection of data to components) to .RData file
# --------------------------------------------

# --------------------------------------------
# - Loads principal component analysis (ccipca) if it exists, otherwise sets initial values. 
setwd(master_dir)
if(file.exists(paste("video_files_cur", year, ".txt", sep = "")) == TRUE & rerun == 0) {
  video_files_cur <- as.vector(read.table(paste("video_files_cur", year, ".txt", sep = ""))[, 1])
} else {
  video_files_cur <- vector()
}

# --------------------------------------------
# - Checks frame_dir, and (clust_dir)/ImageClarity_validated.
# - Subsets to .jpg files that are in frame_dir, but not in (clust_dir)/Image_clarity_validated.

# list of videos in video folder
require(stringr)
video_files <- str_match(sort(list.files(frame_dir)), "(.+)\\.jpg")[, 2]
video_files

# defines new videos to be added
video_files_new <- video_files[!(video_files %in% video_files_cur)]
video_files_new

# cacluates the new rep frames matrix
if(length(video_files_new) > 0) {

  setwd(frame_dir)
  require(parallel)

  print(paste("Adding: ", length(video_files_new), " new files", sep = ""))

	# Reads .jpg file into Python
  # Removes trap by selecting upper middle square of image. This is set in Video_master.r by the parameters x1, x2, y1, y2.
  # Resizes the image in the upper middle square to W x H. W, H are set in Video_master.r
  # Copies the data from the resulting image into a matrix RepFrames_new
  if(mac == TRUE) {
    RepFrames_new <- lapply(video_files_new, 
      FUN = function(video_file) Extract_frame(video_file = video_file, 
        W = W, H = H, frame_dir = frame_dir, skip = skip)
      )
  } else {
    RepFrames_new <- mclapply(video_files_new, 
      FUN = function(video_file) Extract_frame(video_file = video_file, 
        W = W, H = H, frame_dir = frame_dir, skip = skip)
      , mc.cores = n_cores
      , mc.silent = FALSE
      , mc.preschedule = TRUE
      )
  }
  RepFrames_new <- unlist(RepFrames_new)
  RepFrames_new <- matrix(RepFrames_new, ncol = 3*W*H, byrow = TRUE)

  # setwd(master_dir)
  # save(RepFrames_new, file = paste("RepFrames_new", year, ".RData", sep = ""))
  # print(doesnt_exist)
  # setwd(master_dir)
  # load(paste("RepFrames_new", year, ".RData", sep = ""))
  
  # sets n_components as total number of video_files
  n_components <- length(video_files_new) + length(video_files_cur)
  n_components

	# - If principal component analysis (ccipca) already exists from previous data:
	#     Adds new data to current ccipca
	# - If no ccipca exists:
	#     Runs a ccipca on this matrix.
	#   Writes ccipca info (mean, components, iteration, copy, amnesic) to .txt files. 
  print(paste("Starting: CCIPCA_RepFrames;", 
    " new ", length(video_files_new), 
    " cur ", length(video_files_cur), sep = ""))
  if(mac == TRUE) {
    ccipca_dict <- CCIPCA_RepFrames(RepFrames_new = RepFrames_new, n_components = n_components, rerun = rerun, year = year)
  } else {
    # ccipca_dict <- CCIPCA_RepFrames(RepFrames_new = RepFrames_new, n_components = n_components, rerun = rerun, year = year)
  }
  print(paste("Finished: CCIPCA_RepFrames;", 
    " new ", length(video_files_new), 
    " cur ", length(video_files_cur), sep = ""))

  write(c(video_files_cur, video_files_new), file = paste("video_files_cur", year, ".txt", sep = ""))

}
