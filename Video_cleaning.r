setwd(master_dir)

system("rm data_video.RData")
system("rm data_areas.RData")
system("rm data_train.RData")
system("rm data_analy.RData")

system("rm n_components.txt")  
system("rm iteration.txt")  
system("rm amnesic.txt") 
system("rm copy.txt")   
system("rm mean_.txt")
system("rm components_.txt")
system("rm video_files_cur.txt")
system("rm RepFrames_cur.txt")
system("rm prx.txt")

# ---------------------------------------

system(paste("rm -r ", clust_dir, sep = ""))
system(paste("mkdir ", clust_dir, sep = ""))
system(paste("mkdir ", clust_dir, "/new", sep = ""))
system(paste("mkdir ", clust_dir, "/cur", sep = ""))
system(paste("mkdir ", clust_dir, "/cur/ImageClarity_validated", sep = ""))
system(paste("mkdir ", clust_dir, "/cur/Clear", sep = ""))
system(paste("mkdir ", clust_dir, "/cur/Cloudy", sep = ""))
system(paste("mkdir ", clust_dir, "/cur/Clear/Presence", sep = ""))
system(paste("mkdir ", clust_dir, "/cur/Clear/Absence", sep = ""))
system(paste("mkdir ", clust_dir, "/cur/Clear/Features", sep = ""))
system(paste("mkdir ", clust_dir, "/cur/Clear/BioticMaterial_validated", sep = ""))

# ----------------------------------------

