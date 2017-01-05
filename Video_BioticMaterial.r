# ----------------------------------------
# Output:
# - Loads data_train, creates newdata as video files for which BioticMaterial = NA
# - If there are files that have not been validated (BioticMaterial = NA), proceed. If not, do nothing.
# - Load each classifier, run classifier on newdata with min cutoff set by P_var, P_val
# - Create lists of Presence, Absence videos from output
# - Creates the following folders:
#    (clust_dir)/new/Clear/Presence_(classifier_name)_P_val(P_val)
#    (clust_dir)/new/Clear/Absence_(classifier_name)_P_val(P_val)
#   (deletes and recreates them)
# - Copies each new video frame to following folders: 
#    (clust_dir)/new/Clear/Presence_(classifier_name)_P_val(P_val)
#    (clust_dir)/new/Clear/Absence_(classifier_name)_P_val(P_val)
# according its classification
# ----------------------------------------

# ----------------------------------------
# - Loads data_train, creates newdata as video files for which BioticMaterial = NA
load(file = paste("data_train", year, ".RData", sep = ""))

newdata <- data_train[is.na(data_train[, "ImageClarity"]) == FALSE 
  & data_train[, "ImageClarity"] == "Clear" 
  & is.na(data_train[, "BioticMaterial"]) == TRUE
  , ]

print(paste("New frames to run BioticMaterial classifier on: ", nrow(newdata), sep = ""))

# ----------------------------------------

# ----------------------------------------
# - If there are files that have not been validated (BioticMaterial = NA), proceed. If not, do nothing.
if(nrow(newdata) > 0) {

  # deletes classification folder, then creates it (aviods overlap)
  system(paste("rm -r ", clust_dir, "/new/Clear/", sep = ""))
  system(paste("mkdir ", clust_dir, "/new/Clear/", sep = ""))

  # cycle through classifiers
  for(classifier_name in c("rndf", "arnn", "nbay")) {

		# ----------------------------------------
		# - Load each classifier, run classifier on newdata with min cutoff set by P_var, P_val
    load(paste(classifier_name, "_video", year, "BioticMaterial.RData", sep = ""))
    # assign("classifier", as.formula(classifier_name))
    classifier <- get(classifier_name)

    P_val = P[P$P_Outputs == "BioticMaterial", "P_val"]
    P_var = P[P$P_Outputs == "BioticMaterial", "P_var"]

    Pred_class_output <- Pred_class(classifier_name = classifier_name,
      classifier = classifier,
      newdata = newdata, 
      Outputs = Outputs,
      P_var = P_var,
      P_val = P_val)
    Pred_class_output <- as.vector(Pred_class_output)

    # ----------------------------------------
    # - Create lists of Presence, Absence videos from output
    Presence_classifier <- newdata$Filenames[Pred_class_output == "BioticMaterialPresence"]
    Absence_classifier <- newdata$Filenames[Pred_class_output == "BioticMaterialAbsence"]

    # ----------------------------------------
    # - Creates the following folders:
    #    (clust_dir)/new/Presence_(classifier_name)_P_val(P_val)
    #    (clust_dir)/new/Absence_(classifier_name)_P_val(P_val)
    #   (deletes and recreates them)

    # deletes classification folder, then creates it (aviods overlap)
    system(paste("rm -r ", clust_dir, "/new/Clear/Presence_", classifier_name, "_P_val", round(P_val, 2), sep = ""))
    system(paste("mkdir ", clust_dir, "/new/Clear/Presence_", classifier_name, "_P_val", round(P_val, 2), sep = ""))

    # deletes classification folder, then creates it (aviods overlap)
    system(paste("rm -r ", clust_dir, "/new/Clear/Absence_", classifier_name, "_P_val", round(P_val, 2), sep = ""))
    system(paste("mkdir ", clust_dir, "/new/Clear/Absence_", classifier_name, "_P_val", round(P_val, 2), sep = ""))

    # ----------------------------------------
    # - Copies each new video frame to following folders: 
    #    (clust_dir)/new/Presence_(classifier_name)_P_val(P_val)
    #    (clust_dir)/new/Absence_(classifier_name)_P_val(P_val)
    # according its classification

    jpg <- Presence_classifier[1]
    for(jpg in newdata$Filenames) {
      if(jpg %in% Presence_classifier) {
        from_dir = paste(clust_dir, "/cur/Clear", sep = "")
        dest_dir = paste(clust_dir, "/new/Clear/Presence_", classifier_name, "_P_val", round(P_val, 2), sep = "")
        system(paste("cp ", from_dir, "/", jpg, ".jpg", " ", dest_dir, sep = ""))
      } else if(jpg %in% Absence_classifier) {
        from_dir = paste(clust_dir, "/cur/Clear", sep = "")
        dest_dir = paste(clust_dir, "/new/Clear/Absence_", classifier_name, "_P_val", round(P_val, 2), sep = "")
        system(paste("cp ", from_dir, "/", jpg, ".jpg", " ", dest_dir, sep = ""))
      }
    }
  }
}
