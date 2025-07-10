# Pull in data files
# The data files are too large to push to GitHub. Instead we used Google Drive
# to store the files: https://drive.google.com/drive/u/1/folders/1CvVC_y_Vwpv0IB66GwucYaODoEnMxqCf
# The user could also generate the data files by running the scripts
# 1. pull_raw_data_from_database.R
# 2. process_updated_markrecap.R
# 3. clean_data.R

library(googledrive)

drive_find(n_max = 30) # check your googledrive connection

drive_download("release",
                path = "data/release.csv", 
                overwrite = T)
drive_download("recapture",
               path = "data/recapture.csv", 
               overwrite = T)
drive_download("trap",
               path = "data/trap.csv", 
               overwrite = T)
drive_download("catch",
               path = "data/catch.csv.zip", 
               overwrite = T)
