# Runs all the scripts in order in the R folder of the project 

source('~/NAPS_project/NAPS_wrangling/R/0-setup_project.R') 

all_scripts <- grep("run_all_scripts|0|recreate|2c", list.files(wd$R), invert = TRUE, value = TRUE)

# Ask user if files need to be downloaded (would be smarter to check data folder)
response <- readline(prompt="Do you want to downlaod the NAPS data (~ 1 hr)? (y/n) ")

if(response == "yes" | response == "y") {
  break
} else if (response == "no" | response == "n") {
  all_scripts <- grep("1a", all_scripts, invert = TRUE, value = TRUE)
} else {
  while (response %in% c("yes", "y", "no", "n")) {
    response <-
      readline(prompt = "Invalid response. Do you want to downlaod the NAPS data (~ 2 hr)? (y/n) ")
  }
  if (response == "yes" | response == "y") {
    break
  } else if (response == "no" | response == "n") {
    all_scripts <- grep("1a", all_scripts, invert = TRUE, value = TRUE)
  }
}




lapply(all_scripts, function(x) source(paste0("~/NAPS_project/NAPS_wrangling/R/", x)))