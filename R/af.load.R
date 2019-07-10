#' Loads Afrobarometer dataset.
#'
#' @param round A number specifying which round of the Afrobarometer to load (6 by default).
#' @param force A boolean specifiying whether or not to force the download of a new copy from the website (false by default)
#' @return The global dataframe \code{afro},and a global integer \code{af.round}.
#' @examples
#' af.load() # Loads the 6th round of the Afrobarometer from local file if available, from the websiteif not
#' af.load(5, T) # Downloads the 5th round of the Afrobarometer, ignoring local files


# afrobarameteR.R
# Author: Mark Daku
# Date Created: July 7, 2019

# Load libraries
library(tidyverse)
library(haven)

# Functions
af.load <- function(round=6, force=FALSE){
  # Specifies the round and whether or not to force the download
  # This may be complicated, as the connection is usually weak
  if (round == 6){
    # 2016
    url <- "http://afrobarometer.org/sites/default/files/data/round-6/merged_r6_data_2016_36countries2.sav"
    fname <- "merged_r6_data_2016_36countries2.sav"
    af.round <<- 6
  }
  else if (round == 5){
    url <- "http://afrobarometer.org/sites/default/files/data/round-5/merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav"
    fname <- "merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav"
    warning("Afrobarometer round 5 is not the most recent round of data")
    af.round <<- 5
  }
  else if (round ==4){
    url <- "http://afrobarometer.org/sites/default/files/data/round-4/merged_r4_data.sav"
    fname <- "round-4/merged_r4_data.sav"
    warning("Afrobarometer round 4 is not the most recent round of data")
    af.round <<- 4
  }
  else if (round ==3){
    url <- "http://afrobarometer.org/sites/default/files/data/round-3/merged_r3_data.sav"
    fname <- "merged_r3_data.sav"
    warning("Afrobarometer round 3 is not the most recent round of data")
    af.round <<- 3
  }
  else if (round ==2){
    url <- "http://afrobarometer.org/sites/default/files/data/round-2/merged_r2_data.sav"
    fname <- "merged_r2_data.sav"
    warning("Afrobarometer round 2 is not the most recent round of data")
    af.round <<- 2
  }
  else if (round ==1){
    url <- "http://afrobarometer.org/sites/default/files/data/round-1/merged_r1_data.sav"
    fname <- "merged_r1_data.sav"
    warning("Afrobarometer round 1 is not the most recent round of data")
    af.round <<- 1
  }
  else{
    stop("Only Afrobarometer rounds 1-6 are currently available")
  }

  # Download the file & load it as an SPSS file

  # First, check to see if the file exists locally
  if (file.exists(fname) & force==FALSE){
    # Does the file exist?
    message("Loading local file")
    afro <<- read_sav(fname)
  }
  else{
    # Downloading file from Afrobarometer.org
    message("Downloading file from Afrobarometer.org")
    download.file(url, fname)
    afro <<- read_sav(fname)

  }
  message("Cleaning source file")
  afro <<- af.clean(afro)
}
