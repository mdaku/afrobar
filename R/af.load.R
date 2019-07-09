# afrobarameteR.R
# Author: Mark Daku
# Date Created: July 7, 2019

# Load libraries
library(tidyverse)
library(haven)

# Functions
af.load <- function(round=6){
  # This may be complicated, as the connection is usually weak
  if (round == 6){
    # 2016
    url <- "http://afrobarometer.org/sites/default/files/data/round-6/merged_r6_data_2016_36countries2.sav"
    af.round <<- 6
  }
  else if (round == 5){
    url <- "http://afrobarometer.org/sites/default/files/data/round-5/merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav"
    warning("Afrobarometer round 5 is not the most recent round of data")
    af.round <<- 5
  }
  else if (round ==4){
    url <- "http://afrobarometer.org/sites/default/files/data/round-4/merged_r4_data.sav"
    warning("Afrobarometer round 4 is not the most recent round of data")
    af.round <<- 4
  }
  else if (round ==3){
    url <- "http://afrobarometer.org/sites/default/files/data/round-3/merged_r3_data.sav"
    warning("Afrobarometer round 3 is not the most recent round of data")
    af.round <<- 3
  }
  else if (round ==2){
    url <- "http://afrobarometer.org/sites/default/files/data/round-2/merged_r2_data.sav"
    warning("Afrobarometer round 2 is not the most recent round of data")
    af.round <<- 2
  }
  else if (round ==1){
    url <- "http://afrobarometer.org/sites/default/files/data/round-1/merged_r1_data.sav"
    warning("Afrobarometer round 1 is not the most recent round of data")
    af.round <<- 1
  }
  else{
    stop("Only Afrobarometer rounds 1-6 are currently available")
  }

  # Download the file & load it as an SPSS file
  temp <- tempfile()
  download.file(url, temp)
  afro <<- read_sav(temp)
  afro <<- afro.clean(afro.src)
}
