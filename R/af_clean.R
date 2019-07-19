#' Cleans the Afrobarometer dataset
#'
#' Returns a clean version of the Afrobarometer dataset. Normally called by default by \code{af.load()}
#' Important: For all rounds of the Afrobarometer, this function recodes missing, refused, etc.
#' data as NA.  If you would prefer to keep the data unmodified, you must run af_load(clean=FALSE)
#'
#' @name af_clean
#' @param afro A dataframe containing Afrobarometer data
#' @return A clean version of the passed in dataframe \code{afro}
#' @examples
#'
#' # Returns a cleaned version of the afro dataframe loaded
#' # using \code{af.load}
#' afro %>% af.clean()
#' @export

af_clean <- function(afro){
  # Lower case all of the functions
  afro <- afro %>% rename_all(tolower)

  if (af_round == 1){
    # Round 1
    # 9 = Don't Know - Leave in
    # 97 = Not applicable
    # 98 = Refused
    # 99 = Missing Data

    afro <- afro %>%
      mutate(dateintr = as.character(dateintr)) %>%
      mutate_all(~ replace(., . == "97" | . == "98" | . == "99" | . > "995", NA)) %>%
      mutate_if(., is.labelled, as_factor)
  }
  else if (af_round ==2 | af_round == 3 | af_round ==4 | af_round ==5 | af_round ==6){
    # Round 2-6
    # -1 = Missing Data
    # We HAVE to leave the Don't Knows alone
    # 995 Other
    # 996 No Further Answer
    # 997 Not Applicable
    # 998 Refused to Answer
    # 999 Don't Know / 9999
    afro <- afro %>%
      mutate(dateintr = as.character(dateintr)) %>%
      mutate_all(~ replace(., . == "-1" | . == "995" | . == "996" | . == "997" | . == "998" | . == "98" | . > "9995" | . == "98", NA)) %>%
      mutate_if(., is.labelled, as_factor)
    # %>%  mutate_if(., is.labelled, as_factor)
  }  # This data seems to be pretty clean already
    # Return dataframe
  View(afro)
  afro
}
