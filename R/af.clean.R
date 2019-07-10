#' Cleans the Afrobarometer dataset.
#'
#' Normally called by default by \code{af.load()}
#'
#' @name af.clean
#' @return A clean version of the global dataframe \code{afro}
#' @examples
#' af.clean() # Cleans the global version of Loads the 6th round of the Afrobarometer from local file if available, from the websiteif not
#' af.load(5, T) # Downloads the 5th round of the Afrobarometer, ignoring local files

af.clean <- function(afro){
  # Lower case all of the functions
  afro <- afro %>% rename_all(tolower)

  # Replace all of the missing labels with NA
#  afro <- afro %>% select(-dateintr, -strtime) %>%
#   mutate_all(~na_if(., -1))

  afro <- haven::as_factor(afro)

  # Return dataframe
  afro
}
