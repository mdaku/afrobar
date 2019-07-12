#' Cleans the Afrobarometer dataset
#'
#' Returns a clean version of the Afrobarometer dataset. Normally called by default by \code{af.load()}
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

  # Replace all of the missing labels with NA
#  afro <- afro %>% select(-dateintr, -strtime) %>%
#   mutate_all(~na_if(., -1))

  afro <- haven::as_factor(afro)

  # Return dataframe
  afro
}
