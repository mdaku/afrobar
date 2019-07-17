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

  if (af_round == 1){
    # Replace 99 with NA

    afro <- afro %>% mutate_all(~ replace(., . == 99 | . == "99", NA))

  }  # This data seems to be pretty clean already
    # Return dataframe
  afro
}
