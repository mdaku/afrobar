#' Returns whether or not a question, or vector of questions, exists in the dataset
#'
#' Returns a clean version of the Afrobarometer dataset. Normally called by default by \code{af.load()}
#'
#' @name af.contains
#' @param afro A dataframe containing Afrobarometer data
#' @param question A vector of questions
#' @return A single boolean that is TRUE only if all questions exist in the dataset
#' @examples
#'
#' # Check to see if question 5 exists
#' afro %>% af.contains("q5")
#'
#' #' # Check to see if questions 5 and 30 exist
#' afro %>% af.contains(c("q5", "q30"))

#' @export

af.contains <- function(afro, question){
  cont <- question %in% colnames(afro)
  return(as.logical(prod(cont)))
}
