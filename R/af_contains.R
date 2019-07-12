#' Returns whether or not a question, or vector of questions, exists in the dataset
#'
#'
#' @name af_contains
#' @param afro A dataframe containing Afrobarometer data
#' @param question A vector of questions
#' @return A single boolean that is TRUE only if all questions exist in the dataset
#' @examples
#'
#' # Check to see if question 5 exists
#' afro %>% af_contains("q5")
#'
#' #' # Check to see if questions 5 and 30 exist
#' afro %>% af_contains(c("q5", "q30"))

#' @export

af_contains <- function(afro, question){
  cont <- question %in% colnames(afro)
  return(as.logical(prod(cont)))
}
