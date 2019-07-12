#' Removes missing, don't know, refused, from afrobarometer data
#'
#' @name af.rem.miss
#' @param afro An afrobarometer dataframe
#' @param question The question that you want to clean
#' @return A table object of the proportion table or cross-tab
#' @examples
#' # Remove all missing responses from a question
#' afro %>% af.tab("q5")
#'
af.rem.miss <-function(afro, question, silent=FALSE){
# Incorporates all the missing data tags from each round
  if (!silent) message("Dropping missing values")
  afro <- afro %>% filter(eval(parse(text=question)) != "Missing") %>%
    filter(eval(parse(text=question)) != "Missing data") %>%
    filter(eval(parse(text=question)) != "Don't know") %>%
    filter(eval(parse(text=question)) != "Don’t know") %>%
    filter(eval(parse(text=question)) != "Refused") %>%
    filter(eval(parse(text=question)) != "Refused to answer")

    afro
}
