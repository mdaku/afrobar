#' Removes missing, don't know, refused, from afrobarometer data
#'
#' @name af_rem_miss
#' @param afro An afrobarometer dataframe
#' @param question The question that you want to clean
#' @return A table object of the proportion table or cross-tab
#' @examples
#' # Remove all missing responses from a question
#' afro %>% af_rem_miss("q5")
#'
af_rem_miss <-function(afro, question, silent=FALSE){
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

#af.rem.miss.all <- function(afro){
#  # There is a vectorized way to do this using apply, but my brain is fuzzy
#  #afro <- sapply(afro, FUN=af.rem.miss, afro=afro)
#  message("Dropping missing values from ALL rows.")
#  message("This will result in a much smaller dataset, and is generally not recommended.")
#  for (col in colnames(afro)){
#    try(afro <- af.rem.miss(afro, col, silent=TRUE))
#    cat(blue("."))
#  }
#}
