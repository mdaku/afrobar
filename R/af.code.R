#' Prints out simple Afrobarometer codebook
#'
#' @name af.code
#' @param afro Afrobarometer datafram
#' @param question Optional: Specify the question whose description you require
#' @return Returns the descriptions of each variable
#' @examples
#'
#' # Prints out the codebook for all variables in the current round
#' # or from the website if the local file does not exist
#' af.code()
#'
#' @export
#'
#'

library(crayon)
library(stringr)


af.code <- function(afro, question=NULL){

if (is.null(question)){
  for (i in 1:length(afro)){

      question.text <-var_label(afro[i][1])[[1]]
      l <- str_length(colnames(afro)[i])
      resid <- (22-l)
      if (resid < 0) resid =0
      cat(blue(colnames(afro)[i]))

      for (j in 1:resid){
        cat(" ")
      }

      # The following adds info on variable type, but it's useless at the moment
      #a <- class(afro[[i]])
      #if(a == "character"){
      #  cat(yellow(" [chr] "))
      #}
      #else if (a == "factor"){
      #  cat(yellow(" [fct] "))
      #}
      cat(question.text)
      cat("\n")
    }
  }
  else{
    if (question %in% colnames(afro)){
      question.text <-var_label(afro[question][1])[[1]]
      l <- str_length(colnames(afro)[colnames(afro) == question])
      resid <- (22-l)
      if (resid < 0) resid =0
      cat(blue(colnames(afro)[colnames(afro) == question]))
      for (j in 1:resid){
        cat(" ")
      }
      cat(question.text)
      cat("\n")
    }
    else{
      message(paste(question, "is not a valid question in Afrobarometer Round", af.round), sep=" ")
    }
  }
}
