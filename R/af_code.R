#' Prints out simple Afrobarometer codebook
#'
#' @name af_code
#' @param afro Afrobarometer datafram
#' @param question Optional: Specify the question whose description you require
#' @return Returns a tibble that contains the variable name and the description of each variable
#' @examples
#'
#' # Prints out the codebook for all variables in the current round
#' # or from the website if the local file does not exist
#' af_code()
#'
#' # Prints out the codebook for all variables that match a particular string in the current round
#' # or from the website if the local file does not exist
#' # The following will return q1, q10, q111, etc. and their corresponding variable descriptions
#' af_code("q1")
#'
#' @export
#'
#'


library(stringr)


af_code <- function(afro, q=NULL){

  # Build the tibble no matter what
  for (i in 1:length(afro)){

    question.text <- as.character(var_label(afro[i][1])[[1]])
    question <- as.character(colnames(afro)[i])
    if (i == 1){
      code.tbl <- data.frame(`question` = as.character(question), `question.text` = as.character(question.text))
    }
    else {
      code.tbl <- add_row(code.tbl, question = as.character(question), question.text=as.character(question.text))
    }
    code.tbl$question <- as.character(code.tbl$question)
    code.tbl$question.text <- as.character(code.tbl$question.text)
  }
  if (is.null(q)){
    return(as_tibble(code.tbl))
  }
  else{
#    if (af_contains(afro, question)){
#      question.text <- as.character(var_label(afro[i][1])[[1]])
#      question <- as.character(colnames(afro)[i])
#      ret <- data.frame(`question` = as.character(question), `question.text` = as.character(question.text))
#    }
#    else{
      ret <- code.tbl %>% filter(str_detect(question, q))
#    }
    return(as_tibble(ret))
  }
  #  if (af_contains(afro, question)){
#
#      question.text <- as.character(var_label(afro[i][1])[[1]])
#      question <- as.character(colnames(afro)[i])
#      code.tbl <- data.frame(`question` = as.character(question), `question.text` = as.character(question.text))
#      #question_text <-var_label(afro[question][1])[[1]]
#      #l <- str_length(colnames(afro)[colnames(afro) == question])
#      #resid <- (22-l)
#      #if (resid < 0) resid =0
#      #cat(blue(colnames(afro)[colnames(afro) == question]))
#      #for (j in 1:resid){
#      #  cat(" ")
#      #}
#      #cat(question_text)
#      #cat("\n")
#    }
#    else{#


      # message(paste(question, "is not a valid question in Afrobarometer Round", af_round), sep=" ")
#    }
  }

