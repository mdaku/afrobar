#' One and two-way tables for categorical questions
#'
#' Easily produced one and two-way tables for categorical questions accompanied by a barplot
#'
#' If a cross-tab, also performs a chi-square test
#'
#' @name af_tab
#' @param afro An afrobarometer dataframe
#' @param question A string specifying the dependent varirable to examine
#' @param question2 A string specifying the independent variable to examine
#' @param miss A boolean specifying whether or not to include missing/don't know/refused responses (default FALSE)
#' @param digits An integer specifying number of digits to round to (default is 2)
#' @param pos For two variables, specifies the style of the barplot: "stack" or "dodge" (default)
#'
#' @return A table object of the proportion table or cross-tab
#' @examples
#' # For question 5: Produce a table of percentages, a table of raw numbers,
#' # and a barplot of responses, dropping missing/don't know/refused responses
#' afro %>% af_tab("q5")
#'
#' # For question 5: Produces a table of percentages, a table of raw numbers,
#' # and a barplot of responses, including missing/don't know/refused responses
#' # afro %>% af_tab("q5", miss=T)
#'
#' # For questions 5 & 7: Produces a cross-tab table of proportions,
#' # a table of raw numbers, and a side-by-side barplot of responses,
#' # dropping missing/don't know/refused responses
#' afro %>% af_tab("q5", "q7")
#'
#' # For questions 5 & 7: Produces a cross-tab table of proportions,
#' # a table of raw numbers, and a stacked barplot of responses,
#' # dropping missing/don't know/refused responses
#' afro %>% af_tab("q5", "q7", pos="stack")
#' @export


# Descriptive Table & Graphs
library(ggplot2)
library(labelled)
af_tab <- function(afro, question, question2=NULL, digits=2, miss=F, pos="dodge"){
  # Need to check if question actually exists
  #v <- afro[question,]
  # We are missing the actual question text from the codebook

  # If there is no other question & the question exists
  if (is.null(question2) & af_contains(afro, question)){
    #eval(parse(text = "x"))
    if(miss){
      a <- afro
    }
    else{
      a <- afro %>% af_rem_miss(question)

      #
      #a <- afro %>% filter(eval(parse(text=question)) != "Missing") %>%
      #filter(eval(parse(text=question)) != "Don't know") %>%
      #  filter(eval(parse(text=question)) != "Refused")
    }
    g <- a %>%
      ggplot(aes(x=eval(parse(text=question)))) +
      theme_minimal() +
      xlab("")+
      ylab("Count") +
      ggtitle(label=var_label(afro[question][1])[[1]], subtitle = paste("Source: Afrobarometer, Round", af_round)) +
      geom_bar()


      title<-var_label(afro[question][1])[[1]]

      print(g)
      cat(title)
      cat("\n")
      cat("Percentages\n")
      a[[question]] <- factor(a[[question]])
      tab <- (round(prop.table(table(a[question])),digits))
      print(tab)
      cat("\n")
      cat("Raw numbers\n")
      summary(a[[question]])

      print(g)
      print(var_label(afro[question][1])[[1]])
      a[[question]] <- factor(a[[question]])
      round(prop.table(table(a[question])),digits)


  }
  else if (is.null(question2)){
    message(paste(question,"is not a valid question in Afrobarometer Round", af_round, sep=" "))
    tab <- "No results"
  }
  else if (af_contains(afro, c(question, question2))){
    # By construction of statements above, question 2 is NOT null
    if(miss){
      a <- afro
    }
    else{
      a< - afro %>% af_rem_miss(question) %>% af_rem_miss(question2)
    }

    title <- paste(var_label(afro[question][1])[[1]],var_label(afro[question2][1])[[1]], sep=" by \n")
    g <- a %>%
      #ggplot(aes(x=eval(parse(text=question)), y=eval(parse(text=question2)))) +
      ggplot(aes(x=eval(parse(text=question)))) +
      theme_minimal() +
      xlab("")+
      ylab("Count") +

      ggtitle(label=title, subtitle = paste("Source: Afrobarometer, Round", af_round)) +
      labs(fill="") +
      geom_bar(aes(fill=eval(parse(text=question2))), position=pos)

    print(g)

    # Perform some tests here
    chi <- chisq.test(afro[[question]], afro[[question2]])

    a[[question]] <- factor(a[[question]])
    a[[question2]] <- factor(a[[question2]])

    cat(title)
    cat("\n")
    cat("Percentages\n")
    tab <- (round(prop.table(table(a[[question]], a[[question2]])),digits))
    print(tab)
    cat("\n")
    cat(paste("Chi-squared: ", round(chi$statistic, 2), " (p-value: ", round(chi$p.value, 4), ")", sep=""))
    cat("\n")
    cat("\n")
    cat("Raw numbers: ")
    cat(question)
    cat(":\n")
    print(summary(a[[question]]))
    cat("\n")
    cat("Raw numbers: ")
    cat(question2)
    cat(":\n")
    print(summary(a[[question2]]))

    print(tab)
  }
  else{
    # This should cover all cases
    if (!af_contains(afro, question)){
      message(paste(question,"is not a valid question in Afrobarometer Round", af_round, sep=" "))
    }
    if (!af_contains(afro, question2)){
      message(paste(question2,"is not a valid question in Afrobarometer Round", af_round, sep=" "))
    }
    tab <- "No results."


  }
  return(tab)


}
