#' One and two-way tables for categorical questions
#'
#' @name af.load
#' @param question A string specifying the dependent varirable to examine
#' @param question2 A string specifying the independent variable to examine
#' @param digits An integer specifying number of digits to round to (default = 2)
#' @param
#' @return A table object of the
#' @examples
#' af.tab(afro, "q5") # For question 5: Produces a table of percentages, a table of raw numbers, and a barplot of responses, dropping missing/don't know/refused responses
#' af.tab(afro, "q5", T) # For question 5: Produces a table of percentages, a table of raw numbers, and a barplot of responses, includes missing/don't know/refused responses
#' af.tab(afro, "q5", "q7") # For questions 5 & 7: Produces a cross-tab table of proportions, a table of raw numbers, and a side-by-sie barplot of responses, dropping missing/don't know/refused responses
#' af.tab(afro, "q5", "q7", pos="stack") # For questions 5 & 7: Produces a cross-tab table of proportions, a table of raw numbers, and a stacked barplot of responses, dropping missing/don't know/refused responses


# Descriptive Table & Graphs
library(ggplot2)
library(labelled)
af.tab <- function(afro, question, question2=NULL, digits=2, miss=F, pos="dodge"){
  # Need to check if question actually exists
  #v <- afro[question,]
  # We are missing the actual question text from the codebook

  if (is.null(question2)){
    #eval(parse(text = "x"))
    if(miss){
      a <- afro
    }
    else{
      a <- afro %>% filter(eval(parse(text=question)) != "Missing") %>%
      filter(eval(parse(text=question)) != "Don't know") %>%
        filter(eval(parse(text=question)) != "Refused")
    }
    g <- a %>%
      ggplot(aes(x=eval(parse(text=question)))) +
      theme_minimal() +
      xlab("")+
      ylab("Count") +
      ggtitle(label=var_label(afro[question][1])[[1]], subtitle = paste("Source: Afrobarometer, Round", af.round)) +
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

  else{

    if(miss){
      a <- afro
    }
    else{
      a <- afro %>% filter(eval(parse(text=question)) != "Missing") %>%
        filter(eval(parse(text=question)) != "Don't know") %>%
        filter(eval(parse(text=question)) != "Don’t know") %>%
        filter(eval(parse(text=question)) != "Refused") %>%
        filter(eval(parse(text=question2)) != "Missing") %>%
        filter(eval(parse(text=question2)) != "Don't know") %>%
        filter(eval(parse(text=question2)) != "Don’t know") %>%
        filter(eval(parse(text=question2)) != "Refused")
    }


    title <- paste(var_label(afro[question][1])[[1]],var_label(afro[question2][1])[[1]], sep=" by \n")
    g <- a %>%
      #ggplot(aes(x=eval(parse(text=question)), y=eval(parse(text=question2)))) +
      ggplot(aes(x=eval(parse(text=question)))) +
      theme_minimal() +
      xlab("")+
      ylab("Count") +

      ggtitle(label=title, subtitle = paste("Source: Afrobarometer, Round", af.round)) +
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

  return(tab)

}
