# Descriptive Table & Graphs
library(ggplot2)
library(labelled)
af.tab <- function(question, question2=NULL, digits=2, miss=T, pos="dodge"){
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
      print(round(prop.table(table(a[question])),digits))

      cat("\n")
      cat("Raw numbers\n")
      summary(a[[question]])
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
    print(round(prop.table(table(a[[question]], a[[question2]])),digits))
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
    }


}
