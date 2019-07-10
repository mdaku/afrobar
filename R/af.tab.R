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

<<<<<<< HEAD
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
=======
      print(g)
      print(var_label(afro[question][1])[[1]])
      a[[question]] <- factor(a[[question]])
      round(prop.table(table(a[question])),digits)

>>>>>>> 5213a1ca1e85bcce507cf629acddab24fe7cf2d3
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

<<<<<<< HEAD
    title <- paste(var_label(afro[question][1])[[1]],var_label(afro[question2][1])[[1]], sep=" by \n")
=======
    t <- paste(var_label(afro[question][1])[[1]],var_label(afro[question2][1])[[1]], sep=" by \n")
>>>>>>> 5213a1ca1e85bcce507cf629acddab24fe7cf2d3
    g <- a %>%
      #ggplot(aes(x=eval(parse(text=question)), y=eval(parse(text=question2)))) +
      ggplot(aes(x=eval(parse(text=question)))) +
      theme_minimal() +
      xlab("")+
      ylab("Count") +
<<<<<<< HEAD
      ggtitle(label=title, subtitle = paste("Source: Afrobarometer, Round", af.round)) +
=======
      ggtitle(label=t, subtitle = paste("Source: Afrobarometer, Round", af.round)) +
>>>>>>> 5213a1ca1e85bcce507cf629acddab24fe7cf2d3
      labs(fill="") +
      geom_bar(aes(fill=eval(parse(text=question2))), position=pos)

    print(g)
<<<<<<< HEAD

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
=======
    a[[question]] <- factor(a[[question]])
    a[[question2]] <- factor(a[[question2]])

    print(t)

    round(prop.table(table(a[[question]], a[[question2]])),digits)
  }
>>>>>>> 5213a1ca1e85bcce507cf629acddab24fe7cf2d3


}
