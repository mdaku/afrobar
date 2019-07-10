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

    t <- paste(var_label(afro[question][1])[[1]],var_label(afro[question2][1])[[1]], sep=" by \n")
    g <- a %>%
      #ggplot(aes(x=eval(parse(text=question)), y=eval(parse(text=question2)))) +
      ggplot(aes(x=eval(parse(text=question)))) +
      theme_minimal() +
      xlab("")+
      ylab("Count") +
      ggtitle(label=t, subtitle = paste("Source: Afrobarometer, Round", af.round)) +
      labs(fill="") +
      geom_bar(aes(fill=eval(parse(text=question2))), position=pos)

    print(g)
    a[[question]] <- factor(a[[question]])
    a[[question2]] <- factor(a[[question2]])

    print(t)

    round(prop.table(table(a[[question]], a[[question2]])),digits)
  }


}
