# Descriptive Table & Graphs
library(ggplot2)

af.tab <- function(question, digits=2, missing=T){
  # Need to check if question actually exists
  #v <- afro[question,]
  # We are missing the actual question text from the codebook
  print(var_label(afro[question][1])[[1]])
  round(prop.table(table(afro[question])),digits)
  #eval(parse(text = "x"))
  if(missing){
    a <- afro
  }
  else{
    a <- afro %>% filter(eval(parse(text=question)) != "NA") %>%
    filter(eval(parse(text=question)) != "Don't know")

  }
  a %>%
    ggplot(aes(x=eval(parse(text=question)))) +
    theme_minimal() +
    xlab("")+
    ylab("Count") +
    ggtitle(label=var_label(afro[question][1])[[1]], subtitle = paste("Source: Afrobarometer, Round", af.round)) +
    geom_bar()

  #
  # We also want this to actually look good
  #question
}
