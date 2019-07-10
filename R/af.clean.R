# afro.clean.R

# Cleans and tidies the data

af.clean <<- function(afro){
  # Lower case all of the functions
  afro <- afro %>% rename_all(tolower)

  # Replace all of the missing labels with NA
#  afro <- afro %>% select(-dateintr, -strtime) %>%
#   mutate_all(~na_if(., -1))

  afro <- haven::as_factor(afro)

  # Return dataframe
  afro
}
