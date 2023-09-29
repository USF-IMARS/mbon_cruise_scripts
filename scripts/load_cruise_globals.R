load_cruise_globals <- function(){
  library(here)
  library(glue)
  source(here("cruise_variables", glue("{CRUISE_ID}.R")))
}