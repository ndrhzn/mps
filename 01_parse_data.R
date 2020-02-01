library(dplyr)
library(lubridate)

parse_data <- function(){
  
  df <- data.frame()
  
  files <- list.files('data/raw/', full.names = TRUE)
  
  for(i in files) {
    
    x <- read.csv(i, stringsAsFactors = F)
    
    x <- x %>% 
      select(id, convocation, gender, birthday, last_name, first_name) %>% 
      mutate(birthday = as.Date(birthday, format = '%d.%m.%Y'),
             year = year(birthday)) %>% 
      filter(!is.na(birthday))
    
    df <- rbind.data.frame(df, x)
    
  }
  
  return(df)
  
}
