library(dplyr)

source('01_parse_data.R')

df <- parse_data() %>% 
  mutate(birthday_fullname = paste(birthday, first_name, last_name))

df$counter <- NA

for(i in 1:nrow(df)) {
  
  if(df$convocation[i] == 1) {
    
   df$counter[i] = 1
     
  } else if(df$convocation[i] > 1 &
            any(grepl(df$birthday_fullname[i], 
                  df[df$convocation < df$convocation[i],]$birthday_fullname))) {
    
    df$counter[i] = 1 + sum(grepl(df$birthday_fullname[i], 
                                  df[df$convocation < df$convocation[i],]$birthday_fullname))
    
  } else {
    
    df$counter[i] = 1
    
  }
}