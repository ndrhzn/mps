library(dplyr)
source('01_parse_data.R')

# read data ------------------------------------------------------------

df <- parse_data()

# find potential duplicates with different surnames ----------------------

duplicates <- data.frame()

for(i in 1:nrow(df)) {
  
  foo <- df %>% 
    filter(birthday == df$birthday[i],
           first_name == df$first_name[i],
           last_name != df$last_name[i])
  
  if(nrow(foo) > 0) {
    
   duplicates <- rbind.data.frame(duplicates, foo)
     
  }
}

duplicates <- duplicates %>% 
  arrange(birthday, first_name, convocation) %>% 
  unique.data.frame()

# match duplicate records -----------------------------------------------

df <- df %>% 
  mutate(last_name = case_when(
    first_name == 'Адам' & last_name == 'Чікал' ~ 'Чикал',
    first_name == 'Віктор' & last_name %in% c('Таран (Терен)', 'Таран-Терен') ~ 'Таран',
    first_name == 'Єгор' & last_name == 'Анненков' ~ 'Аннєнков',
    first_name == 'Ігор' & last_name == 'Плужніков' ~ 'Плужников',
    first_name == 'Ірина' & last_name == 'Бєлоусова' ~ 'Белоусова',
    first_name == 'Костянтин' & last_name == 'Пискуновський' ~ 'Піскуновський',
    first_name == 'Мустафа' & last_name == 'Джемілев' ~ 'Джемілєв',
    first_name == 'Олег' & last_name == 'Грачов' ~ 'Грачев',
    first_name == 'Юрій' & last_name == 'Чмир' ~ 'Чмирь',
    first_name == 'Віктор' & last_name == 'Веретенніков' ~ 'Веретенников',
    first_name == 'Борис' & last_name == 'Кожевников' ~ 'Кожевніков',
    first_name == 'Валерій' & last_name == 'Коломойцев' ~ 'Коломойцев-Рибалка',
    TRUE ~ last_name
  ))

# count number of convocations -------------------------------------------

df <- df %>% 
  mutate(birthday_fullname = paste(birthday, first_name, last_name)) %>% 
  unique.data.frame()

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

# clean -------------------------------------------------------------------------

rm(foo, duplicates, i)
