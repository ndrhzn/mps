library(dplyr)
library(ggplot2)
source('04_find_multiple_convocations.R')

## ---------------------------------------------------------------------

convocations <- data.frame(
  convocation = c(1, 2, 3, 4, 5, 6, 7, 8),
  years = c('1990-1994', '1994-1998', 
            '1998-2002', '2002-2006', 
            '2006-2007', '2007-2012', 
            '2012-2014', '2014-2019'),
  stringsAsFactors = F
)

df <- df %>% 
  select(convocation, first_name, last_name, counter) %>% 
  inner_join(convocations) %>% 
  arrange(convocation, counter) %>% 
  group_by(convocation, years, counter > 1) %>% 
  do(cbind.data.frame(first_name = .$first_name,
                      last_name = .$last_name,
                      counter = .$counter,
                      head(expand.grid(x = 1:10, y = 1:60), nrow(.)))) %>% 
  ungroup() %>% 
  mutate(y = ifelse(counter > 1, (y * -1) + 1, y),
         counter = as.character(counter)) %>% 
  select(-`counter > 1`)

#write.csv(df, file = 'convocations.csv', row.names = F)
#jsonlite::toJSON(df, auto_unbox = T) %>% writeLines('convocations.json')

## ----------------------------------------------------------------------

png(filename = 'convocations.png', width = 1000, height = 1000)

ggplot(df)+
  geom_tile(aes(x = x, y = y, fill = as.factor(counter)),
            size = 0.1, color = '#F3F7F7', alpha = 0.8)+
  geom_hline(yintercept = 50, color = '#5D646F', size = 0.5)+
  scale_fill_manual(values = c('#7fc97f', '#eda9d0','#e48dbb','#d971a6',
                               '#cb5692','#bb3a7d','#a71d68','#8e0152'))+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(breaks = c(-40, 50),
                     expand = c(0.01, 0.01),
                     labels = c('депутати, які не вперше у ВР', 
                                'депутати, які вперше у ВР'))+
  facet_wrap(~years, nrow = 1)+
  guides(fill = guide_legend(title = 'яке це за рахунком скликання для депутата?', 
                             title.position = 'top', 
                             nrow = 1))+
  labs(
    title = 'Як оновлювався склад Верховної ради',
    subtitle = stringr::str_wrap('Кожен прямокутник на графіку - це один депутат або депутатка у Верховній Раді. Кількість депутатів у скликаннях не однакова, оскільки тут враховані всі люди, що були депутатами протягом певного скликання - навіть ті, що мали депутатський мандат лише кілька днів, а потім склали повноваження через призначення у Кабінеті Міністрів чи іншої причини', 100),
    caption = 'Дані: Верховна Рада України | Візуалізація: Textura.in.ua'
  )+
  theme_minimal(base_family = 'Ubuntu Mono')+
  theme(
    text = element_text(color = '#5D646F'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(hjust = c(0, 1), angle = 90, size = 13),
    axis.text.x = element_blank(),
    strip.text = element_text(size = 14, color = '#5D646F', margin = margin(b = 0)),
    axis.ticks = element_blank(),
    legend.position = 'top',
    legend.justification = 'left',
    legend.margin = margin(l = 0, b = 10),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 36, face = 'bold', margin = margin(b = 10)),
    plot.subtitle = element_text(size = 18, margin = margin(b = 10)),
    plot.caption = element_text(size = 12, margin = margin(t = 20)),
    strip.background = element_rect(fill = '#F3F7F7', color = NA),
    panel.spacing.x = unit(15, 'pt'),
    panel.background = element_rect(fill = '#F3F7F7', color = NA),
    plot.background = element_rect(fill = '#F3F7F7'),
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), 'cm'))

dev.off()