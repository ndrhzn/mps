library(dplyr)
library(ggplot2)
library(waffle)

convocations <- data.frame(
  convocation = c(1, 2, 3, 4, 5, 6, 7, 8),
  years = c('1990-1994', '1994-1998', 
            '1998-2002', '2002-2006', 
            '2006-2007', '2007-2012', 
            '2012-2014', '2014-2019'),
  stringsAsFactors = F
)

df_agg <- df %>% 
  group_by(convocation, counter) %>% 
  summarise(count = n()) %>% 
  arrange(convocation, counter) %>% 
  inner_join(convocations)

annotations <- data.frame(
  x = 5.5,
  y = 60,
  text = 'text'
)

png(filename = 'convocations.png', width = 1000, height = 900)

ggplot(df_agg)+
  geom_waffle(aes(fill = as.character(counter), values = count),
              color = 'white', size = 0.025, flip = TRUE)+
  # geom_text(data = annotations, 
  #           aes(x = x, y = y, label = text), 
  #           family = 'Ubuntu Mono', color = '#5D646F')+
  scale_fill_manual(values = c('#7fc97f', '#eda9d0','#e48dbb','#d971a6',
                               '#cb5692','#bb3a7d','#a71d68','#8e0152'))+
  scale_y_continuous(limits = c(0, 60), expand = c(0.01, 0.01))+
  facet_wrap(~years, nrow = 1)+
  guides(fill = guide_legend(title = 'яке це за рахунком скликання для депутата?', 
                             title.position = 'top',
                             nrow = 1))+
  labs(
    title = 'Як оновлювався склад Верховної ради',
    subtitle = 'Кожен прямокутник на графіку - це один депутат або депутатка у Верховній Раді',
    caption = 'Дані: Верховна Рада України | Візуалізація: Textura.in.ua'
  )+
  theme_minimal(base_family = 'Ubuntu Mono')+
  theme(
        text = element_text(color = '#5D646F'),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(size = 14, color = '#5D646F', margin = margin(b = 0)),
        axis.ticks = element_blank(),
        legend.position = 'top',
        legend.justification = 'left',
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 36, face = 'bold', margin = margin(b = 10)),
        plot.subtitle = element_text(size = 18, margin = margin(b = 10)),
        plot.caption = element_text(size = 12, margin = margin(t = 20)),
        strip.background = element_rect(fill = '#F3F7F7', color = NA),
        panel.background = element_rect(fill = '#F3F7F7', color = NA),
        plot.background = element_rect(fill = '#F3F7F7'),
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), 'cm'))

dev.off()