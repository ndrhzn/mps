library(dplyr)
library(ggplot2)
source('04_find_multiple_convocations.R')

# define convocations ---------------------------------------------------------

convocations <- data.frame(
  convocation = c(1, 2, 3, 4, 5, 6, 7, 8),
  years = c('1990-1994', '1994-1998', 
            '1998-2002', '2002-2006', 
            '2006-2007', '2007-2012', 
            '2012-2014', '2014-2019'),
  stringsAsFactors = F
)

# prepare data, join convocations ----------------------------------------------

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

# calculare pecentage ------------------------------------------------------

percentage <- df %>% 
  group_by(convocation, counter) %>% 
  summarise(count = n()) %>% 
  arrange(convocation, counter) %>% 
  inner_join(convocations) %>% 
  ungroup() %>% 
  group_by(convocation) %>% 
  mutate(mps_n = sum(count)) %>% 
  filter(counter == 1) %>% 
  mutate(percentage = round(count/mps_n * 100, digits = 0))

# add annotations data ---------------------------------------------------

annotations <- data.frame(
  x = 5.5,
  y = c(-2, -9, -20, -25, -27),
  text = c('Ми умовно приймаємо перше скликання ВР за точку відліку, але розуміємо, що деякі депутат/ки цього скликання раніше обирались до ВР УРСР',
           '64 людини з першого скликання ВР потрапили і до другого',
           '171 особа з перших двох скликань ВР перейшла до третього',
           '217 людей з попередніх трьох скликань... ви зрозуміли ідею, так?',           
           'Юхим Звягільський ‒ єдиний, хто був депутатом всіх восьми скликань ВР'),
  years = c('1990-1994', '1994-1998', '1998-2002', '2002-2006', '2014-2019')
)

percentage_label <- data.frame(
  text = 'нових депутатів/ок',
  years = '1990-1994'
)

arrow <- data.frame(
  x = 7.5,
  y = -27.5,
  xend = 8,
  yend = -20.5,
  years = '2014-2019'
)

## visualize -------------------------------------------------------------

png(filename = 'convocations.png', width = 1000, height = 850)

ggplot(df)+
  geom_tile(aes(x = x, y = y, fill = as.factor(counter)),
            size = 0.1, color = '#F3F7F7')+
  geom_hline(yintercept = 59, color = '#5D646F', size = 0.5)+
  geom_text(data = annotations,
            aes(x = x, y = y, label = stringr::str_wrap(text, 16)),
            family = 'Ubuntu Condensed', lineheight = 0.8,
            vjust = 1, color = '#5D646F', size = 4.5)+
  geom_text(data = percentage,
            aes(x = 5.5, y = 57.5, label = stringr::str_wrap(paste0(percentage, '%'), 16)),
            family = 'Ubuntu Condensed', lineheight = 0.95,
            vjust = 1, color = '#5D646F', size = 6)+
  geom_text(data = percentage_label,
            aes(x = 5.5, y = 54.5, label = stringr::str_wrap(text, 13)),
            family = 'Ubuntu Condensed', lineheight = 0.95,
            vjust = 1, color = '#5D646F', size = 4.5)+
  geom_curve(data = arrow, aes(x = x, y = y, xend = xend, yend = yend), 
             size = 0.5, color = '#5D646F',
             arrow = arrow(length = unit(5, 'pt')))+
  scale_fill_manual(values = c('#a6bddb', '#eda9d0','#e48dbb','#d971a6',
                               '#cb5692','#bb3a7d','#a71d68','#8e0152'))+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(breaks = c(-40, 59),
                     expand = c(0.01, 0.01),
                     labels = c('депутат/ки, які вже були у ВР', 
                                'депутат/ки, які вперше у ВР'))+
  facet_wrap(~years, nrow = 1)+
  guides(fill = guide_legend(title = 'яке це за рахунком скликання для депутат/ки?', 
                             title.position = 'top', 
                             nrow = 1))+
  labs(
    title = 'Як оновлювався склад Верховної Ради',
    subtitle = stringr::str_wrap('Кожен прямокутник на графіку ‒ це один депутат або депутатка у Верховній Раді. Кількість людей у скликаннях не однакова (і може бути більшою за 450), оскільки тут враховані всі, хто був депутатом чи депутаткою протягом певного скликання', 120),
    caption = 'Дані: Портал відкритих даних Верховної Ради України | Візуалізація: Textura.in.ua'
  )+
  theme_minimal(base_family = 'Ubuntu Condensed')+
  theme(
    text = element_text(color = '#5D646F'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(hjust = c(0, 1),
                               angle = 90, size = 13, margin = margin(r = 10)),
    axis.text.x = element_blank(),
    strip.text = element_text(size = 14, color = '#5D646F', margin = margin(b = 0)),
    axis.ticks = element_blank(),
    legend.position = 'top',
    legend.justification = 'left',
    legend.margin = margin(l = 0, b = 10),
    legend.title = element_text(size = 13.5),
    legend.text = element_text(size = 12),
    plot.title = element_text(family = 'Ubuntu Mono', 
                              size = 36, face = 'bold', margin = margin(b = 10)),
    plot.subtitle = element_text(size = 18, margin = margin(b = 10)),
    plot.caption = element_text(size = 12, margin = margin(t = 20)),
    strip.background = element_rect(fill = '#F3F7F7', color = NA),
    panel.spacing.x = unit(20, 'pt'),
    panel.background = element_rect(fill = '#F3F7F7', color = NA),
    plot.background = element_rect(fill = '#F3F7F7'),
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), 'cm'))

dev.off()

rm(df, convocations, annotations, percentage, percentage_label, arrow)