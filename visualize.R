library(ggplot2)
library(dplyr)

source('parse_data.R')

df <- parse_data() %>% 
  filter(convocation == max(convocation)) %>% 
  select(-id, -birthday) %>% 
  arrange(year, gender) %>% 
  group_by(year, gender) %>% 
  mutate(label = row_number()) %>% 
  ungroup()

segment <- data.frame(
  x = c(1933, 1990),
  y = c(3, 2),
  xend = c(1936, 1987),
  yend = c(12, 12)
)

annotation <- df %>% 
  filter(year %in% c(max(year), min(year))) %>% 
  mutate(name = paste(first_name, last_name)) %>% 
  group_by(year) %>% 
  summarise(name = paste(name, collapse = '\n'))

median_text <- data.frame(
  x = df$year %>% median(),
  y = 28.25,
  label = 'медіана'
)
  
png(filename = 'years.png', width = 1000, height = 600)

ggplot(df)+
  geom_vline(xintercept = df$year %>% median(), color = '#5D646F')+
  geom_point(aes(x = year, y = label, color = factor(gender)), size = 4.5)+
  geom_curve(data = segment[1,], aes(x = x, y = y, xend = xend, yend = yend), 
             curvature = -0.25, color = '#5D646F')+
  geom_curve(data = segment[2,], aes(x = x, y = y, xend = xend, yend = yend), 
             curvature = 0.25, color = '#5D646F')+
  geom_text(data = annotation %>% filter(year == min(year)), 
            aes(x = year, y = 13.5, label = paste(name, collapse = '\n')),
            family = 'Ubuntu Mono', size = 4, color = '#5D646F', hjust = 'left')+
  geom_text(data = annotation %>% filter(year == max(year)), 
            aes(x = year, y = 13.5, label = paste(name, collapse = '\n')),
            family = 'Ubuntu Mono', size = 4, color = '#5D646F', hjust = 'right')+
  geom_text(data = median_text, aes(x = x, y = y, label = label), 
            angle = 90, nudge_x = -0.5,
            family = 'Ubuntu Mono', size = 4, color = '#5D646F', hjust = 'center')+
  scale_color_brewer(palette = 7, type = 'qual', direction = 1)+
  scale_x_continuous(breaks = seq(1920, 1990, 5))+
  scale_y_continuous(breaks = c(1, seq(5, 30, 5)), limits = c(1, 30), expand = c(0.01, 0.01))+
  labs(
    title = 'Клуб старих чоловіків',
    subtitle = 'Розподіл депутатів ВР за роком народження та статтю',
    caption = 'Дані: Верховна Рада України | Візуалізація: Textura.in.ua'
  )+
  theme_minimal(base_family = 'Ubuntu Mono', base_size = 12)+
  theme(
    legend.position = 'none',
    text = element_text(color = '#5D646F'),
    axis.title = element_blank(),
    axis.text = element_text(size = 13),
    strip.text = element_blank(),
    axis.text.x.bottom = element_text(),
    panel.grid.major = element_line(linetype = 'dotted', color = '#5D646F', size = 0.1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 36, face = 'bold'),
    plot.subtitle = element_text(size = 18, margin = margin(b = 10)),
    plot.caption = element_text(size = 12, margin = margin(t = 20)),
    plot.background = element_rect(fill = '#F3F7F7'),
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), 'cm')
  )

dev.off()

rm(df, annotation, segment, median_text, parse_data)
