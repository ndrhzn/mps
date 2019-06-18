library(ggplot2)
library(waffle)

df_agg <- df %>% 
  group_by(convocation, counter) %>% 
  summarise(count = n()) %>% 
  arrange(convocation, counter)

ggplot(df_agg)+
  geom_waffle(aes(fill = as.character(counter), values = count),
              color = "white", size = .1, flip = TRUE)+
  scale_fill_brewer(type = 'qual')+
  scale_y_continuous(limits = c(0, 60))+
  facet_wrap(~convocation, nrow = 1)+
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())