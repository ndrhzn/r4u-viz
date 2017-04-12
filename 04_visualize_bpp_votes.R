library(ggplot2)
library(dplyr)
library(stringr)

votes <- read.csv('data/personal_votes.csv', stringsAsFactors = F)

#process data-------------------------------------------------------------------

votes_summarised <- votes %>% 
  filter(member.party == 'Фракція ПАРТІЇ "БЛОК ПЕТРА ПОРОШЕНКА"',
         !vote %in% c('no', 'abstention')) %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(date, id, vote) %>% 
  summarise(count = n()) %>% 
  arrange(date)
  

text = data.frame(x = max(votes_summarised$date) + 30, y = c(90, 25), 
                  label = c('за', 'не голосували'), vote = c('aye', 'not voting'))


#visualize----------------------------------------------------------------------

png(filename = 'bpp_votes.png', width = 1000, height = 1000)

ggplot(votes_summarised, aes(x = date, y = count, color = vote))+
  geom_point(alpha = 0.7, size = 3)+
  geom_smooth(se = F, method = 'lm', size = 0.5)+
  geom_text(data = text, aes(x = x, y = y, label = label, color = vote), 
            angle = 90, hjust = 0.5, 
            family = 'Ubuntu Condensed', size = 6)+
  scale_color_manual(values = c('aye' = '#2C3E50', 'not voting' = '#E74C3C'))+
  scale_x_date(expand = c(0.01, 0.01))+
  labs(x = NULL, y = 'кількість депутатів',
       title = 'Як БПП голосує за законодавчі ініціативи Президента',
       subtitle = str_wrap('Кожна точка на графіку - одне голосування у Верховній Раді. Положення точки на осі Y позначає кількість депутатів, які проголосували "За" або ж не брали участі в цьому голосуванні. Лінія тренду вказує на те, що з часом підтримка законодавчих ініціатив Президента у БПП зменшується, разом з тим зростає кількість депутатів, що не беруть участі в голосуваннях', 110),
       caption = 'Дані: Проект "Вони голосують для тебе" | rada4you.org')+
  theme_minimal(base_family = 'Ubuntu Condensed')+
  theme(
    text = element_text(color = '#3A3F4A'), 
    axis.title.x = element_blank(),
    axis.title.y = element_text(hjust = 1, size = 16),
    axis.text = element_text(size = 16),
    axis.ticks.length = unit(0.5, 'lines'),
    panel.grid.major = element_line(linetype = "dotted", size = 0.3, color = '#5D646F'),
    panel.spacing.x = unit(5, 'lines'),
    panel.grid.minor = element_blank(),
    legend.position = 'none',
    legend.title = element_text(hjust = 0.5),
    legend.margin = margin(b = -10, t = 0),
    plot.title = element_text(face = "bold", size = 34, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 18, margin = margin(b = 20)),
    plot.caption = element_text(size = 14, margin = margin(b = 10, t = 50), color = "#5D646F"),
    plot.background = element_rect(fill = "#EFF2F4"),
    plot.margin = unit(c(2, 3, 2, 3), "cm")
  )

dev.off()