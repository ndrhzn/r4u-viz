library(dplyr)
library(stringr)
library(ggplot2)
library(ggbeeswarm)

agreement <- read.csv('data/people_comparisons.csv', stringsAsFactors = F)

#process data-------------------------------------------------------------------

agreement <- agreement %>% select(person.latest_member.name.first,
                                  person.latest_member.name.last,
                                  person.latest_member.party,
                                  agreement)

agreement$person.latest_member.party <- agreement$person.latest_member.party %>% 
  str_replace_all(pattern = "Фракція Політичної партії \"НАРОДНИЙ ФРОНТ\"", 
                  replacement = 'Народний фронт') %>% 
  str_replace_all(pattern = "Фракція ПАРТІЇ \"БЛОК ПЕТРА ПОРОШЕНКА\"",
                  replacement = 'Блок Петра Порошенка') %>%
  str_replace_all(pattern = "Фракція політичної партії \"Всеукраїнське об'єднання \"Батьківщина\" у Верховній Раді України",
                  replacement = 'ВО "Батьківщина"') %>%
  str_replace_all(pattern = "Фракція Радикальної партії Олега Ляшка" ,
                  replacement = 'Радикальна партія Олега Ляшка') %>%
  str_replace_all(pattern = "Група \"Воля народу\"" ,
                  replacement = 'Воля народу') %>%
  str_replace_all(pattern = "Фракція Політичної партії \"Об'єднання \"САМОПОМІЧ\""  ,
                  replacement = 'Об’єднання "Самопоміч"') %>%
  str_replace_all(pattern = "Група \"Партія \"Відродження\""  ,
                  replacement = 'Відродження') %>%
  str_replace_all(pattern = "Фракція Політичної партії \"Опозиційний блок\" у Верховній Раді України восьмого скликання"   ,
                  replacement = 'Опозиційний блок')

parties <- agreement %>% 
  select(person.latest_member.party) %>% 
  unique.data.frame() %>% 
  mutate(person.latest_member.party = factor(person.latest_member.party))

#visualize----------------------------------------------------------------------

png(filename = 'agreement.png', width = 1000, height = 1000)

g <- ggplot(agreement)+
  geom_quasirandom(aes(x = reorder(person.latest_member.party, agreement), 
                       y = agreement,
                       color = agreement), 
                   varwidth = T, alpha = 0.7, size = 3)+
  scale_y_continuous(expand = c(0, 0.25), breaks = c(60, 85, 95, 100), 
                     limits = c(35, 100), position = 'top')+
  scale_color_continuous(low = '#528E93', high = '#0A1927')+
  coord_flip()+
  labs(x = NULL, y = "підтримка політичного курсу Президента, %\n",
       title = 'Підтримка політики Президента в розрізі фракцій',
       subtitle = str_wrap('Кожна точка на графіку - це один депутат. Положення точки на осі Х позначає рівень підтримки депутатом законодавчих ініціатив Президента', 110),
       caption = 'Дані: Проект "Вони голосують для тебе" | rada4you.org')+
  theme_minimal(base_family = 'Ubuntu Condensed')+
  theme(
    text = element_text(color = '#3A3F4A'), 
    axis.title.y = element_blank(),
    axis.title.x = element_text(hjust = 1, size = 16),
    axis.text = element_text(size = 16),
    axis.text.y = element_blank(),
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


  g = g + geom_text(data = parties,
                    aes(y = 35, x = person.latest_member.party, 
                        label = person.latest_member.party, 
                        family = "Ubuntu Condensed"), 
                    hjust = 0, vjust = -0.5,  
                    color = "#3A3F4A", size = 6)
  
g

dev.off()
