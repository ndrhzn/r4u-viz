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
                  replacement = 'Опозиційний блок') %>% 
  str_wrap(width = 15)

#visualize----------------------------------------------------------------------

png(filename = 'agreement.png', width = 1000, height = 1000)

ggplot(agreement)+
  geom_quasirandom(aes(x = reorder(person.latest_member.party, agreement), y = agreement), 
                   varwidth = F, color = 'darkcyan', alpha = 0.7)+
  coord_flip()+
  labs(x = NULL, y = "підтримка політичного курсу президента, %",
       title = 'Підтримка політики президента в розрізі фракцій',
       subtitle = 'Кожна точка на графіку - це один депутат. Положення точки на осі Х позначає рівень підтримки депутатом політики "За політичний курс президента" від листопада 2014 року до травня 2016 року',
       caption = 'Дані: Проект "Вони голосують для тебе" | rada4you.org')+
  theme_minimal(base_family = 'Ubuntu Condensed')+
  theme(
    text = element_text(color = '#3A3F4A'), 
    axis.title.y = element_blank(),
    axis.title.x = element_text(hjust = 1, size = 16),
    axis.text = element_text(size = 16),
    axis.ticks.length = unit(0.5, 'lines'),
    panel.grid.major = element_line(linetype = "dotted", size = 0.3, color = '#5D646F'),
    panel.spacing.x = unit(5, 'lines'),
    panel.grid.minor = element_blank(),
    legend.position = 'none',
    legend.title = element_text(hjust = 0.5),
    legend.margin = margin(b = -10, t = 0),
    plot.title = element_text(face = "bold", size = 34, margin = margin(b = 20)),
    plot.subtitle = element_text(size = 22, margin = margin(b = 20)),
    plot.caption = element_text(size = 14, margin = margin(b = 10, t = 50), color = "#5D646F"),
    plot.background = element_rect(fill = "#EFF2F4"),
    plot.margin = unit(c(2, 3, 2, 3), "cm")
  )

dev.off()
