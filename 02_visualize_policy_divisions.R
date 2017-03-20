library(ggplot2)
library(dplyr)
library(stringr)

divisions <- read.csv('data/policy_divisions.csv', stringsAsFactors = F)

#process the data---------------------------------------------------------------

divisions <- divisions %>% 
  select(division.name, division.date, division.aye_votes, 
         division.no_votes, division.possible_turnout, division.rebellions) %>% 
  mutate(division.date = as.Date(division.date),
         accepted = division.aye_votes >= 225)

text = data.frame(x = max(divisions$division.date) + 30, y = c(200, 250), 
                  label = c('відхилені', 'прийняті'), accepted = c(FALSE, TRUE))

#visualize----------------------------------------------------------------------

png(filename = 'divisions.png', width = 1000, height = 1000)

ggplot(divisions, aes(x = division.date, y = division.aye_votes))+
  geom_hline(yintercept = 225, linetype = 'dashed', color = 'darkgray', size = 0.45)+
  geom_point(aes(color = accepted), size = 3, alpha = 0.75)+
  geom_smooth(se = F, method = 'lm', color = 'darkgray', size = 0.75)+
  geom_text(data = text, aes(x = x, y = y, label = label, color = accepted), 
            angle = 90, hjust = 0.5, 
            family = 'Ubuntu Condensed', size = 6)+
  scale_color_manual(values = c('TRUE' = '#2C3E50', 'FALSE' = '#E74C3C'))+
  scale_x_date(expand = c(0.01, 0.01))+
  scale_y_continuous(breaks = c(50, 100, 150, 200, 250, 300, 350), expand = c(0.05, 0.05))+
  labs(x = NULL, y = 'кількість голосів "за"',
       title = 'Як Рада голосує за законодавчі ініціативи Президента',
       subtitle = str_wrap('Кожна точка на графіку - одне голосування у Верховній Раді. Положення точки на осі Y позначає кількість голосів "За" в цьому голосуванні. Лінія тренду вказує на те, що з часом підтримка законодавчих ініціатив Президента у Раді зменшується', 110),
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