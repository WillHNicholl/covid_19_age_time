extrafont::loadfonts(device = "win")
library(tidyverse)
library(gganimate)

setwd("C:/Users/331180/Documents/R/Other")

covid_age <- read_csv("covid_age.csv")


covid_age <- covid_age %>% 
  select(date, age, newCasesBySpecimenDate) %>% 
  filter(!(age %in% c("0_59", "60+", "unassigned"))) %>%
  mutate(age = str_replace(age, "_", "-")) %>% 
  mutate(age = factor(age, levels = c("0-4", 
                                      "5-9",
                                      "10-14", 
                                      "15-19", 
                                      "20-24", 
                                      "25-29", 
                                      "30-34", 
                                      "35-39", 
                                      "40-44", 
                                      "45-49",
                                      "50-54", 
                                      "55-59",
                                      "60-64",
                                      "65-69",
                                      "70-74",
                                      "75-79",
                                      "80-84",
                                      "85-89",
                                      "90+")))


levels(covid_age$age)

# df1 <- covid_age %>% 
#   group_by(age, date) %>% 
#   arrange(date) %>% 
#   summarise(newCasesBySpecimenDate = sum(newCasesBySpecimenDate)) %>%
#   mutate(rol_sum = cumsum(newCasesBySpecimenDate)) %>%
#   group_by(date) %>%
#   arrange(-rol_sum) %>%
#   mutate(rank=row_number()) %>%
#   filter(rank<=5)
# 
# levels(covid_age$age)
# 
# 
# t <- df1 %>%
#   ggplot(aes(x = -rank,y = rol_sum, group = age)) +
#   geom_tile(aes(y = rol_sum / 2, height = rol_sum, fill = age), width = 0.9) +
#   geom_text(aes(y = 0, label = paste(age, " ")), vjust = 0.2, hjust = 1) +
#   geom_text(aes(label = scales::comma(rol_sum)), hjust = "left", nudge_y = 100000, colour = "grey30") +
#   coord_flip(clip="off") +
#   #scale_fill_manual(name = 'age', values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#AA0B3C")) +
#   scale_x_discrete("") +
#   scale_y_continuous("",labels=scales::comma) +
#   hrbrthemes::theme_ipsum(plot_title_size = 20, subtitle_size = 14, caption_size = 12, base_size = 10) +
#   theme(panel.grid.major.y=element_blank(),
#         panel.grid.minor.x=element_blank(),
#         legend.position = "bottom",
#         plot.margin = margin(2,1,2,4,"cm"),
#         axis.text.y=element_blank()) +
#   # gganimate code to transition by year:
#   transition_time(date) +
#   ease_aes('cubic-in-out') +
#   labs(title='Covid-19 UK: Number of Cases by {round(frame_time,0)}',
#        subtitle='Top 5 Age Groups Nation-wide',
#        caption='Calculated as a daily rolling sum of total cases to date.
#        Data from: https://coronavirus.data.gov.uk/details/download') +
#   guides(fill=guide_legend(title="Age Group"))
# 
# t
# 
# animate(t, 
#         nframes = 1000, 
#         fps = 25, 
#         start_pause = 50,
#         end_pause = 50, 
#         width = 1200, 
#         height = 900,
#         renderer = gifski_renderer("gganim_total_covid.gif"))




df2 <- covid_age %>% 
  group_by(age, date) %>% 
  arrange(date) %>% 
  summarise(newCasesBySpecimenDate = sum(newCasesBySpecimenDate)) %>%
  group_by(date) %>%
  arrange(-newCasesBySpecimenDate) %>%
  mutate(rank=row_number()) %>%
  group_by(age) %>%
  filter(rank<=5)


p <- df2 %>%
  ggplot(aes(x = -rank,y = newCasesBySpecimenDate, group = age)) +
  geom_tile(aes(y = newCasesBySpecimenDate / 2, height = newCasesBySpecimenDate, fill = age), width = 0.9) +
  geom_text(aes(y = 0, label = paste(age, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(label = scales::comma(newCasesBySpecimenDate)), hjust = "left", nudge_y = 100000, colour = "grey30") +
  coord_flip(clip="off") +
  #scale_fill_manual(name = 'age', values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#AA0B3C")) +
  scale_x_discrete("") +
  scale_y_continuous("",labels=scales::comma) +
  hrbrthemes::theme_ipsum(plot_title_size = 20, subtitle_size = 14, caption_size = 12, base_size = 10) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = "bottom",
        plot.margin = margin(2,1,2,4,"cm"),
        axis.text.y=element_blank()) +
  # gganimate code to transition by year:
  transition_time(date) +
  ease_aes('cubic-in-out') +
  labs(title='Covid-19 UK: Daily Number of Cases on {round(frame_time,0)}',
       subtitle='Top 5 age groups on a given day',
       caption='Calculated as a nation-wide daily sum of cases, grouped by age group.
       Data from: https://coronavirus.data.gov.uk/details/download') +
  guides(fill=guide_legend(title="Age Group"))


#p


animate(p, 
        nframes = 1000, 
        fps = 25, 
        start_pause = 50,
        end_pause = 50, 
        width = 1200, 
        height = 900,
        renderer = gifski_renderer("gganim_daily_covid.gif"))
