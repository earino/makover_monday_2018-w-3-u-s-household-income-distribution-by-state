library(tidyverse)
library(gganimate)
library(gtools)

income <- read_csv("income.csv")
income_levels <- mixedsort(unique(income$income_level))
income_levels <- c(income_levels[16], income_levels[1:15])

income <- income %>% mutate(income_level=factor(income_level,
                                                levels=income_levels))

census_regions <- read_csv("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")

combined_frame <- income %>% inner_join(census_regions, by=c('state' = 'State'))

visualization_frame <- combined_frame %>% group_by(year, Region, income_level) %>%
  summarise(percent_of_total=mean(percent_of_total))

p <- ggplot(visualization_frame, aes(x=income_level, y=percent_of_total, frame=year)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Distribution by Income Level", "by % of total state population") +
  facet_wrap(~ Region) +
  xlab("") +
  ylab("% of Population")

gganimate(p, filename = "Visualization.gif")

