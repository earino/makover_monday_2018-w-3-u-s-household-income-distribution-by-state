library(tidyverse)
library(animation)
library(tweenr)
library(gtools)

income <- read_csv("income.csv")
income_levels <- mixedsort(unique(income$income_level))
income_levels <- c(income_levels[16], income_levels[1:15])

income <- income %>% mutate(income_level=factor(income_level,
                                                levels=income_levels))

census_regions <- read_csv("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")

combined_frame <- income %>% inner_join(census_regions, by=c('state' = 'State'))

visualization_frame <-
  combined_frame %>%
  group_by(year, Region, income_level) %>%
  summarise(percent_of_total = mean(percent_of_total)) %>%
  ungroup()

mylist <-
  visualization_frame %>%
  mutate(year2 = year, Region = as_factor(Region, levels=c("Northeast", "South", "Midwest", "West"))) %>%
  nest(-year) %>%
  pull(data)

tween.df <- tween_states(mylist
                         ,tweenlength = 1
                         ,statelength = 2
                         ,ease = 'cubic-in-out'
                         ,nframes=121)

tween.df$income_level <- factor(tween.df$income_leve, levels=income_levels)
tween.df$compacted_level <- fct_relabel(tween.df$income_level, function(x) {
  idx <- match(x, levels(tween.df$income_level))
  c(rep("< 50", 9), rep("50 - 99", 3), rep("100 - 149", 2), "150 - 199", "200+")[idx]
})

myplot<-function(i){
  mydata <- filter(tween.df, i==.frame)
  year <- round(mydata[1,]$year)

  mydata$income_level <- factor(mydata$income_level,
                                levels=income_levels)

  g <-
    ggplot(data = mydata
           , aes(x = compacted_level, y = percent_of_total / 4, fill = Region)
    ) +
    geom_bar(stat="identity", position="stack") +
    theme_minimal() +
    ggtitle(paste("Distribution by Income Level:", year),
            "by percentage of total state population in thousands of dollars") +
    xlab("") +
    ylab("") +
    ylim(0, .5) +
    scale_fill_brewer(palette = "Set2")

  return(g)
}

oopt = ani.options(interval = 0.1)
saveGIF({for (i in seq_along(unique(tween.df$.frame))) {
  g <- myplot(i)
  print(g)
  print(i)
  ani.pause()
}
}, movie.name="incomedata_tween.gif", ani.width = 600, ani.height = 600)

income_levels[1:9]
income_levels[10:12]
income_levels[13:14]
income_levels[15:16]





