

```r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 3.4.2
```

```
## ── Attaching packages ─────────────────────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
## ✔ tibble  1.3.4     ✔ dplyr   0.7.4
## ✔ tidyr   0.7.2     ✔ stringr 1.2.0
## ✔ readr   1.1.1     ✔ forcats 0.2.0
```

```
## Warning: package 'tidyr' was built under R version 3.4.2
```

```
## Warning: package 'purrr' was built under R version 3.4.2
```

```
## Warning: package 'dplyr' was built under R version 3.4.2
```

```
## ── Conflicts ────────────────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(gganimate)
library(gtools)

income <- read_csv("income.csv")
```

```
## Parsed with column specification:
## cols(
##   year = col_integer(),
##   state = col_character(),
##   income_level = col_character(),
##   percent_of_total = col_double(),
##   number_of_households = col_integer()
## )
```

```r
income_levels <- mixedsort(unique(income$income_level))
income_levels <- c(income_levels[16], income_levels[1:15])

income <- income %>% mutate(income_level=factor(income_level,
                                                levels=income_levels))

census_regions <- read_csv("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")
```

```
## Parsed with column specification:
## cols(
##   State = col_character(),
##   `State Code` = col_character(),
##   Region = col_character(),
##   Division = col_character()
## )
```

```r
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
```

```
## Executing: 
## convert -loop 0 -delay 100 Rplot1.png Rplot2.png Rplot3.png
##     Rplot4.png Rplot5.png Rplot6.png Rplot7.png Rplot8.png
##     'Visualization.gif'
```

```
## Output at: Visualization.gif
```

