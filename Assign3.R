library(tidyverse)
library(dplyr)
library(broom)

tidy_anscombe <- anscombe %>%
  pivot_longer(cols = everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)")

ggplot(tidy_anscombe,
       aes(x = x,
           y = y)) +
  geom_point() + 
  facet_wrap(~set) +
  geom_smooth(method = "lm", se = FALSE)
#> `geom_smooth()` using formula 'y ~ x'
