library(tidyverse)
library(psych)

college <- read.csv("regression_example.csv")
college

describe(college)

linmode <- lm(GPA ~ SAT, data = college)

ggplot(college, aes(SAT, GPA)) +
  geom_point() +
  theme_light() +
  labs(x = "SAT Scores",
       y = "GPA upon graduation",
       title = "SAT and GPA") +
  stat_smooth(method = "lm", se = FALSE)


summary(linmode)



library(scales)

data <- read.csv("real_estate_price_size_year_view.csv")
describe(data)
summary(data)

point <- format_format(big.mark = " ", decimal.mark = ",", scientific = F)
ggplot(data, aes(price, size)) +
  geom_point() +
  theme_light() +
  labs(x = "House Price (in USD)",
       y = "House Size (in sq ft",
       title = "House Pricing and Size") +
  scale_x_continuous(labels = point) +
  scale_y_continuous(labels = point)

linmod <- lm(price ~ size, data = data)  

ggplot(data, aes(price, size)) +
  geom_point() +
  theme_light() +
  stat_smooth(method = "lm", se = F) +
  labs(x = "House price (in USD",
       y = "House size (in ft sq)",
       title = "House pricing and size") +
  scale_x_continuous(labels = point) +
  scale_y_continuous(labels = point)

summary(linmod)





