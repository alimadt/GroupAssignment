rm(list=ls())
library(tidyverse)
library(modelsummary)


# SECTION 1 (Just preparing data and creating some new variables)

df <- read.csv('https://osf.io/yzntm/download')

# Data frame with Central European countries
df2 <- df %>% filter(addresscountryname %in% c('Czech Republic', 'Germany', 'Italy', 'Hungary', 'Austria',
                                                 'Poland', 'Slovakia'))

# True price for 1 night
df2 <- separate(df2, price_night, ' ', into =
                     c('pr','word', 'nights', 'night'))
df2 <- select(df2, -pr)
df2 <- select(df2, -night)
df2 <- select(df2, -word)
df2$nights <- as.numeric(df2$nights)
df2$trueprice <- df2$price/df2$nights

# guestreviewrating separation
df2 <- separate(df2, guestreviewsrating, ' /', into =
                 c('actualrating', 'rate'))
df2 <- select(df2, -rate)
df2$actualrating <- as.numeric(df2$actualrating)

# Accomtype
df2 <- separate(df2, accommodationtype, '@', into = 
                 c('garbage', 'acctype'))
df2 <- select(df2, -garbage)
df2 <- mutate(df2, acctype_f = factor(acctype))

# Season
df2 <- df2 %>%
  mutate(season = case_when(month == 12 | month == 1 |month == 2 ~ 'winter',
                            month == 3 | month == 4 |month == 5 ~ 'spring',
                            month == 6 | month == 7 |month == 8 ~ 'summer',
                            month == 9 | month == 10 |month == 11 ~ 'autumn'))

# Hotel classification
df2 <- df2 %>%
  mutate(class = case_when(starrating == 0.0 | starrating == 1.0 ~ 'tourist',
                           starrating == 1.5 | starrating == 2.0 ~ 'standard',
                           starrating == 2.5 | starrating == 3.0 ~ 'comfort',
                           starrating == 3.5 | starrating == 4.0 ~ 'first class',
                           starrating == 4.5 | starrating == 5.0 ~ 'luxury'))
xtabs(~ class + country, data = df2, addNA = TRUE)

# Country as factor
df2 <- mutate(df2, country = factor(addresscountryname))

# Distance
df2 <- separate(df2, center1distance, ' ', into =
                  c('distance', 'miles'))
df2 <- select(df2, -miles)
df2$distance <- as.numeric(df2$distance)



# SECTION 2 (The beginning of descriptive statistics)

# Number of hotels in countries
count(df2, country)

# Mean prices for countries
datasummary(trueprice*country ~ Mean + Max + Min, data=df2)

# Ratings for diff countries
datasummary(actualrating*country ~ Mean, data=df2)

# Plot for diff years diff countries
df3 <- aggregate(trueprice ~ country + year, 
                 data=df2, 
                 function(x) { 
                   c(mean_price = mean(x)) 
                 })
ggplot(data = df3, aes(x=year, y=trueprice, fill=country)) + 
  geom_bar(stat='identity') + facet_wrap(~country) +
  geom_text(aes(label = round(trueprice)), vjust = -0.2)

# Mean prices for diff seasons
datasummary(trueprice*season ~ Mean, data=df2)

# Plot for classes
count(df2, class)
ggplot(data = df2, aes(x=reorder(class, +trueprice), y=trueprice)) + geom_bar(stat = 'summary')