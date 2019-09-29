########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 25, 2019
########################################################################################

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# 2. Read & inspect the dataset ----
cotton <- read_csv("data/cotton-usda-nass.csv")
str(cotton)
summary(cotton)
dim(cotton)
colnames(cotton)
head(cotton)
tail(cotton)

# 3.1. Create a NC data subset ----
cotton %>%
  select(year, state, ag_district, county, data_item, value) %>%
  filter(state == "NORTH CAROLINA") -> nc_cotton

# 3.2. Divide the data_item column ----
nc_cotton %>%
  separate(data_item, 
           into = c("cotton_type", "measurement"),
           sep = "-") -> nc_cotton

# 3.3. Convert the value column to numeric type ----
nc_cotton %>%
  subset(value != "(D)") -> nc_cotton

as.numeric(nc_cotton$value)

nc_cotton$value <- as.numeric(nc_cotton$value)

str(nc_cotton)

# 4. Visualizing trends ----
nc_cotton %>%
  ggplot(nc_cotton, mapping = aes(x = year, y = value)) +
  geom_point() +
  facet_grid(cols = vars(ag_district), rows = vars(measurement),
             scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.title.y=element_blank()) +
  xlab("Year")+
  labs(title = "Cotton Production in NC", 
       caption = "Source: USDA NASS")

# 5. Summarize data from 2018 ----
nc_cotton %>%
  filter(year == "2018") %>%
  spread(key = "measurement", value = "value") -> nc_cotton_2018
  
nc_cotton_2018 %>%
  rename(yield = ` YIELD, MEASURED IN LB / ACRE`) %>%
  rename(acres = ` ACRES HARVESTED`) %>%
  mutate(total_lbs = (yield * acres)) %>%
  select(county, total_lbs) %>%
  top_n(3, total_lbs)
            