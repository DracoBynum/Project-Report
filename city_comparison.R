#install.packages("dplyr")
library("dplyr")
#install.packages("ggplot2")
library("ggplot2")


Seattle_population <- 744955

seattle_crimes <- read.csv('Seattle_2018.csv', stringsAsFactors=FALSE) %>%
  filter(startsWith(Crime.Subcategory, "AGGRAVATED ASSAULT") | startsWith(Crime.Subcategory, "ROBBERY") |
           startsWith(Crime.Subcategory, "BURGLARY")) %>%
  mutate(crime_type = substr(Crime.Subcategory, 1, 4), city = "Seattle")  %>%
  select(c(crime_type, city))

  seattle_crimes$crime_type[seattle_crimes$crime_type == "AGGR"] <- "Aggravated-Assault"
  seattle_crimes$crime_type[seattle_crimes$crime_type == "ROBB"] <- "Robbery"
  seattle_crimes$crime_type[seattle_crimes$crime_type == "BURG"] <- "Burglary"
  
seattle_crimes <- group_by(seattle_crimes, crime_type) %>%
  summarize(crime_amount = n()) %>%
  mutate(city = "Seattle")



Denver_population <- 716492

denver_crimes <- read.csv('Denver_2018.csv', stringsAsFactors=FALSE) %>%
  filter(OFFENSE_CATEGORY_ID == "aggravated-assault" | OFFENSE_CATEGORY_ID == "robbery" |
           OFFENSE_CATEGORY_ID == "burglary") %>%
  mutate(city = "Denver") %>%
  select(c(OFFENSE_CATEGORY_ID, city)) %>%
  rename(crime_type = OFFENSE_CATEGORY_ID)

  denver_crimes$crime_type[denver_crimes$crime_type == "aggravated-assault"] <- "Aggravated-Assault"
  denver_crimes$crime_type[denver_crimes$crime_type == "burglary"] <- "Burglary"
  denver_crimes$crime_type[denver_crimes$crime_type == "robbery"] <- "Robbery"

  denver_crimes <- group_by(denver_crimes, crime_type) %>%
    summarize(crime_amount = (n() * (Seattle_population / Denver_population))) %>%
    mutate(city = "Denver")
  
  
  
DC_population <- 702455

DC_crimes <- read.csv('DC_2018.csv', stringsAsFactors = FALSE)  %>%
  filter(offense.text == "robbery" | offense.text == "burglary" | offense.text == "assault w/dangerous weapon") %>%
  mutate(city = "D.C") %>%
  select(c(offense.text, city)) %>%
  rename(crime_type = offense.text)

  DC_crimes$crime_type[DC_crimes$crime_type == "assault w/dangerous weapon"] <- "Aggravated-Assault"
  DC_crimes$crime_type[DC_crimes$crime_type == "burglary"] <- "Burglary"
  DC_crimes$crime_type[DC_crimes$crime_type == "robbery"] <- "Robbery"

  
  DC_crimes <- group_by(DC_crimes, crime_type) %>%
    summarize(crime_amount = (n() * (Seattle_population / DC_population))) %>%
    mutate(city = "D.C.")
  


Seattle_and_Denver_crimes <- full_join(seattle_crimes, denver_crimes, by = c("crime_type", "crime_amount", "city")) 
city_comparison_df <- full_join(Seattle_and_Denver_crimes, DC_crimes, by = c("crime_type", "crime_amount", "city"))

city_comparision_plot <- ggplot(data = city_comparison_df) +
  geom_col(mapping = aes(x = crime_type, y = crime_amount, fill = city), position = "dodge") +
  labs(title = "Crime Comparison by City for 2018") +
  labs(x = "Crime Category") +
  labs(y = "Crimes Reported (Adj. by population)") +
  labs(fill = "City") +
  scale_fill_brewer(type = "qal", palette = "Set2") 
  
print(city_comparision_plot)
