library(tidyverse)
library(ggplot2)

getwd()

diabetes <- read.csv("~/Desktop/R project/3_Diabetes/Diabetes prevalence.csv")
str(diabetes)

obesity <- read.csv("~/Desktop/R project/3_Diabetes/Obesity prevalance.csv")
str(obesity)

#Data preparation
diabetes_1 <- diabetes %>%
  select(REF_AREA, REF_AREA_LABEL, X2011, X2021) %>%
  mutate(diabetes_change = X2021 - X2011) %>%
  rename(Location = REF_AREA_LABEL,
         diabetes_2021 = X2021,
         diabetes_2011 = X2011,
         code = REF_AREA)

obesity_1 <- obesity %>%
  select(SpatialDimValueCode, Location, Period, Value) %>%
  pivot_wider(
    names_from = Period,
    values_from = Value
  ) %>%
  rename(obesity_2021 = "2021",
         obesity_2011 = "2011",
         code = SpatialDimValueCode) %>%
  
  obesity_1 <- obesity_1 %>%
  mutate(
    obesity_2011 = as.numeric(str_extract(obesity_2011, "^[0-9.]+")),
    obesity_2021 = as.numeric(str_extract(obesity_2021, "^[0-9.]+")),
    obesity_change = obesity_2021 - obesity_2011
  )


##Diabetes
diabetes_summary_avg <- diabetes_1 %>%
  summarise(
    dm_mean_2011 = mean(diabetes_2011, na.rm = TRUE),
    dm_mean_2021 = mean(diabetes_2021, na.rm = TRUE),
    dm_mean_change = mean(diabetes_change, na.rm = TRUE)
  )
print(diabetes_summary_avg)

diabetes_long <- diabetes_1 %>%
  select(Location, diabetes_2011, diabetes_2021) %>%
  pivot_longer(cols = starts_with("diabetes"), 
               names_to = "Year", 
               values_to = "Prevalence") %>%
  mutate(Year = ifelse(Year == "diabetes_2011", "2011", "2021")) %>%
  filter(!is.na(Prevalence))

ggplot(diabetes_long, aes(Year, Prevalence)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Diabetes Prevalence (2011 vs. 2021)", y = "Prevalence (%)", x = NULL) +
  theme_minimal()

##Diabetes_top20(2021)
top20_DM_2021 <- diabetes_1 %>%
  arrange(desc(diabetes_2021)) %>%
  head(20)

ggplot(top20_DM_2021, aes(x = reorder(Location, diabetes_2021), y = diabetes_2021)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = round(diabetes_2021,1)), 
            hjust = -0.1) +
  labs(title = "Top 20 Countries by Diabetes Prevalence in 2021",
       subtitle = "Prevalence among adults aged 20–79",
       x = NULL, y = "Prevalance(%)") +
  coord_flip() +
  theme_minimal()

##Diabetes_change
diabetes_1 %>%
  filter(!is.na(diabetes_change)) %>%
  ggplot(aes(y = diabetes_change)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Distribution of Prevalence Change (2011-2021)", 
       y = "Change in Prevalence (%)", x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

##Diabetes_change_top20
top20_DMchange <- diabetes_1 %>%
  arrange(desc(abs(diabetes_change))) %>%
  head(20)

ggplot(top20_DMchange, aes(x = reorder(Location, abs(diabetes_change)), y = diabetes_change)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = diabetes_change), hjust = ifelse(top20_DMchange$diabetes_change > 0, -0.1, 1.1)) +
  coord_flip() +
  labs(
    title = "Top 20 Countries by Diabetes Prevalence Change (2011–2021)",
    subtitle = "Prevalence among adults aged 20–79",
    y = "Prevalence Change (%)",
    x = NULL
  ) +
  theme_minimal()

#Obesity
obesity_summary_avg <- obesity_1 %>%
  summarise(
    obesity_mean_2011 = mean(obesity_2011, na.rm = TRUE),
    obesity_2021 = mean(obesity_2021, na.rm = TRUE),
    obesity_change = mean(obesity_change, na.rm = TRUE)
  )
print(obesity_summary_avg)

#Obesity_cahnge
obesity_1 %>%
  filter(!is.na(obesity_change)) %>%
  ggplot(aes(y = obesity_change)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Distribution of Prevalence Change (2011-2021)", 
       y = "Change in Prevalence (%)", x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

##Obesity_change_top20
top20_Obesitychange <- obesity_1 %>%
  arrange(desc(abs(obesity_change))) %>%
  head(20)

ggplot(top20_Obesitychange, aes(x = reorder(Location, abs(obesity_change)), y = obesity_change)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = obesity_change), hjust = ifelse(top20_Obesitychange$obesity_change > 0, -0.1, 1.1)) +
  coord_flip() +
  labs (
    title = "Top 20 Countries by Obesity Prevalance Change (2011–2021)",
    subtitle = "Prevalence among adults (18 years and older)",
    y = "Prevalence Change (%)",
    x = NULL
  ) +
  theme_minimal()

#Merge Obesity and Diabetes
data <-obesity_1 %>%
  left_join(diabetes_1, by = "code")

data <- data %>%  
  select(-Location.y)

data <- data %>%
  select(code, Location.x, 
         obesity_2011, obesity_2021, obesity_change, 
         diabetes_2011, diabetes_2021, diabetes_change) %>%
  rename(Country = Location.x)

nrow(obesity_1)
nrow(data)

colSums(is.na(data))
data %>% 
  summarise(
    avg_obesity_2021 = mean(obesity_2021, na.rm = TRUE),
    avg_diabetes_2021 = mean(diabetes_2021, na.rm = TRUE)
  )

missing_list <- data %>%
  filter(is.na(diabetes_change)) %>%
  select(code, Location.x)
print(missing_list)

data_clean <- data %>%
  drop_na(obesity_change, diabetes_change)

#Diabetes v.s Obesity
##2021
cor.test(data_clean$diabetes_2021, data_clean$obesity_2021)

model_2021 <- lm(diabetes_2021 ~ obesity_2021, data = data_clean)
summary(model_2021)

ggplot(data_clean, aes(x = obesity_2021, y = diabetes_2021)) +
  geom_point() +
  geom_smooth(method = "lm")

##change
cor.test(data_clean$obesity_change, data_clean$diabetes_change)

model_change <- lm(diabetes_change ~ obesity_change, data = data_clean)
summary(model_change)

ggplot(data_clean, aes(x = obesity_change, y = diabetes_change)) +
  geom_point() +
  geom_smooth(method = "lm")

##residuals
data_clean <- data_clean %>%
  mutate(res_2021 = residuals(model_2021))

# Positive Outliers
top_positive_res <- data_clean %>%
  arrange(desc(res_2021)) %>%
  head(5) %>%
  select(Country, obesity_2021, diabetes_2021, res_2021)
print(top_positive_res)

# Negative Outliers
top_negative_res <- data_clean %>%
  arrange(res_2021) %>%
  head(5) %>%
  select(Country, obesity_2021, diabetes_2021, res_2021)
print(top_negative_res)


