#Karlyn Ledesma 
#u1287358 
#Final Project Props & Stats
install.packages("dplyr")
install.packages("maps")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)

#Upload Stroke mortality data 
stroke_mortality_data <- read.csv("/Users/karlyn/Downloads/Stroke_Mortality_Data_2017-2019.csv")
#filter for UTAH counties (3 yaer average)
Utah_data <- stroke_mortality_data %>% filter(LocationAbbr == "UT")
#basic statistics of the Utah data 
summary(Utah_data)

#cleaning up the data 
#focused here on the data value column (xx per 100,000 population )
#checking for missing values in data value column 
missing_data <- is.na(Utah_data$Data_Value)
#counting the rows of missing data 
sum(is.na(Utah_data$Data_Value))
# 281 rows/counties with missing data :( 
#isolate the rows 
missing_rows <- Utah_data[missing_data,]
View(missing_rows)

#checking for duplicate data 
duplicates <- Utah_data[duplicated(Utah_data)]
#count
num_duplicates <- sum(duplicated(Utah_data))
print(num_duplicates)
#There are no duplicate rows 

#checking if a county has a 3 year 
weber_data <-Utah_data %>% filter(LocationDesc == "Weber County")
View(weber_data)
#each county has multiple rows due to to demographic variable (race and sex)
#for this project I will focus on overall 

#filter Utah stroke mortality for overall race and overall sex 
overall_stroke_data <- Utah_data %>% filter(Stratification1 == "Overall", Stratification2 == "Overall")
View(overall_stroke_data)
#overall_stroke_data to be used from now on 

#Descriptive Statistics 
summary(overall_stroke_data)
descriptive_stats_stroke <- Utah_data %>% 
  summarise(
    Count = n(),
    Mean = mean(Data_Value, na.rm = TRUE),
    Median = median(Data_Value, na.rm = TRUE),
    Mode = as.numeric(names(sort(table(Data_Value), decreasing = TRUE)[1])),
    SD = sd(Data_Value, na.rm = TRUE),
    Min = min(Data_Value, na.rm = TRUE),
    Max = max(Data_Value, na.rm = TRUE),
    Q1 = quantile(Data_Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Data_Value, 0.75, na.rm = TRUE),
    IQR = IQR(Data_Value, na.rm = TRUE)
  )
View(descriptive_stats_stroke)
#create box plots to identify outliers 
ggplot(Utah_data, aes(y = Data_Value)) +
  geom_boxplot(fill = "beige", outlier.color = "red", outlier.shape = 16) +
  labs(title = "Boxplot of Stroke Mortality Rates in Utah Counties",
       y = "Stroke Mortality Rate (per 100,000 population)") +
  theme_minimal()
# top 5 counties with high stroke mortality rate 
top_5_high <- overall_stroke_data %>%
  arrange(desc(Data_Value)) %>%
  head(5)
#top 5 counties with low stroke mortality rate
top_5_low <- overall_stroke_data %>%
  arrange(Data_Value) %>%
  head(5)

#Bar plot 
ggplot(top_5_high, aes(x = reorder(LocationDesc, Data_Value), y = Data_Value)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = round(Data_Value, 1)), vjust = -0.5) +
  labs(title = "Top 5 Counties with High Stroke Mortality Rate",
       x = "County",
       y = "Stroke Mortality Rate (per 100,000)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(top_5_low, aes(x = reorder(LocationDesc, Data_Value), y = Data_Value)) +
  geom_bar(stat = "identity", fill = "#003366") +
  geom_text(aes(label = round(Data_Value, 1)), vjust = -0.5) +
  labs(title = "Top 5 Counties with Low Stroke Mortality Rate",
       x = "County",
       y = "Stroke Mortality Rate (per 100,000)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#visual Skewness 
hist(overall_stroke_data$Data_Value, breaks = 30, col = "sienna", main = "Histogram of Stroke Mortality per Utah County")
#standard deviation for data
sd(overall_stroke_data$Data_Value)
#will stop here with mortality rate 

#2017-2019 Houshold median income 
#2017 Median income 
median_2017_householdincome <- read.csv("/Users/karlyn/Downloads/County_2017_Comparison.csv")
view(median_2017_householdincome)
#2018 Median income 
median_2018_householdincome <- read.csv("/Users/karlyn/Downloads/County_2018_Comparison.csv")
view(median_2018_householdincome)
#2019 Median income 
median_2019_householdincome <- read.csv("/Users/karlyn/Downloads/County_2019_Comparison.csv")
view(median_2019_householdincome)
#double checked all 3, no missing data good to continue 
#combined data for all 3 years, 
combined_3year_income <- bind_rows(median_2017_householdincome, median_2018_householdincome,median_2019_householdincome)
View(combined_3year_income)
#remove dollar sign 
combined_3year_income$Colors <- as.numeric(gsub("[$,]", "", combined_3year_income$Colors))
View(combined_3year_income)

#find descriptive statistics per county 
descriptive_stats_income <-combined_3year_income %>% 
  group_by(X) %>%
  summarise(
    Count = n(),
    Mean = mean(Colors, na.rm = TRUE),
    Median = median(Colors, na.rm = TRUE),
    Mode = as.numeric(names(sort(table(Colors), decreasing = TRUE)[1])),
    SD = sd(Colors, na.rm = TRUE),
    Min = min(Colors, na.rm = TRUE),
    Max = max(Colors, na.rm = TRUE),
    Q1 = quantile(Colors, 0.25, na.rm = TRUE),
    Q3 = quantile(Colors, 0.75, na.rm = TRUE),
    IQR = IQR(Colors, na.rm = TRUE)
  )
view(descriptive_stats_income)

#doing a boxplot
ggplot(combined_3year_income, aes(x = reorder(X, Colors), y = as.numeric(Colors), fill = as.factor(X.1))) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Distribution of Median Household Income by County (2017-2019)",
       x = "County",
       y = "Median Household Income",
       fill = "Year") +
  theme_minimal()
#trying a line chart instead
ggplot(combined_3year_income, aes(x = X.1, y = as.numeric(Colors), group = X)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ X, scales = "free_y") +
  labs(title = "Median Household Income Trends by County (2017-2019)",
       x = "Year",
       y = "Median Household Income") +
  theme_minimal() +
  theme(legend.position = "none") 

#TESTS 

#Linear regression Stroke Mortality vs Median Household Income 
#county names are have different names in their perspective datasetm whill rename to be able to merge
names(overall_stroke_data)[names(overall_stroke_data) == "LocationDesc"] <- "County"
names(combined_3year_income)[names(combined_3year_income) == "X"] <- "County"

#combine stroke and income 
combined_data <- merge(overall_stroke_data, combined_3year_income, by = "County")
#make linear regression 
model_income <- lm(Data_Value ~ Colors, data = combined_data)
#summary of the model 
summary(model_income)

#visualization 
library(ggplot2)
ggplot(combined_data, aes(x = Data_Value, y = Colors)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Linear Regression: Income vs. Stroke Mortality",
       x = "Median Household Income",
       y = "Stroke Mortality Rate") +
  theme_minimal()


#CORRELATION TEST
# Pearson correlation
cor.test(combined_data$Data_Value, combined_data$Colors, method = "pearson")
#Chi test for top 5 high mortality and top 5 low mortalty 
#sort by stroke moratlty 
#top_5_high top 5 counties with high mortality rates
#top_5_low top 5 counties with low mortality rates

