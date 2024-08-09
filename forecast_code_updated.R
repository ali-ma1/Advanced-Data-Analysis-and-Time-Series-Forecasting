# installing packages 
install.packages("tidymodels")
install.packages("modeltime")
install.packages("tseries")
install.packages("stats")
install.packages("zoo")
install.packages("changepoint")
install.packages("lubridate")
install.packages("timetk")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("ggplot2")
install.packages("TSstudio")
install.packages("anomalize")
install.packages("urca")
install.packages("vars")
install.packages("mFilter")
install.packages("anomalize")
install.packages("marima")

# Load libraries ---- 

library(dplyr)
library(ggplot2)
library(tidymodels)
library(modeltime)
library(tseries)
library(stats)
library(zoo)
library(changepoint)
library(lubridate)
library(timetk)
library(dplyr)
library(tidyr)
library(TSstudio)
library(ggplot2)
library(anomalize)
library(marima)
library(dplyr)
library(ggplot2)
library(lubridate)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)

# Loading datasets ----

# Load the dataset with the date column as a character to prevent automatic conversion
sales_sample1 <- read.csv("sample1_sales_timeseries_updated.csv", header=TRUE, sep=",",
                          colClasses=c("integer", "character", "factor", "factor", "numeric", "numeric"))

# fixing date format for every dataset.
sales_sample1$date <- as.Date(sales_sample1$date, format="%m/%d/%Y")


sales_sample2 <- read.csv("sample2_sales_timeseries_updated.csv", header=TRUE, sep=",",
                          colClasses=c("integer", "Date", "factor", "factor", "numeric", "numeric"))
sales_sample2$date <- as.Date(sales_sample1$date, format="%m/%d/%Y")


sales_sample3 <- read.csv("sample3_sales_timeseries_updated.csv", header=TRUE, sep=",",
                          colClasses=c("integer", "Date", "factor", "factor", "numeric", "numeric"))

sales_sample3$date <- as.Date(sales_sample1$date, format="%m/%d/%Y")


sales_sample4 <- read.csv("sample4_sales_timeseries_updated.csv", header=TRUE, sep=",",
                          colClasses=c("integer", "Date", "factor", "factor", "numeric", "numeric"))
sales_sample4$date <- as.Date(sales_sample1$date, format="%m/%d/%Y")



sales_sample5 <- read.csv("sample5_sales_timeseries_updated.csv", header=TRUE, sep=",",
                          colClasses=c("integer", "Date", "factor", "factor", "numeric", "numeric"))
sales_sample5$date <- as.Date(sales_sample1$date, format="%m/%d/%Y")


#Reading data ------------------------------------------------------------------------------
head(sales_sample1,10)
head(sales_sample4,10)
summary(sales_sample1)
str(sales_sample1)
summary(sales_sample2)
str(sales_sample2)
summary(sales_sample3)
str(sales_sample3)
summary(sales_sample4)
str(sales_sample4)
summary(sales_sample5)
str(sales_sample5)

# Preparing data-------------------------------------------------------------------------------

#rbind and sort by date

combined_sales <- rbind(sales_sample1, sales_sample2, sales_sample3, sales_sample4, sales_sample5)

# Sorting combined dataset by date
sorted_sales <- arrange(combined_sales, date)
View(sorted_sales)


#groupby date,family --> sum(sales)
# Grouping by date and family, then summarising to get sum of sales

grouped_sales <- combined_sales %>% 
  group_by(date, family) %>% 
  summarise(total_sales = sum(sales, na.rm = TRUE), .groups = 'drop')

head(grouped_sales)

# Plotting original data
ggplot(grouped_sales, aes(x = date, y = total_sales, color = family)) + 
  geom_line() + 
  ggtitle("Original Sales Data")



#use pivot_wider to format it
wide_sales <- grouped_sales %>%
  pivot_wider(
    names_from = family,      
    values_from = total_sales 
  )
head(wide_sales)


# Pre processing Data ---- 


# Check for missing values across all columns
missing_values <- wide_sales %>% summarise_all(~sum(is.na(.)))
print(missing_values)


# visualization of missing data
wide_sales_long <- wide_sales %>%
  pivot_longer(cols = -date, names_to = "family", values_to = "sales")

ggplot(wide_sales_long, aes(x = date, y = family)) +
  geom_tile(aes(fill = is.na(sales)), color = "white") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "green"),
                    name = "Missing Data",
                    labels = c("Missing" = "Yes", "Not Missing" = "No")) +
  labs(title = "Missing Data Visualization",
       x = "Date",
       y = "Product Family") +
  theme_minimal() # We can tell that we  have no NA's at all


# Checking outliers
# Create a boxplot to visualize outliers for each product family
wide_sales_long <- wide_sales %>%
  pivot_longer(cols = -date, names_to = "family", values_to = "sales")

ggplot(wide_sales_long, aes(x = family, y = sales, fill = family)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Sales by Product Family", x = "Product Family", y = "Sales")



#identifying outliers using  IQR method
identify_outliers <- function(data) {
  q1 <- quantile(data, 0.25)
  q3 <- quantile(data, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  outliers <- data < lower_bound | data > upper_bound
  return(outliers)
}
# Initial Outlier Identification (using existing wide_sales)
outliers <- wide_sales %>%
  mutate(across(where(is.numeric), identify_outliers, .names = "outlier_{.col}"))

# Initial Summary of Outliers
outliers_initial_summary <- outliers %>%
  select(contains("outlier")) %>%
  summarise_all(sum)
print("Initial Outliers Summary:")
print(outliers_initial_summary)

# Function to cap outliers at different percentiles
cap_outliers_adjusted <- function(x, lower = 0.05, upper = 0.95) {
  quantiles <- quantile(x, probs = c(lower, upper), na.rm = TRUE)
  pmax(pmin(x, quantiles[2]), quantiles[1])
}

# Apply the capping function to treat outliers
wide_sales <- wide_sales %>%
  mutate(across(where(is.numeric), cap_outliers_adjusted, lower = 0.05, upper = 0.95))

# Re-identify outliers on the newly capped data
outliers_rechecked <- wide_sales %>%
  mutate(across(where(is.numeric), identify_outliers, .names = "outlier_{.col}"))

# Recheck Summary of Outliers After Treatment
outliers_summary_after_treatment <- outliers_rechecked %>%
  select(contains("outlier")) %>%
  summarise_all(sum)
print("Outliers Summary After Treatment:")
print(outliers_summary_after_treatment)

# Convert wide_sales to long format for plotting
wide_sales_long <- wide_sales %>%
  pivot_longer(
    cols = -date,  # Assuming 'date' is the only non-sales column; adjust if there are more
    names_to = "family",
    values_to = "sales"
  )

# Plotting the boxplot to visualize sales data after outlier treatment
ggplot(wide_sales_long, aes(x = family, y = sales, fill = family)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Sales by Product Family (After Outlier Treatment)",
       x = "Product Family",
       y = "Sales")

#Normalization----------------------------------------------------------------------------------


# Define the normalize function using min-max scaling
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
wide_sales_original <- wide_sales

wide_sales <- wide_sales %>%
  mutate(across(where(is.numeric), normalize))

wide_sales_long <- wide_sales %>%
  pivot_longer(
    cols = -date,  
    names_to = "family",
    values_to = "sales"
  )

# visualize sales data after normalization
ggplot(wide_sales_long, aes(x = family, y = sales, fill = family)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Sales by Product Family (After Normalization)",
       x = "Product Family",
       y = "Sales")



# Density Plot to visualize sales data after normalization
ggplot(wide_sales_long, aes(x = sales, fill = family, color = family)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Sales Distribution by Product Family After Normalization",
       x = "Sales",
       y = "Density")


#----------EDA-------------------------------------------------------------------------------
# Correlation matrix
correlation_matrix <- wide_sales %>%
  select(-date) %>%
  cor()

# Plotting the correlation matrix
corrplot::corrplot(correlation_matrix, method = "color", addCoef.col = "black", tl.col = "black", number.cex = 0.7)


#----------------------------------------Decomposition----------------------------------------------------------------------

library(forecast)

View(wide_sales)

total_sales <- wide_sales %>%
  rowwise() %>%
  mutate(total_sales = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  select(date, total_sales)

# the total_sales column to a time series object
total_sales_ts <- ts(total_sales$total_sales, frequency = 365, start = c(2013, 1))

#Additive
total_sales_decomposed <- decompose(total_sales_ts)
plot(total_sales_decomposed)


#diff=1 remove linear trend
#diff=2 remove quadratic trend 

?diff
adjusted <- diff(total_sales_ts,lag=12,difference=1)
decomp2<- decompose(adjusted) 
plot(decomp2)
checkresiduals(decomp2$random)



adjusted <- diff(total_sales_ts,lag=12,difference=2)
decomp2<- decompose(adjusted) 
plot(decomp2)
checkresiduals(decomp2$random)


#Multiplicative
total_sales_ts %>% decompose(type="multiplicative") %>% plot()
decompose(total_sales_ts)$seasonal

#Decomposition of both 
ts_decompose(total_sales_ts, type = "both")

#------------------------------------------Change point analysis---------------------------------------------


#install.packages("changepoint")
#library("changepoint")
#library(dplyr)

cpt_totalsales<-total_sales_ts %>%
  cpt.mean(
    test.stat = "Normal"
  ) 
cpt_totalsales

plot(cpt_totalsales,          
     cpt.width = 3,        
     main = "Change Points for Total Sales 2013-2017",
     xlab = "Year",
     ylab = "Sales"
)

cpts.ts(cpt_totalsales)

#----------------------------------Checking Seasonality--------------------------------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(forecast)

# Aggregate the total sales by month
total_sales_monthly <- total_sales %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(total_sales = sum(total_sales))

total_sales_ts_monthly <- ts(total_sales_monthly$total_sales, frequency = 12, start = c(year(min(total_sales_monthly$month)), month(min(total_sales_monthly$month))))

# Plot the seasonal plot with the monthly aggregated data
seasonplot(total_sales_ts_monthly, year.labels = TRUE, col = 1:12, main = "Seasonal Plot: Monthly Total Sales")


plot_seasonal <- function(data, family_name) {
  # Aggregate the total sales by month
  sales_monthly <- data %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarise(total_sales = sum(total_sales, na.rm = TRUE))
    sales_ts_monthly <- ts(sales_monthly$total_sales, frequency = 12, start = c(year(min(sales_monthly$month)), month(min(sales_monthly$month))))
    seasonplot(sales_ts_monthly, year.labels = TRUE, col = 1:12, main = paste("Seasonal Plot: Monthly Total Sales -", family_name))
}

poultry_sales <- grouped_sales %>% filter(family == "POULTRY")
liquor_sales <- grouped_sales %>% filter(family == "LIQUOR,WINE,BEER")
meats_sales <- grouped_sales %>% filter(family == "MEATS")
frozen_foods_sales <- grouped_sales %>% filter(family == "FROZEN FOODS")
prepared_foods_sales <- grouped_sales %>% filter(family == "PREPARED FOODS")

plot_seasonal(poultry_sales, "Poultry")
plot_seasonal(liquor_sales, "Liquor, Wine, Beer")
plot_seasonal(meats_sales, "Meats")
plot_seasonal(frozen_foods_sales, "Frozen Foods")
plot_seasonal(prepared_foods_sales, "Prepared Foods")



#--------------------------------Smoothing:-----------------------------------------------------------------------------

####################Total Sales#########################################

plot(total_sales_ts, main = 'Total Sales Simple Moving Average (SMA) ', ylab = 'Sales')

# Add the 10-period SMA
lines(rollmean(total_sales_ts, 10, na.pad = TRUE), col = 'blue')

# Add the 50-period SMA
lines(rollmean(total_sales_ts, 50, na.pad = TRUE), col = 'red')

legend('topright', legend = c('Raw', 'SMA 10', 'SMA 50'), col = c('black', 'blue', 'red'), lty = 1, cex = 0.8)


#--------------------------------Complete EDA ----------------------------------------------------------------

# sales distribution by month

# Convert the wide_sales data to long format and add a month column
wide_sales_long_temporal <- wide_sales %>%
  pivot_longer(
    cols = -date,  # Assuming 'date' is the only non-sales column
    names_to = "family",
    values_to = "sales"
  ) %>%
  mutate(month = format(date, "%m"))

# Plotting the boxplot to visualize monthly sales data by product family
ggplot(wide_sales_long_temporal, aes(x = month, y = sales, fill = family)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Monthly Sales Distribution by Product Family",
       x = "Month",
       y = "Sales")

# Sales distribution by year

# Add a year column to the long format data
wide_sales_long_temporal <- wide_sales_long_temporal %>%
  mutate(year = format(date, "%Y"))

# Plotting the boxplot to visualize yearly sales data by product family
ggplot(wide_sales_long_temporal, aes(x = year, y = sales, fill = family)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Yearly Sales Distribution by Product Family",
       x = "Year",
       y = "Sales")


#--------------------------Poultry--------------------------------------------------------------------------------------------

# Filtering and selecting only POULTRY data
poultry_sales <- grouped_sales %>%
  filter(family == "POULTRY") %>%
  select(date, total_sales) %>%
  arrange(date)

# Convert to a time series object
total_poultry_sales_ts <- ts(poultry_sales$total_sales, frequency = 365, start = c(2013, 1))

#----Decomposition---------------------#

total_poultry_sales_decomposed <- decompose(total_poultry_sales_ts)
plot(total_poultry_sales_decomposed)

# Multiplicative decomposition
total_poultry_sales_decomposed_multiplicative <- decompose(total_poultry_sales_ts, type = "multiplicative")
plot(total_poultry_sales_decomposed_multiplicative)

# Difference and decompose to remove linear and quadratic trends
adjusted_poultry <- diff(total_poultry_sales_ts, lag = 12, differences = 1)
poultry_decomp2 <- decompose(adjusted_poultry)
plot(poultry_decomp2)
checkresiduals(poultry_decomp2$random)

adjusted_poultry <- diff(total_poultry_sales_ts, lag = 12, differences = 2)
poultry_decomp2 <- decompose(adjusted_poultry)
plot(poultry_decomp2)
checkresiduals(poultry_decomp2$random)



#---Change Points----------------------#
cpt_poultry_sales <- cpt.mean(total_poultry_sales_ts, test.stat = "Normal")
plot(cpt_poultry_sales, cpt.width = 3, main = "Change Points for POULTRY Sales 2013-2017", xlab = "Year", ylab = "Sales")
cpts.ts(cpt_poultry_sales)



#------Smoothing: Simple Moving Average (SMA)-------#
plot(total_poultry_sales_ts, main = 'Simple Moving Average (SMA) - POULTRY', ylab = 'Sales')
lines(rollmean(total_poultry_sales_ts, 10, na.pad = TRUE), col = 'blue')
lines(rollmean(total_poultry_sales_ts, 50, na.pad = TRUE), col = 'red')
legend('topright', legend = c('Raw', 'SMA 10', 'SMA 50'), col = c('black', 'blue', 'red'), lty = 1, cex = 0.8)





#---------------------- FROZEN FOODS-------------------------------------------------------------------------------------------

frozenFoods_sales <- grouped_sales %>%
  filter(family == "FROZEN FOODS") %>%
  select(date, total_sales) %>%
  arrange(date)

total_frozenFoods_sales_ts <- ts(frozenFoods_sales$total_sales, frequency = 365, start = c(2013, 1))

#--Decomposition-------------#

# Additive decomposition
total_frozenFoods_sales_decomposed <- decompose(total_frozenFoods_sales_ts)
plot(total_frozenFoods_sales_decomposed)

# Multiplicative decomposition
total_frozenFoods_sales_decomposed_multiplicative <- decompose(total_frozenFoods_sales_ts, type = "multiplicative")
plot(total_frozenFoods_sales_decomposed_multiplicative)

# Difference and decompose to remove linear and quadratic trends
adjusted_frozenFoods <- diff(total_frozenFoods_sales_ts, lag = 12, differences = 1)
frozenFoods_decomp2 <- decompose(adjusted_frozenFoods)
plot(frozenFoods_decomp2)
checkresiduals(frozenFoods_decomp2$random)

adjusted_frozenFoods <- diff(total_frozenFoods_sales_ts, lag = 12, differences = 2)
frozenFoods_decomp2 <- decompose(adjusted_frozenFoods)
plot(frozenFoods_decomp2)
checkresiduals(frozenFoods_decomp2$random)

#----------- change points-----------#
cpt_frozenFoods_sales <- cpt.mean(total_frozenFoods_sales_ts, test.stat = "Normal")
plot(cpt_frozenFoods_sales, cpt.width = 3, main = "Change Points for FROZEN FOODS Sales 2013-2017", xlab = "Year", ylab = "Sales")
cpts.ts(cpt_frozenFoods_sales)

#--------Smoothing: Simple Moving Average (SMA)-----
plot(total_frozenFoods_sales_ts, main = 'Simple Moving Average (SMA) - FROZEN FOODS', ylab = 'Sales')
lines(rollmean(total_frozenFoods_sales_ts, 10, na.pad = TRUE), col = 'blue')
lines(rollmean(total_frozenFoods_sales_ts, 50, na.pad = TRUE), col = 'red')
legend('topright', legend = c('Raw', 'SMA 10', 'SMA 50'), col = c('black', 'blue', 'red'), lty = 1, cex = 0.8)

#-------------------LIQOUR-----------------------------------------------------------------------------------------------------

liquor_sales <- grouped_sales %>%
  filter(family == "LIQUOR,WINE,BEER") %>%
  select(date, total_sales) %>%
  arrange(date)

total_liquor_sales_ts <- ts(liquor_sales$total_sales, frequency = 365, start = c(2013, 1))

#--------Decomposition---------------#

# Additive decomposition
total_liquor_sales_decomposed <- decompose(total_liquor_sales_ts)
plot(total_liquor_sales_decomposed)

# Multiplicative decomposition
total_liquor_sales_decomposed_multiplicative <- decompose(total_liquor_sales_ts, type = "multiplicative")
plot(total_liquor_sales_decomposed_multiplicative)

# Difference and decompose to remove linear and quadratic trends
adjusted_liquor <- diff(total_liquor_sales_ts, lag = 12, differences = 1)
liquor_decomp2 <- decompose(adjusted_liquor)
plot(liquor_decomp2)
checkresiduals(liquor_decomp2$random)

adjusted_liquor <- diff(total_liquor_sales_ts, lag = 12, differences = 2)
liquor_decomp2 <- decompose(adjusted_liquor)
plot(liquor_decomp2)
checkresiduals(liquor_decomp2$random)

#---------- change points---------#
cpt_liquor_sales <- cpt.mean(total_liquor_sales_ts, test.stat = "Normal")
plot(cpt_liquor_sales, cpt.width = 3, main = "Change Points for LIQUOR,WINE,BEER Sales 2013-2017", xlab = "Year", ylab = "Sales")
cpts.ts(cpt_liquor_sales)

#-----------Smoothing: Simple Moving Average (SMA)-------------#
plot(total_liquor_sales_ts, main = 'Simple Moving Average (SMA) - LIQUOR,WINE,BEER', ylab = 'Sales')
lines(rollmean(total_liquor_sales_ts, 10, na.pad = TRUE), col = 'blue')
lines(rollmean(total_liquor_sales_ts, 50, na.pad = TRUE), col = 'red')
legend('topright', legend = c('Raw', 'SMA 10', 'SMA 50'), col = c('black', 'blue', 'red'), lty = 1, cex = 0.8)

#PREPARED FOODS---------------------------------------------------------------------------------------------------

prepared_foods_sales <- grouped_sales %>%
  filter(family == "PREPARED FOODS") %>%
  select(date, total_sales) %>%
  arrange(date)

total_prepared_foods_sales_ts <- ts(prepared_foods_sales$total_sales, frequency = 365, start = c(2013, 1))

# Additive decomposition
total_prepared_foods_sales_decomposed <- decompose(total_prepared_foods_sales_ts)
plot(total_prepared_foods_sales_decomposed)
title(main = "Additive Decomposition - PREPARED FOODS")

# Multiplicative decomposition
total_prepared_foods_sales_decomposed_multiplicative <- decompose(total_prepared_foods_sales_ts, type = "multiplicative")
plot(total_prepared_foods_sales_decomposed_multiplicative)
title(main = "Multiplicative Decomposition - PREPARED FOODS")

# Difference and decompose to remove linear and quadratic trends
adjusted_prepared_foods <- diff(total_prepared_foods_sales_ts, lag = 12, differences = 1)
prepared_foods_decomp2 <- decompose(adjusted_prepared_foods)
plot(prepared_foods_decomp2)
title(main = "First Order Differencing - PREPARED FOODS")
checkresiduals(prepared_foods_decomp2$random)

adjusted_prepared_foods <- diff(total_prepared_foods_sales_ts, lag = 12, differences = 2)
prepared_foods_decomp2 <- decompose(adjusted_prepared_foods)
plot(prepared_foods_decomp2)
title(main = "Second Order Differencing - PREPARED FOODS")
checkresiduals(prepared_foods_decomp2$random)

#--------------change points------------#
cpt_prepared_foods_sales <- cpt.mean(total_prepared_foods_sales_ts, test.stat = "Normal")
plot(cpt_prepared_foods_sales, cpt.width = 3)
title(main = "Change Points for PREPARED FOODS Sales 2013-2017", xlab = "Year", ylab = "Sales")
cpts.ts(cpt_prepared_foods_sales)

#---------Smoothing: Simple Moving Average (SMA)----------------#
plot(total_prepared_foods_sales_ts, main = 'Simple Moving Average (SMA) - PREPARED FOODS', ylab = 'Sales')
lines(rollmean(total_prepared_foods_sales_ts, 10, na.pad = TRUE), col = 'blue')
lines(rollmean(total_prepared_foods_sales_ts, 50, na.pad = TRUE), col = 'red')
legend('topright', legend = c('Raw', 'SMA 10', 'SMA 50'), col = c('black', 'blue', 'red'), lty = 1, cex = 0.8)



#MEATS----------------------------------------------------------------------------------------------------------------

meats_sales <- grouped_sales %>%
  filter(family == "MEATS") %>%
  select(date, total_sales) %>%
  arrange(date)

total_meats_sales_ts <- ts(meats_sales$total_sales, frequency = 365, start = c(2013, 1))

# Additive decomposition
total_meats_sales_decomposed <- decompose(total_meats_sales_ts)
plot(total_meats_sales_decomposed)
title(main = "Additive Decomposition - MEATS")

# Multiplicative decomposition
total_meats_sales_decomposed_multiplicative <- decompose(total_meats_sales_ts, type = "multiplicative")
plot(total_meats_sales_decomposed_multiplicative)
title(main = "Multiplicative Decomposition - MEATS")

# Difference and decompose to remove linear and quadratic trends
adjusted_meats <- diff(total_meats_sales_ts, lag = 12, differences = 1)
meats_decomp2 <- decompose(adjusted_meats)
plot(meats_decomp2)
title(main = "First Order Differencing - MEATS")
checkresiduals(meats_decomp2$random)

adjusted_meats <- diff(total_meats_sales_ts, lag = 12, differences = 2)
meats_decomp2 <- decompose(adjusted_meats)
plot(meats_decomp2)
title(main = "Second Order Differencing - MEATS")
checkresiduals(meats_decomp2$random)

#--------------change points---------------------#
cpt_meats_sales <- cpt.mean(total_meats_sales_ts, test.stat = "Normal")
plot(cpt_meats_sales, cpt.width = 3)
title(main = "Change Points for MEATS Sales 2013-2017", xlab = "Year", ylab = "Sales")
cpts.ts(cpt_meats_sales)

#--------------Smoothing: Simple Moving Average (SMA)----------------
plot(total_meats_sales_ts, main = 'Simple Moving Average (SMA) - MEATS', ylab = 'Sales')
lines(rollmean(total_meats_sales_ts, 10, na.pad = TRUE), col = 'blue')
lines(rollmean(total_meats_sales_ts, 50, na.pad = TRUE), col = 'red')
legend('topright', legend = c('Raw', 'SMA 10', 'SMA 50'), col = c('black', 'blue', 'red'), lty = 1, cex = 0.8)

#Anomaly Detection----------------------------------------------------------------------------------------------------

#install.packages("anomalize")
# Load the library
#library(anomalize)


# Anomaly detection
anomaly_detection <- wide_sales_long %>%
  group_by(family) %>%
  anomalize::anomalize(sales, method = "gesd")

ggplot(anomaly_detection, aes(x = date, y = sales, color = anomaly)) +
  geom_point() +
  facet_wrap(~ family, scales = "free_y") +
  theme_minimal() +
  labs(title = "Anomaly Detection in Sales by Product Family",
       x = "Date",
       y = "Sales")



#--------------------Forcast the external factors-----------------------------------------------------------------------------------------

# the date range 
earthquake_date <- as.Date("2016-04-16")
start_date <- as.Date("2016-01-01")
end_date <- as.Date("2016-12-31")

sales_earthquake <- wide_sales %>%
  filter(date >= start_date & date <= end_date)

monthly_sales <- sales_earthquake %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

ggplot(monthly_sales, aes(x = month)) +
  geom_line(aes(y = POULTRY, color = "Poultry")) +
  geom_line(aes(y = MEATS, color = "Meats")) +
  geom_line(aes(y = `LIQUOR,WINE,BEER`, color = "Liquor, Wine, Beer")) +
  geom_line(aes(y = `FROZEN FOODS`, color = "Frozen Foods")) +
  geom_line(aes(y = `PREPARED FOODS`, color = "Prepared Foods")) +
  geom_vline(xintercept = as.numeric(floor_date(earthquake_date, "month")), linetype = "dashed", color = "red") +
  labs(title = "Monthly Sales Data Around the Earthquake Period (2016)",
       x = "Month",
       y = "Normalized Sales",
       color = "Product Family") +
  theme_minimal()


#----------------------Aggregate-----------------------------------------------------------------------------------------------------

# Total sales per year

plot(aggregate(total_sales_ts,FUN=sum),
     xlab = "Year",
     ylab = "sales") 

plot(aggregate(total_sales_ts,FUN=mean),
     xlab = "Year",
     ylab = "sales") 


# Aggregate POULTRY 
plot(aggregate(total_poultry_sales_ts, FUN = sum), xlab = "Year", ylab = "Sales")
plot(aggregate(total_poultry_sales_ts, FUN = mean), xlab = "Year", ylab = "Sales")

# Aggregate FROZEN FOODS 
plot(aggregate(total_frozenFoods_sales_ts, FUN = sum), xlab = "Year", ylab = "Sales")
plot(aggregate(total_frozenFoods_sales_ts, FUN = mean), xlab = "Year", ylab = "Sales")

# Aggregate LIQUOR,WINE,BEER 
plot(aggregate(total_liquor_sales_ts, FUN = sum), xlab = "Year", ylab = "Sales", main = "Annual Total Sales - LIQUOR,WINE,BEER")
plot(aggregate(total_liquor_sales_ts, FUN = mean), xlab = "Year", ylab = "Sales", main = "Annual Average Sales - LIQUOR,WINE,BEER")


# Aggregate PREPARED FOODS 
plot(aggregate(total_prepared_foods_sales_ts, FUN = sum), xlab = "Year", ylab = "Sales", main = "Annual Total Sales - PREPARED FOODS")
plot(aggregate(total_prepared_foods_sales_ts, FUN = mean), xlab = "Year", ylab = "Sales", main = "Annual Average Sales - PREPARED FOODS")


#Aggregate Meats 
plot(aggregate(total_meats_sales_ts, FUN = sum), xlab = "Year", ylab = "Sales", main = "Annual Total Sales - MEATS")
plot(aggregate(total_meats_sales_ts, FUN = mean), xlab = "Year", ylab = "Sales", main = "Annual Average Sales - MEATS")


#--------------VAR---------------------------------------------------------------------------------------------

dev.off() # This closes the current graphics device

sales_ts <- ts(wide_sales[,-1], start = c(2013, 1), frequency = 365)

autoplot(sales_ts, facets = TRUE) + 
  labs(title = "Sales Time Series by Product Family", x = "Time", y = "Sales")

plot.ts(sales_ts)

# Phillips-Perron Unit Root Test- univariate time series
pp_results <- apply(sales_ts, 2, function(x) pp.test(x))
pp_pvalues <- sapply(pp_results, function(x) x$p.value)
print(pp_pvalues)


# Augmented Dickey-Fuller (ADF) 
adf_results <- apply(sales_ts, 2, function(x) adf.test(x))
adf_pvalues <- sapply(adf_results, function(x) x$p.value)
print(adf_pvalues)


# Regression for common product families - MEATS and POULTRY
meats <- sales_ts[, "MEATS"]
poultry <- sales_ts[, "POULTRY"]

#  OLS regression model
ols_model <- lm(poultry ~ meats)
summary(ols_model)

# Plot 
plot(meats, poultry, main = "Regression of POULTRY on MEATS", xlab = "MEATS", ylab = "POULTRY")
abline(ols_model, col = "red")

# Diagnostic plots 
par(mfrow = c(2, 2))
plot(ols_model)



#determine the persistence of the model (each)
acf(sales_ts)
pacf(sales_ts)

#combined ACF PACF
combined_sales <- rowMeans(sales_ts, na.rm = TRUE)
combined_sales_ts <- ts(combined_sales, start = c(2013, 1), frequency = 365)
par(mfrow = c(1, 2))  
acf(combined_sales_ts, main = "ACF of Combined Sales")
pacf(combined_sales_ts, main = "PACF of Combined Sales")

# a matrix for VAR
var_data <- cbind(sales_ts[, "MEATS"], sales_ts[, "POULTRY"], sales_ts[, "LIQUOR,WINE,BEER"], sales_ts[, "PREPARED FOODS"], sales_ts[, "FROZEN FOODS"])
colnames(var_data) <- c("MEATS", "POULTRY", "LIQUOR.WINE.BEER", "PREPARED.FOODS", "FROZEN.FOODS")


# optimal lags
lagselect <- VARselect(var_data, lag.max = 10, type = "const")

optimal_lag <- lagselect$selection 
optimal_lag

#  VAR model using the optimal lag
ModelSales <- VAR(var_data, p = 10, type = "const", season = NULL, exog = NULL)
summary(ModelSales)


# Serial correlation test
Serial1 <- serial.test(ModelSales, lags.pt = 12, type = "PT.asymptotic")
print(Serial1)


# Heteroscedasticity test
Arch1 <- arch.test(ModelSales, lags.multi = 12, multivariate.only = TRUE)
print(Arch1)


# Normal distribution of residuals test
Norm1 <- normality.test(ModelSales, multivariate.only = TRUE)
print(Norm1)


# Jarque-Bera test 
jarque_bera_test <- apply(residuals(ModelSales), 2, function(x) jarque.bera.test(x))
jb_pvalues <- sapply(jarque_bera_test, function(x) x$p.value)
print(jb_pvalues)


# Kurtosis and Skewness tests
kurtosis_test <- apply(residuals(ModelSales), 2, function(x) e1071::kurtosis(x))
skewness_test <- apply(residuals(ModelSales), 2, function(x) e1071::skewness(x))

print("Kurtosis:")
print(kurtosis_test)

print("Skewness:")
print(skewness_test)

# Structural breaks in the residuals
Stability1 <- stability(ModelSales, type = "OLS-CUSUM")
plot(Stability1)


# check if a variable has a causal relationship with others
GrangerPOULTRY <- causality(ModelSales, cause = "POULTRY")
print(GrangerPOULTRY)

GrangerMEATS <- causality(ModelSales, cause = "MEATS")
print(GrangerMEATS)

GrangerFROZENFOODS <- causality(ModelSales, cause = "FROZEN.FOODS")
print(GrangerFROZENFOODS)

GrangerPREPAREDFOODS <- causality(ModelSales, cause = "PREPARED.FOODS")
print(GrangerPREPAREDFOODS)

GrangerLIQUOR <- causality(ModelSales, cause = "LIQUOR.WINE.BEER")
print(GrangerLIQUOR)

# Impulse Response Functions
POULTRYirf <- irf(ModelSales, impulse = "MEATS", response = "POULTRY", n.ahead = 20, boot = TRUE)
plot(POULTRYirf, ylab = "POULTRY", main = "Shock from MEATS")

MEATSirf <- irf(ModelSales, impulse = "POULTRY", response = "MEATS", n.ahead = 20, boot = TRUE)
plot(MEATSirf, ylab = "MEATS", main = "Shock from POULTRY")

FROZENFOODSirf <- irf(ModelSales, impulse = "PREPARED.FOODS", response = "FROZEN.FOODS", n.ahead = 20, boot = TRUE)
plot(FROZENFOODSirf, ylab = "FROZEN FOODS", main = "Shock from PREPARED FOODS")

PREPAREDFOODSirf <- irf(ModelSales, impulse = "POULTRY", response = "PREPARED.FOODS", n.ahead = 20, boot = TRUE)
plot(PREPAREDFOODSirf, ylab = "PREPARED FOODS", main = "Shock from POULTRY")

LIQUORirf <- irf(ModelSales, impulse = "POULTRY", response = "LIQUOR.WINE.BEER", n.ahead = 20, boot = TRUE)
plot(LIQUORirf, ylab = "LIQUOR,WINE,BEER", main = "Shock from POULTRY")

#analyze the contribution of shocks in explaining forecast error variances
FEVD1 <- fevd(ModelSales, n.ahead = 10)
print(FEVD1)
plot(FEVD1)

# Forecasting using VAR
forecast <- predict(ModelSales, n.ahead = 12, ci = 0.95)
fanchart(forecast, names = "POULTRY", main = "Fanchart for POULTRY", xlab = "Horizon", ylab = "POULTRY Sales")
fanchart(forecast, names = "MEATS", main = "Fanchart for MEATS", xlab = "Horizon", ylab = "MEATS Sales")
fanchart(forecast, names = "FROZEN.FOODS", main = "Fanchart for FROZEN FOODS", xlab = "Horizon", ylab = "FROZEN FOODS Sales")
fanchart(forecast, names = "PREPARED.FOODS", main = "Fanchart for PREPARED FOODS", xlab = "Horizon", ylab = "PREPARED FOODS Sales")
fanchart(forecast, names = "LIQUOR.WINE.BEER", main = "Fanchart for LIQUOR,WINE,BEER", xlab = "Horizon", ylab = "LIQUOR,WINE,BEER Sales")


#-------------------Multivariate timeseries forcasting-----------------------------------------------------------


# Load necessary libraries
#install.packages("marima")
#library(marima)
#library(ggplot2)

# Extract the date column as a vector
date_column <- wide_sales$date

# Convert the normalized data to a matrix
normalized_sales <- as.matrix(wide_sales[, -1])
original_sales <- as.matrix(wide_sales_original[, -1])

max_values <- apply(wide_sales_original[, -1], 2, max)
min_values <- apply(wide_sales_original[, -1], 2, min)

k <- ncol(normalized_sales)

# the MARIMA model using define.model
model_def <- define.model(kvar = k, ar = 2, ma = 2)

# MARIMA model on the entire dataset
marima_model <- marima(ts(normalized_sales), model_def$ar.pattern, model_def$ma.pattern, penalty = 1)

# Check the structure of the marima_model object
print(str(marima_model))

residuals_all <- marima_model$residuals[, 1:nrow(normalized_sales)]
residuals_all <- t(residuals_all)

# Ensure the residuals are aligned correctly with the entire data
if (all(dim(residuals_all) == dim(normalized_sales))) {
  fitted_values <- normalized_sales - residuals_all
} else {
  stop("Residuals dimensions do not match the data dimensions.")
}

# Denormalize the fitted values
denormalize <- function(x, max_val, min_val) {
  return(x * (max_val - min_val) + min_val)
}

denormalized_fitted <- t(apply(fitted_values, 1, function(col) {
  mapply(denormalize, col, max_values, min_values)
}))

# Create a list of data frames for plotting the denormalized fitted values against the actual values for each family
family_names <- colnames(wide_sales)[-1]
fitted_df_list <- lapply(1:ncol(denormalized_fitted), function(i) {
  data.frame(Date = date_column, 
             Family = family_names[i], 
             Fitted = denormalized_fitted[, i], 
             Actual = denormalize(normalized_sales[, i], max_values[i], min_values[i]))
})

# remove any rows with NA values 
fitted_df_list <- lapply(fitted_df_list, function(df) {
  df[complete.cases(df), ]
})

# Plotting the fit on the entire data for each family separately
for (i in 1:length(fitted_df_list)) {
  fitted_df <- fitted_df_list[[i]]
  p <- ggplot(fitted_df, aes(x = Date)) +
    geom_line(aes(y = Fitted, color = "Fitted")) +
    geom_line(aes(y = Actual, color = "Actual")) +
    labs(title = paste("Model Fit vs Actual on Entire Data for", fitted_df$Family[1]),
         x = "Date",
         y = "Values") +
    theme_minimal() +
    scale_color_manual(values = c("Fitted" = "blue", "Actual" = "red"))
  print(p)
}

# Forecast for the next 15 days after the entire dataset
nstep <- 15  # Number of steps ahead for forecasting

# Ensure the series has enough length to accommodate the forecast
extended_series <- rbind(normalized_sales, matrix(NA, nrow = nstep, ncol = k))

# Perform the forecast
forecast_results <- arma.forecast(series = ts(extended_series), marima = marima_model, nstart = nrow(normalized_sales), nstep = nstep)

# Extract the forecast values for the next 15 days
Predict <- forecast_results$forecasts[, (nrow(normalized_sales) + 1):(nrow(normalized_sales) + nstep)]

# Ensure that the dimensions match expected values
if (nrow(Predict) != k || ncol(Predict) != nstep) {
  stop("Forecast dimensions do not match the expected length.")
}

# Transpose the Predict matrix to match the expected format (nstep x k)
Predict <- t(Predict)

# Denormalize the Predict values for all families
denormalized_predict <- t(apply(Predict, 1, function(col) {
  mapply(denormalize, col, max_values, min_values)
}))

# Create a list of data frames for plotting the last 100 days and the forecasted next 15 days
last_100_days <- tail(wide_sales, 100)
last_100_dates <- last_100_days$date
last_100_data <- as.matrix(last_100_days[, -1])

# Denormalize the last 100 days data
denormalized_last_100 <- t(apply(last_100_data, 1, function(col) {
  mapply(denormalize, col, max_values, min_values)
}))

forecast_dates <- seq(max(date_column) + 1, by = "day", length.out = nstep)

for (i in 1:ncol(denormalized_last_100)) {
  last_100_df <- data.frame(Date = last_100_dates, 
                            Family = family_names[i], 
                            Sales = denormalized_last_100[, i],
                            Type = "Actual")
  
  forecast_df <- data.frame(Date = forecast_dates, 
                            Family = family_names[i], 
                            Sales = denormalized_predict[, i],
                            Type = "Forecast")
  
  combined_df <- rbind(last_100_df, forecast_df)
  
  # Remove rows with NA or extreme values in the combined data frame
  combined_df <- combined_df[complete.cases(combined_df), ]
  
  tail(combined_df, 20)
  
  p <- ggplot(combined_df, aes(x = Date, y = Sales, color = Type)) +
    geom_line() +
    labs(title = paste("Last 100 Days and 15-Day Forecast for", family_names[i]),
         x = "Date",
         y = "Values") +
    theme_minimal() +
    scale_color_manual(values = c("Forecast" = "blue", "Actual" = "black"))
  print(p)
}

# Alternatively, forecasting using Auto Arima to show different ways its done.

# Perform auto.arima and forecast for each family
for (i in 1:ncol(normalized_sales)) {
  ts_data <- ts(normalized_sales[, i], frequency = 365)
  arima_model <- auto.arima(ts_data)
  forecast_arima <- forecast(arima_model, h = nstep)
  
  # Denormalize the forecasted values
  denormalized_forecast_arima <- denormalize(forecast_arima$mean, max_values[i], min_values[i])
  
  # Create a data frame for the last 100 days and the forecasted next 15 days
  last_100_df <- data.frame(Date = last_100_dates, 
                            Family = family_names[i], 
                            Sales = denormalized_last_100[, i],
                            Type = "Actual")
  
  forecast_dates <- seq(max(date_column) + 1, by = "day", length.out = nstep)
  forecast_df <- data.frame(Date = forecast_dates, 
                            Family = family_names[i], 
                            Sales = denormalized_forecast_arima,
                            Type = "Forecast")
  
  combined_df_auto_arima <- rbind(last_100_df, forecast_df)
  
  # Remove rows with NA or extreme values in the combined data frame
  combined_df_auto_arima <- combined_df_auto_arima[complete.cases(combined_df), ]
  
  p <- ggplot(combined_df_auto_arima, aes(x = Date, y = Sales, color = Type)) +
    geom_line() +
    labs(title = paste("Last 100 Days and 15-Day Forecast using auto.arima for", family_names[i]),
         x = "Date",
         y = "Values") +
    theme_minimal() +
    scale_color_manual(values = c("Forecast" = "blue", "Actual" = "black"))
  print(p)
}

#Adding the multivariate Arima forecasts to a csv file and saving it.

# Create a data frame for the entire original dataset with a 'Type' column set to "Actual"
entire_data <- data.frame(Date = date_column, Family = rep(family_names, each = length(date_column)), Sales = as.vector(t(original_sales)), Type = "Actual")

for (i in 1:ncol(denormalized_predict)) {
  forecast_df <- data.frame(Date = forecast_dates, Family = family_names[i], Sales = denormalized_predict[, i], Type = "Forecast")
  
  entire_data <- rbind(entire_data, forecast_df)
}

# Remove rows with NA values in the combined data frame
entire_data <- entire_data[complete.cases(entire_data), ]

# Group by date and arrange by family names
entire_data <- entire_data %>%
  group_by(Date) %>%
  arrange(Family, .by_group = TRUE) %>%
  ungroup()

# Save the combined data to a CSV file
write.csv(entire_data, "combined_sales_forecast.csv", row.names = FALSE)