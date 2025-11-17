# The following code was prepared as part of a HarvardX Datascience Captstone 
# project and is intended to create a machine learning model that can predict
# residential water use in gpcd based on readily available data.

# ----Load-and-create-dataset----

# Identify settings and load libraries
options(digits = 5)
options(pillar.sigfig = 5)

if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(openxlsx)) install.packages("openxlsx", 
                                     repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", 
                                         repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", 
                                     repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", 
                                     repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", 
                                     repos = "http://cran.us.r-project.org")

library(tidyverse)
library(openxlsx)
library(caret)
library(lubridate)
library(broom)
library(knitr)
library(randomForest)

# ----CARDS-dataset----
# Load CaRDS data: Monthly water supply and demand time series for 404 water suppliers 
# in California (2013-2021)
water_data <- read.csv("CaRDS.csv")
supplier_data <- read.csv("Supplier_Info.csv")

dim(water_data)
head(water_data)
unique(water_data$PWSID)
unique(water_data$Variable)

dim(supplier_data)
head(supplier_data)

# ----Reshape-and-join-data----

# Reformat water data so that variables are columns and dated records are rows
water_data_long <- pivot_longer(water_data, "X2013.01.01":"X2021.12.01") 
Water_data_wide <- pivot_wider(water_data_long, names_from = Variable, 
                               values_from = value)

# Adjust date column to remove "X" and day
Water_data_wide <- Water_data_wide |>
  mutate(name = str_remove(name, "^X")) |>  # Remove the leading "X"
  separate(name, into = c("year", "month", "day"), sep = "\\.") 

# Recreate date column that is recognized by R as a date 
Water_data_wide$Date <- as.Date(
  paste(Water_data_wide$year, Water_data_wide$month, "01", sep = "-"))

# Join supplier data to demand data
water_data_supplier <- left_join(Water_data_wide, supplier_data, by = "PWSID")

# ----Normalize-water-use----
# Create column for normalized demand (gallons per capita per day)
water_data_supplier <- water_data_supplier |>
  mutate(days_in_month = days_in_month(ymd(Date)),
    gpcd = (demand * 0.264172) / (Population_21 * days_in_month)
  ) |> ungroup()

# Filter out rows with negative or 0 demand or population values, and include 
# only large water systems (size is LWS)
data_filtered <- water_data_supplier |> 
  filter(demand > 0, Population_21 > 0, size == "LWS")

data_filtered <- data_filtered |>
  mutate(gpcd = as.numeric(gpcd))  # Ensure gpcd is numeric

min(data_filtered$gpcd)
max(data_filtered$gpcd)

# ----Filter-gpcd----
# Filter out gpcd values outside of lowest and highest residential gpcds 
# identified by the Pacific Institute (rounded to the nearest 10 gpcd)
# https://pacinst.org/new-data-show-residential-per-capita-water-use-across-california/
lower_bound <- 40
upper_bound <- 590

data_filtered_clean <- data_filtered |> 
  filter(gpcd >= lower_bound & gpcd <= upper_bound)
max(as.numeric(data_filtered_clean$gpcd), na.rm = TRUE)
min(as.numeric(data_filtered_clean$gpcd), na.rm = TRUE)

# ----Load-and-filter-SAFER----
# Load California State Water Resources Control Board (SWRCB) 2022 SAFER dataset
SAFER <- read.xlsx("2022risk.xlsx", sheet = "Afford. Raw Data Summary (1)", 
                   startRow = 2)

# Filter SAFER file for median household income (MHI). 
MHI <- SAFER |> select(PWSID, Weighted.Average.MHI2) |> 
  rename(MHI = Weighted.Average.MHI2)

# Join MHI dataset to the data_filtered_clean dataset. Replace missing values 
# with California statewide MHI of $78,672 
# (US Census, 2020 American Community Survey 5-Year Estimates) 
data_filtered_clean <- left_join(data_filtered_clean,MHI, by = "PWSID") 

data_filtered_clean <- data_filtered_clean |>
  mutate(MHI = na_if(MHI, "N/A")) |>
  mutate(MHI = as.numeric(MHI)) |>
  mutate(MHI = replace_na(MHI, 78672))

# ----COVID----
# Incorporate the State of California COVID stay at home order period as a factor 
# (March 2020 to June 2021)
# https://www.gov.ca.gov/wp-content/uploads/2021/06/6.11.21-EO-N-07-21-signed.pdf
data_filtered_clean <- data_filtered_clean |>
  group_by(year, month) |>
    mutate(COVID = if_else(
    (year == 2020 & month >= 3) | (year == 2021 & month <= 6),
    1, 0
  )) |>
  ungroup()

# ----Transform-fields----
# Convert Hydrologic.Region to a factor.
data_filtered_clean$Hydrologic.Region <- as.factor(
  data_filtered_clean$Hydrologic.Region)

# Convert Climate.Zone to a factor.
data_filtered_clean$Climate.Zone <- as.factor(
  data_filtered_clean$Climate.Zone)

# Convert County to a factor.
data_filtered_clean$County <- as.factor(
  data_filtered_clean$County)

# Convert month to a factor.
data_filtered_clean$County <- as.factor(
  data_filtered_clean$County)

# Transform precipitation to reduce potential impacts of extreme precipitation periods
data_filtered_clean$precipitation_log <- log(data_filtered_clean$precipitation+1)

# ----Filtered-data-summary----
dim(data_filtered_clean)
data_filtered_clean |> summarise(unique_count = n_distinct(PWSID))


# ----Explore-dataset----

# ----Explore-demand-hydrologic-region----
# Boxplot of demand by hydrologic region
ggplot(data_filtered_clean, aes(y = Hydrologic.Region, x = demand)) +
  geom_boxplot(fill = "lightblue", color = "gray", outlier.shape = NA) +
  scale_x_continuous(limits = c(0, 800000000)) +
  labs(title = "Total Monthly Residential Demands by Hydrologic Region",
       x = "Total Residential Demand by Month (liters)",
       y = "Hydrologic Region")

# Boxplot of water demand in gallons per capita per day by hydrologic region
ggplot(data_filtered_clean, aes(y = Hydrologic.Region, x = gpcd)) +
  geom_boxplot(fill = "lightblue", color = "gray", outlier.shape = NA) +
  scale_x_continuous(limits = c(0, 300))+
  labs(title = "Normalized Residential Demand by Hydrologic Region",
       x = "Normalized Residential Demand (GPCD)",
       y = "Hydrologic Region")

# Line plot of monthly water demand by hydrologic region
ggplot(data_filtered_clean, aes(x = Date, y = demand, 
                                color = Hydrologic.Region)) +
  geom_line() +
  labs(title = "Monthly Residential Demand by Hydrologic Region",
       x = "Date",
       y = "Total Demand by Month (liters)")

# Line plot of annual water demand by hydrologic region
annual_demand <- data_filtered_clean |>
  group_by(Hydrologic.Region, year) |>
  summarise(annual_demand = sum(demand, na.rm = TRUE)) |>
  mutate(year = as.numeric(year)) |>
  ungroup()

ggplot(annual_demand, aes(x = year, y = annual_demand, 
                          color=Hydrologic.Region)) +
  geom_line() +
  labs(title = "Annual Residential Demand by Hydrologic Region",
       x = "Date",
       y = "Total Residential Demand by Year (liters)")

# ----Explore-demand-climate-zone----
# Boxplot of demand by climate zone
ggplot(data_filtered_clean, aes(y = Climate.Zone, x = demand)) +
  geom_boxplot(fill = "lightblue", color = "gray", outlier.shape = NA) +
  scale_x_continuous(limits = c(0, 800000000)) +
  labs(title = "Total Monthly Residential Demands by Climate Zone",
       x = "Total Residential Demand by Month (liters)",
       y = "Climate Zone")

# Boxplot of water demand in gallons per capita per day by climate zone
ggplot(data_filtered_clean, aes(y = Climate.Zone, x = gpcd, group = Climate.Zone)) +
  geom_boxplot(fill = "lightblue", color = "gray", outlier.shape = NA) +
  scale_x_continuous(limits = c(0, 300)) +
  labs(title = "Normalized Residential Demand by Climate Zone",
       x = "Normalized Residential Demand (GPCD)",
       y = "Climate Zone")

# Line plot of monthly water demand by climate zone
ggplot(data_filtered_clean, aes(x = Date, y = demand, 
                                color = Climate.Zone)) +
  geom_line() +
  labs(title = "Monthly Residential Demand by Climate Zone",
       x = "Date",
       y = "Total Residential Demand by Month (liters)")

# Line plot of annual water demand by Climate Zone
annual_demand <- data_filtered_clean |>
  group_by(Climate.Zone, year) |>
  summarise(annual_demand = sum(demand, na.rm = TRUE)) |>
  mutate(year = as.numeric(year)) |>
  ungroup()

ggplot(annual_demand, aes(x = year, y = annual_demand, 
                          color = Climate.Zone)) +
  geom_line() +
  labs(title = "Annual Residential Demand by Climate Zone",
       x = "Date",
       y = "Total Residential Demand by Year (liters)")

# ----Explore-climate-precip----
# Line plot of monthly precipitation by climate zone
ggplot(data_filtered_clean, aes(x = Date, y = precipitation, 
                                color = Climate.Zone)) +
  geom_line() +
  labs(title = "Precipitation by Climate Zone Time Series",
       x = "Date",
       y = "Precipitation (mm)") 

ggplot(data_filtered_clean, aes(x = precipitation, y = Climate.Zone)) +
  geom_boxplot(fill = "lightgreen", color = "darkgray") +
  labs(title = "Precipitation by Climate Zone",
         x = "Precipitation (mm)") 

# ----Explore-climate-temp----
# Line plot of monthly temperature by hydrologic region
ggplot(data_filtered_clean, aes(x = Date, y = temperature, 
                                color = Climate.Zone)) +
  geom_line() +
  labs(title = "Temperature by Climate Zone Time Series",
       x = "Date",
       y = "Temperature (degrees C)") 

# Boxplot of monthly temperature by hydrologic region
ggplot(data_filtered_clean, aes(x = temperature, y = Climate.Zone)) +
  geom_boxplot(fill = "blue", color = "darkgray") +
  labs(title = "Temperature by Climate Zone",
       x = "Temperature (degrees C)") 

# ----Temp-vs-precip----
# Plot precipitation versus temperature by climate zone
ggplot(data_filtered_clean, aes(x = temperature, y = precipitation, 
                                color = Climate.Zone)) +
  geom_point() +
  scale_y_continuous(trans = "log10") +
  labs(title = "Temperature versus Precipitation by Climate Zone",
       x = "Temperature (degrees C)",
       y = "Precipitation (mm)") 

# ----Explore-PDSI----
# Line plot of PDSI by month by climate zone
ggplot(data_filtered_clean, aes(x = Date, y = PDSI, color = Climate.Zone)) +
  geom_line() +
  labs(title = "Palmder Drought Severity Index Time Series by Climate Zone",
       x = "Date",
       y = "PDSI") 

# Boxplot of PDSI by climate zone
ggplot(data_filtered_clean, aes(x = PDSI, y = Climate.Zone, group = Climate.Zone)) +
  geom_boxplot(fill = "purple", color = "darkgray") +
  labs(title = "Palmder Drought Severity Index  by Climate Zone",
       x = "PDSI") 

# ----Explore-month----
# Boxplot of gpcd by month
ggplot(data_filtered_clean, aes(x = gpcd, y = month, group = month)) +
  geom_boxplot(fill = "darkred", color = "darkgray", outlier.shape = NA) +
  scale_x_continuous(limits = c(25, 125)) +
  labs(title = "Normalized Demand by Month",
       x = "Normalized Demand (GPCD)", y = "Month") 

# ----Explore-MHI----
# Plot of gpcd versus median household income
ggplot(data_filtered_clean, aes(x = MHI, y = gpcd)) +
  geom_point(fill = "pink", color = "darkgray") +
  labs(title = "Normalized Demand versus MHI",
       x = "MHI", y = "Normalized Demand (GPCD)") 

# ----Split-training-test-sets----
set.seed(1999, sample.kind="Rounding")
test_index <- createDataPartition(
  y = data_filtered_clean$demand, times = 1, p = 0.2, list = FALSE)
train_set <- data_filtered_clean[-test_index,]
test_set <- data_filtered_clean[test_index,]


# ----ANOVAs----
# Conduct ANOVA analysis to determine how differect factors affect demand in gpcd
aov_result_region <- aov(gpcd ~ Hydrologic.Region, data = train_set)

aov_result_temp <- aov(gpcd ~ temperature, data = train_set)

aov_result_precip <- aov(gpcd ~ precipitation, data = train_set)

aov_result_month <- aov(gpcd ~ month, data = train_set)

aov_result_year <- aov(gpcd ~ year, data = train_set)

aov_result_zone <- aov(gpcd ~ Climate.Zone, data = train_set)

aov_result_COVID <- aov(gpcd ~ COVID, data = train_set)

aov_result_MHI <- aov(gpcd ~ MHI, data = train_set)

aov_result_County <- aov(gpcd ~ County, data = train_set)

aov_summary <- bind_rows(
  tidy(aov_result_region) |> filter(term != "Residuals") |> 
    mutate(variable = "Hydrologic.Region"),
  tidy(aov_result_temp) |> filter(term != "Residuals") |> 
    mutate(variable = "Temperature"),
  tidy(aov_result_precip) |> filter(term != "Residuals") |> 
    mutate(variable = "Precipitation"),
  tidy(aov_result_month) |> filter(term != "Residuals") |> 
    mutate(variable = "Month"),
  tidy(aov_result_year) |> filter(term != "Residuals") |> 
    mutate(variable = "Year"),
  tidy(aov_result_zone) |> filter(term != "Residuals") |> 
    mutate(variable = "Climate Zone"),
  tidy(aov_result_COVID) |> filter(term != "Residuals") |> 
    mutate(variable = "COVID"),
  tidy(aov_result_MHI) |> filter(term != "Residuals") |> 
    mutate(variable = "MHI"),
  tidy(aov_result_County) |> filter(term != "Residuals") |> 
    mutate(variable = "County")
)

aov_summary_mutated <- aov_summary |> 
  mutate(f_value = statistic, p_value = p.value,
         signif = case_when(p.value < 0.001 ~ "***", 
                            p.value < 0.01  ~ "**",
                            p.value < 0.05  ~ "*", 
                            p.value < 0.1   ~ ".", 
                            TRUE~ "")) |>
  select(variable, df, sumsq, meansq, f_value, p_value, signif) |>
  arrange(desc(sumsq))

# ----ANOVAs-results----
aov_summary_mutated |> 
  kable(caption = "ANOVA Results")

# ----Develop-models----
# Develop machine learning models

# ----GLM----
# Train and test glm model 
train_glm <- train(
  gpcd ~ Hydrologic.Region + Climate.Zone + year + COVID + MHI + month + County + temperature, 
  method = "glm", 
  data = train_set)

y_hat_glm <- predict(train_glm, test_set, type = "raw")

# ----GLM-results----
postResample(pred = y_hat_glm, obs = test_set$gpcd)


ggplot(data = data.frame(Observed = test_set$gpcd, Predicted = y_hat_glm),
       aes(x = Observed, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(
    title = "Predicted vs Observed GPCD (GLM Model)",
    x = "Observed GPCD",
    y = "Predicted GPCD"
  )


# ----KNN----
# Train and test knn model
train_knn <- train(
  gpcd ~ Hydrologic.Region + Climate.Zone + year + COVID + MHI + month + County + temperature, 
  method = "knn",
  data = train_set,
  tuneGrid = data.frame(k = seq(2, 18, 2)))
train_knn$bestTune
train_knn$finalModel

y_hat_knn <- predict(train_knn, test_set, type = "raw")

# ----KNN-results----
postResample(pred = y_hat_knn, obs = test_set$gpcd)
suppressWarnings(ggplot(train_knn, highlight = TRUE))

ggplot(data = data.frame(Observed = test_set$gpcd, Predicted = y_hat_knn),
       aes(x = Observed, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(
    title = "Predicted vs Observed GPCD (KNN Model)",
    x = "Observed GPCD",
    y = "Predicted GPCD"
  )

# ----RF----
# Train and test random forest model
tune_grid <- expand.grid(mtry = seq(30, 50, 5))

train_rf <- train(
 gpcd ~ Hydrologic.Region + Climate.Zone + year + COVID + MHI + month + County + temperature,
 method = "rf",
 data = train_set,
 trControl = trainControl(method = "cv", number = 5, allowParallel = TRUE),
 tuneGrid = tune_grid,
 ntree = 1000
)

y_hat_rf<- predict(train_rf, test_set, type = "raw")

# ----RF-results----
postResample(pred = y_hat_rf, obs = test_set$gpcd)

ggplot(data = data.frame(Observed = test_set$gpcd, Predicted = y_hat_rf),
       aes(x = Observed, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(
    title = "Predicted vs Observed GPCD (RF Model)",
    x = "Observed GPCD",
    y = "Predicted GPCD"
  )

# ----RF-model-summary----
# View the best tuning parameters
train_rf$bestTune

# Plot model performance across tuning parameters
plot(train_rf,
     main = "Random Forest Model Error Rate",
     xlab = "Number of Trees",
     ylab = "Mean Squared Error")


# View the final Random Forest model details
train_rf$finalModel

# Check variable importance
varImp(train_rf)

# Detailed summary of the model
summary(train_rf$finalModel)

plot(test_set$gpcd, y_hat_rf)

# ----RF-detailed-results----
# Define bins
test_set$gpcd_bin <- cut(test_set$gpcd,
                         breaks = c(0, 100, 200, 300, Inf),
                         labels = c("0-100", "100-200", "200-300", "300+"),
                         include.lowest = TRUE)

# Combine predictions with bins
results <- data.frame(obs = test_set$gpcd,
                      pred = y_hat_rf,  
                      bin = test_set$gpcd_bin)

# # Scatter plot by bin for random forest model results
# ggplot(results, aes(x = obs, y = pred, color = bin)) +
#   geom_point(alpha = 0.5) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   facet_wrap(~ bin) +
#   labs(title = "Predicted vs Observed GPCD by Range (Random Forest Model)",
#        x = "Observed GPCD", y = "Predicted GPCD") 

# Box plot by bin for random forest model results
results$error <- results$pred - results$obs

ggplot(results, aes(x = bin, y = error, fill = bin)) +
  geom_boxplot() +
  labs(title = "Prediction Error by GPCD Range",
       x = "GPCD Range", y = "Prediction Error") +
  theme_minimal()

results |>
  group_by(bin) |>
  summarise(RMSE = sqrt(mean((pred - obs)^2)),
            MAE = mean(abs(pred - obs)),
            R2 = cor(pred, obs)^2,
            .groups = "drop")

