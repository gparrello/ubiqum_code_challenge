# ---------------------------------- #
# R Programming Challenge            #
# Ubiqum Code Academy                #
# 10.03.2019 - Version 1             #
# ---------------------------------- #

# Exercise 1. Perform an exploratory analysis of Barcelona's air quality for January & February of 2019.
# Download the data here: https://opendata-ajuntament.barcelona.cat/data/en/dataset/qualitat-aire-detall-bcn
  # 1.1. Does the pollution level depend on the time of the day?
  # 1.2. Analyse the differences between locations
  # 1.3. What's the most problematic pollutant?

pacman::p_load(
  "data.table",
  "ggplot2",
  "caret"
)

df_01 <- fread("./data/012019.csv")
df_02 <- fread("./data/022019.csv")
df <- rbind(df_01, df_02)
rm(df_01, df_02)

# According to metadata: dataTime is "Time Stamp de l'hora de descàrrega del fitxer"
# That doesn't make sense, timestamps show dates corresponding to Jan and Feb 2019.
# I'm treating them as datetime for original data.

df$hour <- unclass(as.POSIXlt(df$dateTime, origin="1970-01-01"))$hour
df$qualitat_aire <- as.factor(df$qualitat_aire)
df$qualitat_o3 <- as.factor(df$qualitat_o3)
df$qualitat_no2 <- as.factor(df$qualitat_no2)
df$qualitat_pm10 <- as.factor(df$qualitat_pm10)
df$valor_o3 <- as.numeric(gsub(" µg/m³", "", df$valor_o3))
df$valor_no2 <- as.numeric(gsub(" µg/m³", "", df$valor_no2))
df$valor_pm10 <- as.numeric(gsub(" µg/m³", "", df$valor_pm10))
df$hora_o3 <- as.numeric(gsub("h", "", df$hora_o3))
df$hora_no2 <- as.numeric(gsub("h", "", df$hora_no2))
df$hora_pm10 <- as.numeric(gsub("h", "", df$hora_pm10))

## 1.1:
# The number of records for each class of air pollution level (Pobra, Regular, Bona)
# is very unequal to conclude much

plot1 <- ggplot(data = df) +
  aes(x = qualitat_aire) +
  geom_bar(fill = '#0c4c8a') +
  theme_minimal() +
  facet_wrap(vars(nom_cabina))

# That being said, for each cabin doing the measurements, there are some variations
# for class "Regular" between hours. Not the case for other classes:

plot2 <- ggplot(data = df) +
  aes(x = qualitat_aire, y = hour) +
  geom_violin(scale = 'area', adjust = 1, fill = '#0c4c8a') +
  theme_minimal() +
  facet_wrap(vars(nom_cabina))

# I realize this is wrong if I check hour only. A finer-grade analysis is needed to determine
# differences by time.
# In any case, there are fluctuations by hour for two different measurements: o3, no2:

# o3 fluctuates a lot by hour, dipping at the end of the day:
plot_o3 <- ggplot(data = df) +
  aes(x = hora_o3, weight = valor_o3) +
  geom_bar(fill = '#0c4c8a') +
  theme_minimal()

# no2 also fluctuates by hour:
plot_no2 <- ggplot(data = df) +
  aes(x = hora_no2, weight = valor_no2) +
  geom_bar(fill = '#0c4c8a') +
  theme_minimal()

# pm10 remains steady during the whole day:
plot_pm10 <- ggplot(data = df) +
  aes(x = hora_pm10, weight = valor_pm10) +
  geom_bar(fill = '#0c4c8a') +
  theme_minimal()

# What remains to be seen is the impact of each of these in the final air quality evaluation.
### WARNING: Not removing NAs is imputed as hour 0?!


## 1.2:

# For o3, there are differences in the fluctuation by hour and levels between locations:
# - Sants has the highest levels of o3
# - Gracia, Eixample and Ciutadella have the lowest
plot_o3_loc <- ggplot(data = df) +
  aes(x = hora_o3, weight = valor_o3) +
  geom_bar(fill = '#0c4c8a') +
  theme_minimal() +
  facet_wrap(vars(nom_cabina))

# For no2, the fluctuation by hour seems to be similar for all locations:
# peaks at 00hs and between 19 and 21hs.
# The difference is in the levels measured:
# - Sants the lowest levels of no2, followed by Vall Hebron and Palau Reial.
# - The rest all show similar higher levels.
plot_no2_loc <- ggplot(data = df) +
  aes(x = hora_no2, weight = valor_no2) +
  geom_bar(fill = '#0c4c8a') +
  theme_minimal() +
  facet_wrap(vars(nom_cabina))

# Although pm10 is steady everywhere, the levels for each location (cabin)
# are different:
# - Ciutadella and Sants do not measure this value.
# - Observ Fabra and Palau Reial show lower levels of pm10
# - Eixample, Gracia and Poblenou have higher levels of pm10
# - Vall Hebron is in between both groups
plot_pm10_loc <- ggplot(data = df) +
  aes(x = hora_pm10, weight = valor_pm10) +
  geom_bar(fill = '#0c4c8a') +
  theme_minimal() +
  facet_wrap(vars(nom_cabina))


# Reflecting upon the findings, there seems to be an inverse correlation between o3, and no2 and pm10
# meaning: the lowest the o3, the highest the pollution in no2 and pm10


## 1.3:
# To determine the most important pollutant, we could do a RF to predict qualitat_aire
# and use VarImp to check which of the three is more influential: qualitat_03, qualitat_no2 or qualitat_pm10

x <- df[, c("qualitat_o3", "qualitat_no2", "qualitat_pm10", "valor_o3", "valor_no2", "valor_pm10")]
y <- df$qualitat_aire

### THIS IS WHERE I WAS WORKING AT DEADLINE
# Fails for NAs in predictors!
rf <- train(
  x = x,
  y = y,
  trControl = trainControl(method = "oob"),
  importance = TRUE,
  verbose = TRUE
)

# Not yet tested!
model <- rf$finalModel
varImp(model)
importance(model)

### THIS IS WHERE I LEFT OF


# Exercise 2. Given a dataframe with two columns, "timestamp" and "x", write a function that returns 
# x aggregated by a chosen measure (mean, mode...) and a chosen time unit (day, month...)

fDoThings_WeekDay <- function(df) {
  
  pacman::p_load(
    "dplyr"
  )
  
  # calculate mode
  # from: https://www.tutorialspoint.com/r/r_mean_median_mode.htm
  mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  # from: https://stackoverflow.com/questions/1962278/dealing-with-timestamps-in-r
  df$grouping <- unclass(as.POSIXlt(df$timestamp))$wday
  
  output <- df %>% 
    group_by(grouping) %>%
    summarize(
      x_mean = mean(x),
      x_mode = mode(x),
      x_min = min(x),
      x_max = max(x)
    )
    
  return(output)  
  
}


fDoThings_Year <- function(df) {
  
  pacman::p_load(
    "dplyr"
  )
  
  # calculate mode
  # from: https://www.tutorialspoint.com/r/r_mean_median_mode.htm
  mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  # from: https://stackoverflow.com/questions/1962278/dealing-with-timestamps-in-r
  df$grouping <- unclass(as.POSIXlt(df$timestamp))$year
  
  output <- df %>% 
    group_by(grouping) %>%
    summarize(
      x_mean = mean(x),
      x_mode = mode(x),
      x_min = min(x),
      x_max = max(x)
    )
    
  return(output)  
  
}

# Test your function with this data
set.seed(888)
timestamp = seq(from = ISOdate(2018,11,15), to = ISOdate(2019,1,15), by = "min")
x = rnorm(length(timestamp))
mydf <- data.frame(timestamp, x)


# Aggregate by week day, return mean of x
#     time_gr    x_agg
#1       1     0.0124 
#2       2    -0.00918
#3       3    -0.00725
#4       4    -0.00951
#5       5     0.00117
#6       6     0.00845
#7       7     0.00959

test1 <- fDoThings_WeekDay(mydf)

# Aggregate by year, return max of x
#    time_gr  x_agg
#1    2018    4.24
#2    2019    4.21

test2 <- fDoThings_Year(mydf)

