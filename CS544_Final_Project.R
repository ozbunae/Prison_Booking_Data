#Term Project
#Andrew Ozbun
#CS544 Spring 1 2022
#02/24/2022

#Downloading libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(sampling)
library(gridExtra)
library(RColorBrewer)


options(scipen = 999)

####CHANGE THIS TO COME FROM THE WORKING DIRECTORY
#read in CSV
df <- read.csv("inmate_data.csv")

head(df)
nrow(df)

##Data Cleaning -----------------------------------------------------------
# #I decided not to use this because it gets rid of too much data!!!
# #getting rid of rows with empty strings
# df1 <- df[rowSums(df == "")==0, , drop = FALSE]
# df1
# nrow(df1)

#Checking for Null values
#There are 924
sum(is.na(df))
#Since there are plenty of observations, lets drop those rows
inmate_df <- na.omit(df)
#checking that nas were dropped
sum(is.na(inmate_df))

head(inmate_df)
nrow(inmate_df)

#Checking Data types
#Note that Dates and Times are in character and integer, respectively
sapply(inmate_df, class)

#initial EDA ----------------------------------------------------------
#looking at arrest age as numerical attribute
#come back to fix x lims
hist(inmate_df$age_at_arrest,
     xlim = c(10, 90),
     ylim = c(0, 20000),
     xlab = "Arrest Age",
     main = "Histogram of Age at Arrest",
     breaks = seq(from = 15, to = 85, by = 5),
     col = "dodgerblue4")

boxplot(inmate_df$age_at_arrest,
        main = "Distribution of Arrest Age",
        horizontal = TRUE,
        col = "cyan4")

#get names of all attributes
colnames(inmate_df)

#inmates who spent more than a month in prison
rows  <- which(inmate_df$days_in_jail > 30)
hist(inmate_df[rows, "days_in_jail"],
     xlim = c(0, 400),
     ylim = c(0, 3500))

dfover30 <- inmate_df[inmate_df$days_in_jail > 30, ]

#number of inmates who are staying in jail over 30 days
inmates_over30 <- nrow(inmate_df[rows, ])
inmates_over30
#number of total inmates
total_inmates <- nrow(inmate_df)
total_inmates
#inmates staying in jail less than 30 days
inmates_under30 <-  total_inmates - inmates_over30
inmates_under30

#percent of inmates staying over 30 days
percent_over30 <- inmates_over30/total_inmates
percent_over30
#percent of inmates staying under 30 days
percent_under30 <- inmates_under30/total_inmates
percent_under30

#df in jail for 30 days or less
dfnot30 <- inmate_df[inmate_df$days_in_jail < 30, ]
dfnot30
#df in jail for 500 hours or less
dfnot500hr <- dfnot30[dfnot30$hours < 500, ]
dfnot500hr

#number of white prisoners in the df
white <- nrow(inmate_df[which(inmate_df$race == "White"), ])
white
#number of black prisoners in the df
black <- nrow(inmate_df[which(inmate_df$race == "Black"), ])
black

#basic histograms
hist(dfnot500hr$hours,
     xlim = c(0, 500))

hist(inmate_df$hours)

#Examining unique values of categorical variables

#Where they went to H.S.
unique(inmate_df$school)
#How much education
unique(inmate_df$education_status)
#are they employed
unique(inmate_df$employment_status)
#what crime did they commit
unique(inmate_df$crime)
#what is their race
unique(inmate_df$race)
#what is the level of offence
unique(inmate_df$offense_level)
#male or female
unique(inmate_df$sex)
#did they serve in the military
unique(inmate_df$military)
#there is an extra column and this is the same as marital statues
unique(inmate_df$maritial_status)
#married single, ext.
unique(inmate_df$marital_status)
#offense level
#Possibly look at misdemeanor v. Felony
unique(inmate_df$offense_level)
#short descriptions, i.e. sex crime, robbery ext.
unique(inmate_df$superhighlevel)


#plotting categorical variables----

#sex of inmates
barplot(table(inmate_df$sex),
        ylim = c(0, 60000),
        main = "Frequency of Inmates by Sex",
        col = c("coral4", "cornflowerblue") )

employment_df <- inmate_df[which(inmate_df$employment_status != ""), ]
#employed or unemployed
palette1 <- brewer.pal(7, "BuPu")
barplot(table(employment_df$employment_status),
        col = coul,
        ylim = c(0,35000),
        main = "Employment Status of Inmates Booked")

#First looks at racial distribution
race_df <- inmate_df[which(inmate_df$race != ""), ]
palette2 <- brewer.pal(7, "Spectral")
barplot(table(race_df$race),
        ylim = c(0, 40000),
        col = palette2,
        main = "Race of Inmates Booked")

#Looking at Military status
military_df <- inmate_df[which(inmate_df$military != ""), ]
palette3 <- brewer.pal(7, "Dark2")
barplot(table(military_df$military),
        ylim = c(0, 70000),
        col = palette3,
        main = "Inmates who served in the Military")

#marital status 
marital_df <- inmate_df[which(inmate_df$marital_status != ""), ]
palette4 <- brewer.pal(7, "RdGy")
barplot(table(marital_df$marital_status),
        ylim = c(0, 60000),
        col = palette4,
        main = "Marital Status of Inmates Booked")

##Time Series -----------------------------------------------------------

#changing data type to date
inmate_df$booking_date <- as.Date(inmate_df$booking_date, "%m/%d/%Y")
sapply(inmate_df, class)
#data frame of dates with counts
date_table <- as.data.frame(table(inmate_df$booking_date))
head(date_table)
sapply(date_table, class)

#saving booking date as vector
inc_date <- as.Date(unique(inmate_df$booking_date))
#frequencies as vector
date_counts <- date_table$Freq
#data frame
#note -- when saved as vector data type of date
#becomes factor.  Try to make more efficient?
date_freq <- data.frame(inc_date, date_counts)
head(date_freq)
sapply(date_freq, class)

# Basic Time Series Line Plot
ts1 <- ggplot(date_freq, aes(x=inc_date, y=date_counts)) +
  geom_line() + 
  xlab("Year") +
  ylab("Number of Bookings per Day")
ts1

#Breakdown by year, month ext.

#create year and month and day columns
date_freq <- date_freq %>% mutate(month = month(inc_date))
head(date_freq)
date_freq <- date_freq %>% mutate(year = year(inc_date))
head(date_freq)
date_freq <- date_freq %>% mutate(days = weekdays(inc_date))
head(date_freq)

#creating sample time series for 2012
bookings2012 <- date_freq[which(date_freq$year == 2012), ]
nrow(bookings2012)
head(bookings2012)

p2012 <- ggplot(bookings2012, aes(x=inc_date, y=date_counts)) +
  geom_line() + 
  xlab("Year") +
  ylab("Number of Bookings per Day")

p2012

book2012tibble <- as_tibble(bookings2012)
book2012tibble

months_2012 <- book2012tibble %>%
  group_by(month) %>%
  summarise(mean_booking = mean(date_counts, na.rm = TRUE))
months_2012

ggplot(months_2012, aes(x = month, y = mean_booking)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = c(1:12), labels = c(month.abb))

weekdays_2012 <- book2012tibble %>%
  group_by(days) %>%
  summarise(mean_days = mean(date_counts, na.rm = TRUE))
weekdays_2012

##NOT IN WORKING ORDER
weekdays_2012$days <- factor(weekdays_2012$days, 
                             levels = c("Sunday",
                                        "Monday",
                                        "Tuesday",
                                        "Wednesday",
                                        "Thursday",
                                        "Friday",
                                        "Saturday"))
weekdays_2012 <- weekdays_2012[order(weekdays_2012$days), ]

ggplot(weekdays_2012, aes(x = days, y = mean_days)) +
  geom_bar(stat = "identity")

##CREATING DATA AS TIBBLE
tibble_data <- as_tibble(date_freq)
tibble_data
tibble_data <- tibble_data[which(tibble_data$year != 2018), ]

weekdays <- tibble_data %>%
  group_by(days) %>%
  summarise(mean_days = mean(date_counts, na.rm = TRUE))
weekdays
weekdays$days <- factor(weekdays$days, 
                             levels = c("Sunday",
                                        "Monday",
                                        "Tuesday",
                                        "Wednesday",
                                        "Thursday",
                                        "Friday",
                                        "Saturday"))
weekdays <- weekdays[order(weekdays$days), ]
weekdays

ggplot(weekdays, aes(x = days, y = mean_days)) +
  geom_bar(stat = "identity",
           fill = "#B24F60") +
  ylim(0, 35) +
  labs(title = "Average Bookings \nBy Day of the Week",
       x = "Weekday",
       y = "Average Number of Bookings")


#For loop to break up data by year and plot by month
tib_list <- list()
for (i in unique(tibble_data$year)){
  
  bd <- tibble_data[which(tibble_data$year == i), ]
  
  d <- bd %>%
    group_by(month) %>%
    summarise(mean_booking = mean(date_counts, na.rm = TRUE))
  
  tib_list[[quo_name(i)]] <- ggplot(d, aes(x = month, y = mean_booking)) +
    geom_point(color = "deeppink2") +
    geom_line() +
    labs(title = i, 
         x = "Month",
         y = "Average Daily Bookings")+
    ylim(18, 40)+
    scale_x_continuous(breaks = c(1:12), labels = c(month.abb))
  
}

grid.arrange(grobs = tib_list,
             top = "Monthly Booking Data \nYear over Year")

#Using Forecasting Package ------
library(fpp2)

ts_tibble <- aggregate(date_counts ~ month + year , 
                       tibble_data , 
                       mean )

ts_tibble

TS <- ts(ts_tibble[,3], 
         start = c(2012, 1), 
         end = c(2017, 12),
         frequency = 12)
TS
autoplot(TS) +
  ggtitle("Time Series: Average Monthly Prison Bookings") +
  ylab("Bookings per Day")

#Differencing
DTS <- diff(TS)
autoplot(DTS) +
  ggtitle("Time Series: Average Monthly Prison Bookings") +
  ylab("Bookings per Day")


#Seasonality
ggseasonplot(DTS) +
  ggtitle("Seasonal Plot of Differenced Data") +
  ylab("Bookings per Day")

ggsubseriesplot(DTS)

#ARIMA model
#d = differencing D = seasonal difference
fit_arima <- auto.arima(TS, 
                        d = 1, 
                        D = 1,
                        stepwise = FALSE, #don't need to save time
                        approximation = FALSE,
                        trace = TRUE)#print models
print(summary(fit_arima))
checkresiduals(fit_arima)

sqrt(4.468)
#sd is 2.113764

#Forecasting
forecst <- forecast(fit_arima, h = 24)
autoplot(forecst) +
  ggtitle("Time Series Forecast of Daily Bookings") +
  ylab("Bookings Per Day") +
  xlab("Year")


##Normal Distribution ------------------------------------------------------
arrest_age <- inmate_df$age_at_arrest
fit = density(arrest_age)
#Age at arrest Distribution
ggplot(inmate_df, aes(x = arrest_age))+
  geom_histogram(aes(y= ..density..), binwidth = 5, color = "black", fill = "seagreen3") +
  geom_density(fill = "blue", alpha = .2)

vline <- function(x = 0, color = "3B5281") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color, dash = "dash")
  )
}

#Age at arrest Distribution using PLOTLY
fig <- plot_ly(x = arrest_age, 
               type = "histogram",
               name = "Histogram")%>%
  add_trace(data = fit,#add trace will create overlay of density
            x = fit$x, 
            y = fit$y,
            mode = "lines",#creates outside line
            type = "scatter",#needed or produces error
            fill = "tozeroy", 
            yaxis = "y2", 
            inherit = FALSE, #doesn't inherit attributes of first fig
            name = "Density")%>%
  #adding in arguments for second graph
  layout(legend = list(orientation = "h"),#change legend to bottom
         yaxis2 = list(overlaying = "y", 
                       side = "right",
                       showticklabels = FALSE,#get rid of second label
                       showgrid = FALSE))#get rid of second gridline
#labels and tick values
fig <- layout(fig, xaxis = list(title = "Age at Arrest", #create tick
                                range= c(15, 85),
                                dtick = 10,#bin width of tick
                                tickmode = "linear"),
                                #tickangle = 45),
              yaxis = list(title = "Frequency",
                           range = c(0, 4500)),
              title = "Distribution of Age at Arrest",
              shapes = list(vline(mean(arrest_age))))

fig

#days in jail distribution
#people who spent more than a month in jail
ggplot(inmate_df[rows, ], aes(x = days_in_jail))+
  geom_histogram(aes(y= ..density..), binwidth = 5, color = "black", fill = "white") +
  geom_density(fill = "blue", alpha = .2)

#Age Distribution by Group
#dropping empty strings from the data set
df2<-inmate_df[(inmate_df$race != ""),]
head(df2)
nrow(df2)

#df where high level crime does not equal an empty string
df3 <- dfover30[(dfover30$superhighlevel != ""), ]
head(df3)
nrow(df3)

#can I change palette for this?
ggplot(data = df2, aes(fill = race, x = age_at_arrest)) +
  geom_density( alpha = 0.2) + xlab("Arrest Age")+
  ylab("Probability Density Function") +
  labs(title = "Distribution of Arrest Age \nBy Race") +
  ylim(0, 0.09) +
  scale_fill_discrete(name = "Race")

#Histograms of Race distribution
ggplot(df2, aes(x = age_at_arrest)) +
  geom_histogram(aes(fill = factor(race)), binwidth = 5) +
  facet_grid(race ~., scales = "free") +
  scale_fill_brewer(palette = "BrBG", name = "Race") +
  labs(title = "Histograms of Arrest Age \nSorted by Race",
       y = "Frequency",
       x = "Age at Arrest")+
  theme(strip.text = element_blank())#takes away y2 labels

#Boxplots of age at arrest by race
p <- plot_ly(df2, 
             x = ~age_at_arrest, 
             color = ~race, 
             type = "box",
             boxpoints = "outliers") %>%
  layout(xaxis = list(title = "<b> Age at Arrest <b>"),
         legend = list(title = list(text = '<b> Race </b>')),
         title = "<b> Quartile Summary of Age at Arrest \nBy Race <b>")
p

#days in jail v. crime
hist(df3$days_in_jail)
p2 <- plot_ly(df3, 
             x = ~days_in_jail, 
             color = ~superhighlevel, 
             type = "box",
             boxpoints = "outliers") %>%
  layout(title = "Quartile Summary of Days in Jail \nBy Offense",
         legend = list(title = list(text = "Offense")),
         xaxis = list(title = "Days in Jail"))
p2

mu <- mean(df2$age_at_arrest)
sigma <- sd(df2$age_at_arrest)

mu
sigma

#Central Limit Theorem -------------------------------------------------------
#Histogram of Booking Age with Means resampling size 10

set.seed(6232)

samples <- 5000
sample_size <- 10

xbar <- numeric(samples)

for (i in 1: samples) {
  xbar[i] <- mean(sample(df2$age_at_arrest, sample_size, replace = FALSE))
}

fit2 <- density(xbar)
hist(xbar, prob = TRUE)

histo1 <- plot_ly(x = xbar,
                   type = "histogram",
                   histnorm = "probability",
                   name = "Histogram",
                   opacity = 0.7,
                   marker = list(color = "#67032F"))%>%
  layout(yaxis = list(range = c(0, 0.07),
                      title = "Probability Mass Function"),
         title = "Histogram of Booking Ages",
         xaxis = list(range = c(18, 45),
                      title = "Booking Age"),
         shapes = list(vline(mean(xbar)))) %>%
  add_lines(data = fit2,#add trace will create overlay of density
            x = fit2$x, 
            y = fit2$y,
            yaxis = "y2", 
            inherit = FALSE, #doesn't inherit attributes of first fig
            name = "Density",
            marker = list("red")) %>%
  layout(legend = list(orientation = "h"),#change legend to bottom
         yaxis2 = list(overlaying = "y", 
                       side = "right",
                       showticklabels = FALSE,#get rid of second label
                       showgrid = FALSE,#get rid of second gridline
                       rangemode = "tozero"))
histo1

####DO THIS FOR ALL?????
confidence_interval <- function(vector, interval) {
  # Standard deviation of sample
  vec_sd <- sd(vector)
  # Sample size
  n <- length(vector)
  # Mean of sample
  vec_mean <- mean(vector)
  # Error according to t distribution
  error <- qt((interval + 1)/2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  result <- c(vec_mean - error, vec_mean + error)
  return(result)
}
ci1 <- confidence_interval(df2$age_at_arrest, .65)

mu <- mean(xbar)
sigma <- sd(xbar)

ci1

mu
sigma



#Sample Size Subplot ----------------------------------------------------------

samples <- 1000

xbar2 <- numeric(samples)

# Examine different sample sizes
par(mfrow = c(2,2))

for (size in c(10, 20, 30, 40)) {
  for (i in 1:samples) {
    xbar2[i] <- mean(sample(df2$age_at_arrest, size, replace = FALSE))
  }
  
  hist(xbar2, 
       prob = TRUE, 
       xlim=c(18, 45), 
       ylim = c(0, .25),
       col = "cyan4",
       main = paste("Sample Size =", size),
       xlab = "Age at Arrest")
  
  cat("Sample Size = ", size, " Mean = ", mean(xbar2),
      " SD = ", sd(xbar2), "\n")
}

par(mfrow = c(1,1))



###Different Sample sizes using plotly
samples <- 1000

xbar2 <- numeric(samples)
p_list <- list()
p <- plot_ly()

sample_sizes <- c(10, 20, 30, 40)

for (size in sample_sizes) {
  
  for (i in 1:samples) {
    xbar2[i] <- mean(sample(df2$age_at_arrest, size, replace = FALSE))
  }
  p_list[[quo_name(size)]] <- add_trace(p, 
                 x = xbar2,
                 type = "histogram",
                 opacity = 0.5,
                 name = paste("sample size =", size))%>%
    layout(xaxis = list(range = c(20, 40)), 
           yaxis = list(range = c(0, 140)))
}

plotly_subs <- subplot(p_list, nrows = (length(sample_sizes)/2))%>%
  layout(title = list(text = "Distribution of Arrest Age \nFour Sample Sizes"),
         margin = 0.05)

plotly_subs



#SRS w/ out Replacement ------------------------------------------------------
#to sample size.

set.seed(6232)
n <-  100 

s <- srswor(n, nrow(df2))
s[s != 0]

rows <- (1:nrow(df2))[s!=0]
rows <- rep(rows, s[s != 0])
rows

sample_srs <- df2[rows, ]
nrows(sample_srs)

srswor_df <- as.data.frame(table(sample_srs$race), )
colnames(srswor_df) <- c("Race", "Freq")
srswor_df$Percent <- c((srswor_df$Freq/n)*100)
srswor_df

race_plots <- plot_ly(srswor_df,
                        x = ~Race,
                        y = ~Freq,
                        type = "bar",
                        name = "Bar Chart Frequencies",
                        marker = list(color = "#AA4371"))%>%
  layout(title = "Simple Random Sampling Without Replacement",
         yaxis = list(title = "Frequencies",
                      range = c(0, 25)),
         xaxis = list(domain = c(0, 0.5)))%>%
  add_trace(srswor_df,
            labels = ~Race, 
            values = ~Freq, 
            type = 'pie',
            domain = list(x = c(0.5, 1)),
            marker = list(colors = c("#556677", "#AA3344", "#772200", 
                                     "#11AA22", "#AA231B88")))%>% 
  layout(title = 'Frequency of Races <br>Simple Random Sampling<br><br>')

race_plots    

mu_srs <- mean(sample_srs$age_at_arrest)
sigma_srs <- sd(sample_srs$age_at_arrest)

mu_srs
sigma_srs




#Systematic Sampling ----------------------------------------------------------

set.seed(6232)
n <- 100

# Basic Systematic
# #number of rows in the data
# N <- nrow(df2)
# N
# #creating interval size
# k <- ceiling(N / n)
# k
# 
# r <- sample(k, 1)
# r
# select every kth item
# s <- seq(r, by = k, length = n)

# Unequal Probabilities Systematic
pick <- inclusionprobabilities(df2$age_at_arrest, n)
length(pick)
sum(pick)

systematic <- UPsystematic(pick)


sample_sys <- df2[systematic != 0, ]
head(sample_sys)

table_sys <- table(sample_sys$race)
table_sys
ptable_sys <- prop.table(table_sys)

df_sys <- as.data.frame(table_sys, )
colnames(df_sys) <- c("Race", "Freq")
df_sys$Percent <- c((df_sys$Freq/n)*100)
df_sys


sys_plots <- plot_ly(df_sys,
                        x = ~Race,
                        y = ~Freq,
                        type = "bar",
                        name = "Bar Chart Frequencies",
                        marker = list(color = "#AA4371"))%>%
  layout(yaxis = list(title = "Frequencies",
                      range = c(0, 25)),
         xaxis = list(domain = c(0, 0.5)))%>%
  add_trace(df_sys,
            labels = ~Race, 
            values = ~Freq, 
            type = 'pie',
            domain = list(x = c(0.5, 1)),
            marker = list(colors = c("#556677", "#AA3344", "#772200", 
                                     "#11AA22", "#AA231B88")))%>% 
  layout(title = 'Frequency of Races <br>Systematic Sampling<br><br>')

sys_plots

mu_sys <- mean(sample_sys$age_at_arrest)
sigma_sys <- sd(sample_sys$age_at_arrest)

mu_sys
sigma_sys


#Stratified Sampling ---------------------------------------------------------

row.names(df2) <- 1:nrow(df2)
#double check nas are gone
df2 <- na.omit(df2)

set.seed(6232)
n <- 100
#I had to only use races that were more than one percent of the total data
data_strata <- df2[df2$race == "Black" | 
                df2$race == "White" |
                df2$race == "Hispanic", ]
head(data_strata)
#check to make sure the df turned out correctly
unique(data_strata$race)

#order the df by groups
order.index <- order(data_strata$race)
data <- data_strata[order.index,]
#create frequency table
freq <- table(data_strata$race)
freq
#assign amounts to groups and check if it adds to 50
st_sizes <- n * freq / sum(freq)
st_sizes
sum(st_sizes)

#run strata
sample_strat <- strata(data,
                       stratanames = c("race"),
                       size = st_sizes,
                       method = "srswor",
                       description = TRUE)

st <- getdata(data, sample_strat)
st


table_strat <- table(sample_strat$race)
table_strat
perc_sample_strat <- prop.table(table_strat)
perc_sample_strat


df_strata <- as.data.frame(table_strat )
colnames(df_strata) <- c("Race", "Freq")
df_strata$Percent <- c(df_strata$Freq/n*100)
df_strata

strata_plots <- plot_ly(df_strata,
                        x = ~Race,
                        y = ~Freq,
                        type = "bar",
                        name = "Bar Chart Frequencies",
                        marker = list(color = "#AA4371"))%>%
  layout(yaxis = list(title = "Frequencies",
                      range = c(0, 25)),
         xaxis = list(domain = c(0, 0.5)))%>%
  add_trace(df_strata,
            labels = ~Race,
            values = ~Freq,
            type = 'pie',
            domain = list(x = c(0.5, 1)),
            marker = list(colors = c("#556677", "#AA3344", "#772200",
                                     "#11AA22", "#AA231B88")))%>%
  layout(title = 'Frequency of Races <br>Stratified Sampling<br><br>')

strata_plots

strata_mu <- mean(st$age_at_arrest)
strata_sigma <- sd(st$age_at_arrest)

strata_mu
strata_sigma

#Sampling Histograms----------------------------------------------------------

#SRSWOR
plot1 <- plot_ly(x = sample_srs$age_at_arrest,
                 type = "histogram",
                 name = "SRSWOR",
                 marker = list(color = "#895775")) %>%
  layout(yaxis = list(title = "Frequencies",
                      range = c(0, 30)),
         xaxis = list(title = "Age at Arrest"))

#Systematic Sampling
plot2 <- plot_ly(x = sample_sys$age_at_arrest,
                 type = "histogram",
                 name = "Systematic",
                 marker = list(color = "#756857")) %>%
  layout(yaxis = list(title = "Frequencies",
                      range = c(0, 30)),
         xaxis = list(title = "Age at Arrest"))

#Stratified Sampling
plot3 <- plot_ly(x = st$age_at_arrest,
                 type = "histogram",
                 name = "Stratified",
                 marker = list(color = "#9A918D")) %>%
  layout(yaxis = list(title = "Frequencies",
                      range = c(0, 30)),
         xaxis = list(title = "Age at Arrest"))

#Actual Data
plot4 <- plot_ly(x = df2$age_at_arrest,
                 type = "histogram",
                 name = "Raw Data",
                 marker = list(color = "#B85067")) %>%
  layout(yaxis = list(title = "Frequencies",
                      range = c(0, 4500)),
         xaxis = list(title = "Age at Arrest"))

sampling_fig <- subplot(plot1, plot2, plot3, plot4, 
                        nrows = 2, 
                        titleX = TRUE,
                        titleY = TRUE,
                        margin = 0.05) %>%
  layout(title = list(text = "Histograms of Sampling Methods"))

sampling_fig


#table of mu and sigmas-------------------------------------------------------
Mu <- c(mu, mu_srs, mu_sys, strata_mu)
Sigma <- c(sigma, sigma_srs, sigma_sys, strata_sigma)
Sample.Type <- c( "Raw Data", "SRSWOR", "Systematic", "Stratified")

results <- data.frame(Sample.Type, Mu, Sigma)
results


#More Graphs -----------------------------------------------------------------


#filter on felonys and misdemeanors

par(mfrow = c(1,2))
offense_df <- df2[which(df2$offense_level == "Misdemeanor" | 
                                df2$offense_level == "Felony"),]
head(offense_df)

count1 <- table(offense_df$offense_level,
                offense_df$sex)

mosaicplot(count1,
           main = "Sex and Level of Crime",
           color = c("violetred4", "cornflowerblue"),
           las = 1)


## table of race and offense level
#didn't end up using but it exists
count2 <- table(offense_df$offense_level,
                offense_df$race)

mosaicplot(count2,
           main = "Race and Level of Crime",
           color = c("violetred4", "cornflowerblue"),
           las = 1)
par(mfrow = c(1,1))

#stacked bar chart of race v. offense level
ggplot(offense_df %>% count(race, offense_level),
       aes(race, n, fill = offense_level)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Race v. Offense Level") +
  labs(x = "Race of Inmate",
       y = "Number of Inmates")

#bar of race and high level 
#using data_strata df to filter on race and superhighlevel
ggplot(data_strata %>% count(superhighlevel, race),
       aes(superhighlevel, n, fill = race))+
  #use position = "stack" for stacked and "dodge" for side by side
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("Race v. Offense Category") +
  labs(x = "Offense Category",
       y = "Number of Offenders")

employment_offense <- offense_df[which(offense_df$employment_status != ""), ]
##This is the most curated of the ggplots so far
ggplot(employment_offense %>% count(employment_status, offense_level),
       aes(x = employment_status, y = n, fill = offense_level)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Employment Status v. Offense Level") +
  labs(x = "Employment Status",
       y = "Number of Inmates",
       fill = "Offense Level") +
  ylim(0, 30000) +
  scale_fill_brewer(palette="Dark2")


#Plotly stacked of employment v. superhighlevel
emp_crime <- employment_df[which(employment_df$superhighlevel != ""), ]
ec_tibble <- as_tibble(emp_crime)
my_summary <- ec_tibble %>%
  count(superhighlevel, employment_status)

my_summary

ec <- plot_ly(my_summary,
              x = ~superhighlevel,
              y = ~n,
              type = "bar",
              color = ~employment_status) %>%
  layout(barmode = "stack",
         title = list(text = "<b>Employment Status versus Crime Committed<b>"),
         yaxis = list(title = list(text = "Number of Inmate Bookings")),
         xaxis = list(title = list(text = "Crime Committed"),
                      categoryorder = "total descending"),
         legend = list(title = list(text = "<b>Employment Status<b>")))
ec

#Marital Statius

mar_crime <- marital_df[which(marital_df$superhighlevel != ""), ]
mc_tibble <- as_tibble(mar_crime)
mc_summary <- mc_tibble %>%
  count(superhighlevel, marital_status)

mc <- plot_ly(mc_summary,
              x = ~superhighlevel,
              y = ~n,
              type = "bar",
              color = ~marital_status) %>%
  layout(barmode = "stack",
         title = list(text = "<b>Marital versus Crime Committed<b>"),
         yaxis = list(title = list(text = "Number of Inmate Bookings")),
         xaxis = list(title = list(text = "Crime Committed"),
                      categoryorder = "total descending"), 
         legend = list(title = list(text = "<b>Marital Status<b>")))
mc

#Veteran Status
military_df <- inmate_df[which(inmate_df$military != ""), ]
mil_count <- as.data.frame(table(military_df$military))

mil <- plot_ly(mil_count,
               labels = ~Var1,
               values = ~Freq,
               type = "pie",
               marker = list(colors = c("#556677", "#AA3344", "#772200", 
                                        "#11AA22", "#AA231B88"))) %>%
  layout(title = list(text = "Proportion of Veterans"))
mil



sex <- df2[which(df2$superhighlevel != ""), ]
sex_tibble <- as_tibble(sex)
sex_summary <- sex_tibble %>%
  count(superhighlevel, sex)

#sex_summary <- sex_summary[order(sex_summary$n, decreasing = TRUE),]
#sex_summary

splot <- plot_ly(sex_summary,
              x = ~n,
              y = ~superhighlevel,
              type = "bar",
              color = ~sex,
              orientation = "h") %>%
  layout(barmode = "stack",
         title = list(text = "<b>Sex versus Crime Committed<b>"),
         yaxis = list(title = list(text = "Crime Committed"),
                      categoryorder = "total ascending"), 
         xaxis = list(title = list(text = "Number of Bookings")),
         legend = list(title = list(text = "<b>Sex<b>")))
splot


cdf <- as.data.frame(table(df2$crime))
wordstoremove <- c("", "OTHER CRIMINAL OFFENSES", "MISC JAIL CODE")
cdf <- filter(cdf, !(Var1 %in% wordstoremove))
cdf <- cdf[order(cdf$Freq, decreasing = TRUE), ]
cdf <- cdf[1:20, ]
cdf

cdf_colors <- colorRampPalette(brewer.pal(11, "RdGy"))(nrow(cdf))

cdfplot <- plot_ly(cdf,
                 x = ~Freq,
                 y = ~Var1,
                 type = "bar",
                 marker = list(color = cdf_colors),
                 orientation = "h") %>%
  layout(barmode = "stack",
         title = list(text = "<b>Most Common Crimes Committed<b>"),
         yaxis = list(title = list(text = "<b>Crime Committed<b>"),
                      categoryorder = "total ascending"), 
         xaxis = list(title = list(text = "Number of Bookings")))
cdfplot
