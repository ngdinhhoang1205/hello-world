# 1- Make a new script called Section 7 assignment.
## Import twentyeleven.csv

library(readr)
twentyeleven <- read.csv("twentyeleven.csv")
head(twentyeleven)
typeof(twentyeleven$date)


# 2- use lubridate and base R , to get the day of the week, the month and the year.

library(dplyr)
library(lubridate)

twentyeleven$date <- as.Date(twentyeleven$date, format = "%y-%M-%d")

##day of the week

twentyeleven$week_day <- weekdays(twentyeleven$date)

table(twentyeleven$week_day)

## the month

twentyeleven$month <- month(twentyeleven$date)

## the year

twentyeleven$year <- year(twentyeleven$date)

names(twentyeleven)

# 3- Use hour() to get only the hour component in a new column.


twentyeleven$hour <- hour(twentyeleven$hour1)

head(twentyeleven)

table(twentyeleven$hour)

# 4- make a histogram of the hours


hist(twentyeleven$hour)

# 5- Get the last purchase date per customer

twentyeleven %>% group_by(Customer.ID) %>% summarise(last_date = max(date))

# 6- get the recency per customer


customers_orders <- twentyeleven %>% group_by(Customer.ID,date) %>% summarise(date = mean(date)) %>% arrange(Customer.ID,date)

customers_orders$previous_date <- lag(customers_orders$date,n=1)

customers_orders$previous_date <- NULL

customer_splitted <- split(customers_orders,customers_orders$Customer.ID)

length(customer_splitted)

for (i in 1 : length(customer_splitted)){
  customer_splitted[[i]]$previous_date <- lag(customer_splitted[[i]]$date,n=1)
}

customer_splitted[[12]]



# 7- model the inter-arrival time of customers in days and make a histogram of it.

customer_splitted <- plyr::ldply(customer_splitted,data.frame)

customer_splitted$diff <- customer_splitted$date-customer_splitted$previous_date


customer_splitted %>% group_by(Customer.ID) %>% summarise(inter_arrival = mean(diff, na.rm = TRUE))


## for this, you will need a date column to get the days from it.

