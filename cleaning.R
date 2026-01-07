#first import the csv file from the repository as a dataframe in R

library(dplyr)
library(lubridate)

#name the data flights
flights <- StudentData_FlyUIBK_2021_2022_v2

str(flights)
colSums(is.na(flights)) #make sure there are no NAs in the data
#There are only NAs for passenger data, only FlyUIBK tracks passenger numbers

#check the classes of the variables
sapply(flights, class)
#airline, origin,destination = character
#sch dept, sch arr, act arr = hms difftime
#arr del, del indicator, route code, num of passengers = numeric
#departure date = date

#creating variable for day of the week because it does not exist
flights$weekday <- weekdays(flights$`Departure date`)

#convert categorical variables
flights <- flights |>
  mutate(
    airline = factor(Airline),
    origin = factor(`Origin airport`),
    destination = factor(`Destination airport`),
    routecode = factor(`Route Code`),
    weekday = factor(`weekday`)
  )

#remove old variables from flights
flights$Airline <- NULL
flights$`Origin airport` <- NULL
flights$`Destination airport` <- NULL
flights$`Route Code` <- NULL

#rename variables
names(flights)
names(flights)[names(flights) == "Scheduled departure time"] <- "sched_dep"
names(flights)[names(flights) == "Scheduled arrival time"] <- "sched_arr"
names(flights)[names(flights) == "Actual arrival time"] <- "actual_arr"
names(flights)[names(flights) == "Arrival delay in minutes"] <- "arr_delay"
names(flights)[names(flights) == "Delay indicator"] <- "delay"
names(flights)[names(flights) == "Number of passengers"] <- "passengers"
names(flights)[names(flights) == "Departure date"] <- "dep_date"

#recalculate delay 
flights <- flights |>
  mutate(
    delay_calc = as.numeric(difftime(actual_arr, sched_arr, units = "mins"))
  )

#checking to see if any of the original delay times in minutes were calculated incorrectly
flights |> mutate(check = delay_calc == arr_delay) %>% filter(!check) %>% pull(delay_calc)
#no, the delays were calculated correctly, so I will not be using my calculated times because it creates problems when the flights arrive after midnight

#fix my delay problem

flights <- flights |>
  mutate(
    delay_calc = ifelse(delay_calc < -600, delay_calc + 1440, delay_calc)
  )
flights |> mutate(check = delay_calc == arr_delay) %>% filter(!check) %>% pull(delay_calc)
#this confirms that the delays were calculated correctly

#correct delay indicator so that delay = True when delay > 15 minutes
flights <- flights |>
  mutate(
    delay_indicator_calc = ifelse(delay_calc >= 15, 1, 0)
  )

#compare
table(flights$delay_indicator_calc, flights$delay)

flights |> mutate(check = delay_indicator_calc == delay) %>% filter(!check) %>% pull(airline)

#create a new variable
flights$was_corrected <- with(flights, ifelse(delay_indicator_calc == delay, FALSE, TRUE))

#create arrival date variable
flights <- flights %>%
  mutate(
    arr_date = if_else(
      format(sched_arr, "%H:%M:%S") < format(sched_dep, "%H:%M:%S"),
      dep_date + 1,
      dep_date
    )
  )

#create new date/time variables
flights <- flights %>%
  mutate(
    sched_dep_dt = as.POSIXct(
      paste(dep_date, format(sched_dep, "%H:%M:%S")),
      tz = "UTC"
    ),
    sched_arr_dt = as.POSIXct(
      paste(arr_date, format(sched_arr, "%H:%M:%S")),
      tz = "UTC"
    )
  )

#create date/time for the actual arrival var
flights <- flights %>%
  mutate(
    actual_arr_dt = if_else(
      !is.na(actual_arr) &
        format(actual_arr, "%H:%M:%S") < format(sched_dep, "%H:%M:%S"),
      dep_date + 1,
      dep_date
    ),
    actual_arr_dt = as.POSIXct(
      paste(actual_arr_dt, format(actual_arr, "%H:%M:%S")),
      tz = "UTC"
    )
  )

#calculate flight duration
flights <- flights |>
  mutate(
    flight_duration = as.numeric(difftime(sched_arr_dt, sched_dep_dt, units = "mins"))
  )

# check route codes for correctness
flights <- flights %>%
  mutate(
    route_calc = case_when(
      origin == "BER" & destination == "VIE" ~ 1,
      origin == "VIE" & destination == "BER" ~ 2,
      origin == "VIE" & destination == "OSL" ~ 3,
      origin == "OSL" & destination == "VIE" ~ 4,
      TRUE ~ NA_real_
    )
  )
#see if there are errors with route codes
route_errors <- flights |>
  filter(route_calc != routecode)

nrow(route_errors)

route_errors |>
  select(origin, destination, routecode, route_calc)
#this is an empty tibble - there are no route code errors

## To do Cleaning
#are the route indicators all correct
##Analytical Questions
# does correcting the delay indicator affect LDA or FlyUIBK flights more?
#calculate the average flight duration by route and airline


