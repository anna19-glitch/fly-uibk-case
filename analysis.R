##Analytical Questions
# does correcting the delay indicator affect LDA or FlyUIBK flights more?
# calculate the average flight duration by route and airline
# does the day of the week effect things
# do general descriptive statistics
# do t tests and other hypothesis testing
# check delay by origin, delay by destination

library(ggplot2)

#calculate the average delay and percent of flights delayed by airline
flights %>%
  group_by(airline) %>%
  summarize(
    avg_delay = mean(delay_calc),
    pct_delayed = mean(delay_indicator_calc)
  )

#plot the above data
ggplot(flights, aes(x = delay_calc, fill = airline)) +
  geom_histogram(bins = 50, alpha = 0.6) +
  theme_minimal()

#calculate the mean duration by route and airline
duration_mean <- flights |>
  group_by(airline, route_calc) |> # Group by multiple columns
  summarize(average_duration = mean(flight_duration)) # Calculate mean for each group

print(duration_mean)

#calculate the average delay by route and airline 
delay_mean <- flights |>
  group_by(airline, route_calc) |> # Group by multiple columns
  summarize(average_delay = mean(delay_calc)) # Calculate mean for each group

print(delay_mean)

#get info on what weekdays are the most delayed
weekday_analysis <- flights |>
  group_by(airline, route_calc, weekday) |>
  summarize(
    avg_delay = mean(delay_calc),
    pct_delayed = mean(delay_indicator_calc),
    average_duration = mean(flight_duration)
  )

summary(flights)
#my notes from looking at the summary statistics
# there are more flights on tuesday in the dataset, which airline has the most flights scheduled to leave on a specific day
# 873 delay indicators were corrected, so the delay percentages should be much better than they were before
# should i calculate what the avg percent delay was before i corrected it?
# check flight duration variable - the minimum is 5 - this doesn't make sense

#check the distribution
hist(flights$delay_calc, breaks = 50)

qqnorm(flights$delay_calc)
qqline(flights$delay_calc)
#indicates that arrival delays are not normally distributed
#the data is obviously not normal

#t-tests and hypothesis testing

#H1 Flights involving Berlin have higher average arrival delays than flights involving Oslo.
wilcox.test(delay_calc ~ destination,
            data = flights |> filter(destination %in% c("BER", "OSL")))
#the p-value is >0.05 meaning that we cannot reject the null hypothesis

# H2:Arrival delays differ across the four routes.
kruskal.test(delay_calc ~ routecode, data = flights)

# H3: FlyUIBK has higher average arrival delays than LDA.
wilcox.test(delay_calc ~ airline, data = flights)
# the p-value is >0.05 meaning we cannot reject the null hypothesis

#i don't really know how to interpretthis so i should probably leave it out
prop.test(
  table(flights$delay_indicator_calc, flights$airline)
)

#H4:FlyUIBK operates proportionally more flights on congested routes (e.g., BER).
#i don't know how to interpret this but i do think this is one of the important ones
chisq.test(table(flights$airline, flights$destination))

#H5:Delays differ by weekday.
kruskal.test(delay_calc ~ weekday, data = flights)
#the p-value is EXTREMELY small so we can reject the null hypothesis - weekday does matter
#create table to compare which airline flies on which day. this is relatively equal
table(flights$weekday, flights$airline)

#create time buckets for departure
flights <- flights |>
  mutate(dep_hour = hour(sched_dep_dt))
#H6:Later departures have higher delays.
cor.test(flights$dep_hour, flights$delay_calc, method = "spearman")
#the p value is high meaning we cannot reject the null

#H7:Flights with more passengers experience longer delays.
cor.test(
  flights$passengers,
  flights$delay_calc,
  method = "spearman",
  use = "complete.obs"
)
# p-value greater than 0.05 so we cannot reject the null