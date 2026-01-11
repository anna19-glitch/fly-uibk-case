# Individual Question: Analyze your assigned dataset subset for FlyUIBK concerning
# Number of passengers for 4) ROUTE 4: OSL-VIE. You are an operations analyst at
# FlyUIBK. Your task is to analyze passenger volumes and how this may relate to arrival
# delays.

#create data subset with only FlyUIBK flights and route 4
r4 <- flights |>
  filter(
    airline == "FlyUIBK",
    routecode == 4
  )

summary(r4)
# Summary statistics passengers: minimum 50 passengers, maximum 220, mean is 134.5

# Main Questions
# is passenger volume related to arrival delays

# are delayed flights carrying more passengers than non-delayed flights?
# does passenger volume vary systematically by weekday on this route

# I already know that the delays are not normally distributed in the full dataset but recheck for this subset
hist(r4$delay_calc, breaks = 20)

qqnorm(r4$delay_calc)
qqline(r4$delay_calc)
#not normal

#check distributions for passengers variable
hist(r4$passengers, breaks = 20)
qqnorm(r4$passengers)
qqline(r4$passengers)
# distribution has light tails, meaning there are fewer extreme values in the tails than a normal distribution would have
# this is often seen in uniform distribution, but according to the histogram this data is not uniform either

#H0: no relationship between passengers and delay time
#H1:Higher passenger volumes are associated with longer delays

cor.test(
  r4$passengers,
  r4$delay_calc,
  method = "spearman"
)
# the p-value is 0.1709 meaning i cannot reject the null hypothesis
# Spearman Rho is 0.0855 indicating an extremely weak positive relationship between the variables

#H0: no relationship between delayed flights and passenger volume
#H1: Delayed flights carry more passengers

wilcox.test(
  passengers ~ delay_indicator_calc,
  data = r4
)
# p-value of 0.6939, so I cannot reject the null hypothesis

# what is the relationship between passenger volume and weekday
kruskal.test(passengers ~ weekday, data = r4)
aggregate(passengers ~ weekday, data = r4, median)
#p-value is 0.4794 indicating that there is no significant relationship between passengers and weekday
#each day of the week has flights with about the same numbers of passengers