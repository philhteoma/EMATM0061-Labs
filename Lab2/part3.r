# Exploratory data analysis"

# install.packages("tidyverse")
library(tidyverse)

# install.packages("Stat2Data")
library(Stat2Data)

data("Hawks")

summarise(Hawks,
  Wing_mean=mean(Wing, na.rm=TRUE),
  Wing_t_mean=mean(Wing, trim=0.1, na.rm=TRUE),
  wing_med=median(Wing, na.rm=TRUE),
  Weight_mean=mean(Weight, na.rm=TRUE),
  Weight_t_mean=mean(Weight, trim=0.1, na.rm=TRUE),
  Weight_med=median(Weight, na.rm=TRUE),
)
  

Hawks %>% 
  group_by(Species) %>% 
  summarise(
    Wing_mean=mean(Wing, na.rm=TRUE),
    Wing_t_mean=mean(Wing, trim=0.1, na.rm=TRUE),
    wing_med=median(Wing, na.rm=TRUE),
    Weight_mean=mean(Weight, na.rm=TRUE),
    Weight_t_mean=mean(Weight, trim=0.1, na.rm=TRUE),
    Weight_med=median(Weight, na.rm=TRUE),
  )



# 3.2
# Mean of Xi = sum(X1...Xn)/n
# 
# mean X~ = sum(X1*a+b ... Xn*a+b)


# For a given set X1...Xn, sum(X1...Xn) == mean(X1...Xn) * n

# 1: for a given series x1...Xn, sum(X1*a...Xn*a) == sum(X1...Xn)*a?
# 2: for a given series x1...Xn, sum(X1+b...Xn+b) == sum(x1...Xn) + b*n?

# Using hypothesis 1
# mean(X1*a+b...Xn*a+b) = sum(X1*a+b...Xn*a+b) / n
#                       = sum(X1...Xn)*(a*n)+(b*n) / n
#                       = sum(x1...Xn)*a*n / n + b*n/n
#                       = (sum(x1...Xn) / n)*a + b
#                       = mean(x)*a + b

# Sample Variance = (1/n-1)*sum((Xi*a+b) - (a*mU+b))
#                 =         .... aXi - amU
#                 =         .... (Xi - mU) / a
#                 = (1/n-1) * sum((Xi - mU) / a)

# stDev = sqrt of the above

# Above is wrong, need to rework



