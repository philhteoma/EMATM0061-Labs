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


hal <- Hawks$Hallux[!is.na(Hawks$Hallux)]

outlierVal <- 100
numOutliers <- 10

corruptedHal <- c(hal, rep(outlierVal, times=numOutliers))

mean(hal)
mean(corruptedHal)

generateHalWithOutliers <- function(v) {
  return ( c(hal, v))
}

mean(generateHalWithOutliers(seq(1000, 10)))
mean(generateHalWithOutliers(seq(100, 1000)))
mean(generateHalWithOutliers(seq(0, 1000)))

numOutliers <- seq(0, 1000)
meansVector <- vector("numeric")

for (outliers in numOutliers) {
  corruptedHal <- c(hal, rep(outlierVal, times=outliers))
  meansVector <- c(meansVector, mean(corruptedHal))
}

tMeanVector <- vector("numeric")
for (outliers in numOutliers) {
  corruptedHal <- c(hal, rep(outlierVal, times=outliers))
  tMeanVector <- c(tMeanVector, mean(corruptedHal, trim=0.1))
}


medianVector <- vector("numeric")
for (outliers in numOutliers) {
  corruptedHal <- c(hal, rep(outlierVal, times=outliers))
  medianVector <- c(medianVector, median(corruptedHal))
}

meansVector
medianVector
tMeanVector

dfMeansMedians <- data.frame(numOutliers=numOutliers, means=meansVector, medians=medianVector, tMeans=tMeanVector)

dfMeansMedians

dfMeansMedians %>% 
  pivot_longer(!numOutliers, names_to="Estimator", values_to = "Value") %>% 
  ggplot(aes(numOutliers, color=Estimator, linetype=Estimator, y=Value)) +
  geom_line() + xlab("Number of Values")


# Boxplot

Hawks %>% 
  ggplot(aes(x=Species, y=Weight)) +
  geom_boxplot()


num_outliers <- function(v) {
  result <- c()
  iqr <- IQR(v, na.rm=TRUE)
  med <- median(v)
  # firstQuartile <- median(v[1:floor(length(v)/2)])
  # thirdQuartile <- median(v[ceiling(length(v)/2):length(v)])
  q25 <- quantile(v, 0.25, na.rm=TRUE)
  q75 <- quantile(v, 0.75, na.rm=TRUE)
  lowOutlierBound <- q25 - 1.5*iqr
  highOutlierBound <- q75 + 1.5*iqr
  # print(lowOutlierBound)
  # print(highOutlierBound)
  for (i in v) {
    if ((i < lowOutlierBound | i > highOutlierBound) & !is.na(i)) {
      result <- c(result, i)
    }
  }
  # print(result)
  return (length(result))
}

num_outliers(c(-1000, 0, 1, 1, 1, 1, 2, 1000))



Hawks %>% 
  group_by(Species) %>% 
  summarise(
    weight_outliers=num_outliers(Weight),
    wing_outliers=num_outliers(Wing)
    )




