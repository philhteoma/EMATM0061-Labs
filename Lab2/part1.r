# install.packages("tidyverse")
library(tidyverse)


# install.packages("Stat2Data")
library(Stat2Data)

data("Hawks")

hawksSmall <- drop_na(select(Hawks, Age, Day, Month,
                             Year, CaptureTime, Species,
                             Wing, Weight, Tail))

unique(hawksSmall$Age)

# Classify Variables
# 1. Month    - Discrete
# 2. Species  - Categorical
# 3. Age      - Categorical? (Adult, Infant?)
# 4. Wing     - Continuous?
# 5. Weight   - Discrete


# 1.2 Whats wrong with the plot?
# - No axis labels
# - No units
# - Too many variables
# - Colour is likely a poor choice


# Create a histogram of the weights of the hawks

hawksWeightHist <- hawksSmall %>% ggplot(aes(Weight)) + 
               geom_histogram(binwidth = 100) +
               xlab("Hawk Weight (g?)") +
               ylab("count")

# The aesthetic used is the weight of the birds
# The plot is bimodal


# Generate a density plot
hawksTailDensity <- hawksSmall %>% ggplot(aes(x=Tail, color=Species)) + 
  geom_density(adjust=0.5) +
  xlab("Tail Length (mm?)") +
  ylab("count")

hawksTailDensity

# The adjust argument smooths the density plot - low values will overfit, high values underfit

hawksTailViolin <- hawksSmall %>% ggplot(aes(x=Tail, y=Species, fill=Species)) + 
  geom_violin() +
  xlab("Tail Length (mm?)") +
  ylab("count")

hawksTailViolin



# 1.5 Scatter Plots
hawksTailScatter <- hawksSmall %>% ggplot(aes(x=Tail, y=Weight, color=Species, shape=Species)) + 
  geom_point() +
  xlab("Tail Length (mm?)") +
  ylab("Weight")

hawksTailScatter


#1.6 ???

hawksTailWtf <- hawksSmall %>% ggplot(aes(x=Tail, y=Weight, color=Species, shape=Species)) + 
  geom_point() +
  geom_smooth(method="glm") +
  facet_wrap(~Species) +
  xlab("Tail Length (mm?)") +
  ylab("Weight")

hawksTailWtf

# Valid geom_smooth methods:
# "lm",
# "glm",
# "gam",
# "loess"
