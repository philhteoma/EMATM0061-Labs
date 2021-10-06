# install.packages("tidyverse")
library(tidyverse)

# install.packages("Stat2Data")
library(Stat2Data)

data("Hawks")

# Get subset of hawks

head(Hawks)

hSF <- Hawks %>%
  filter(Species=="RT") %>% 
  select("Wing", "Weight", "Tail") %>% 
  filter(Weight >= 1000)

head(hSF)
dim(hSF)
count(hSF)



# 2.2 Arrange
head(arrange(hSF, Wing))


# 2.3 Join
abvs <- unique(Hawks$Species) %>% as.vector()
fullNames <- c("Red-Tailed", "Cooper's", "Sharp-shinned")
hawkNameColumns <- c("Abbreviation", "Full_Name")

hawkNames <- data.frame(Abbreviation=abvs, "Full_Name"=fullNames)

hawksWithNames <- Hawks %>% 
  left_join(hawkNames, by= c("Species" = "Abbreviation"))

hawksWithNames %>% 
  select("Full_Name", "Wing", "Weight") %>% 
  head(7)



#2.4 Mutate
hawksWithBMI <- Hawks %>% 
  mutate(Bird_BMI = 1000 *Weight / (Wing**2)) %>% 
  select("Species", "Bird_BMI")
    

arrange(hawksWithBMI, desc(Bird_BMI)) %>%
  head(8)

hawksWithLowBMI <- hawksWithBMI %>% 
  filter(Bird_BMI <= 100)


arrange(hawksWithLowBMI, desc(Bird_BMI)) %>%
  head(8)


hawksWithLowBMI %>% 
  ggplot(aes(x=Bird_BMI, y=Species, fill=Species)) +
  geom_violin()


# Summarise and groupBy
hawkSummary <- hawksWithNames %>% 
  group_by(Full_Name) %>% 
  summarize(
    num_rows=n(),
    mean_wing_span=mean(Wing, na.rm=TRUE),
    median_wing_span=median(Wing, na.rm=TRUE),
    t_mn_wing=mean(Wing, na.rm=TRUE, trim=0.1),
    tail_wing_ratio=mean(Wing/Tail, na.rm=TRUE)
  )
  

hawkSummary


hawkMissing <- hawksWithNames %>% 
  select(Species, Wing, Weight, Culmen, Hallux, Tail, StandardTail, Tarsus, Crop) %>% 
  group_by(Species) %>% 
  summarize(across(everything(), ~sum(is.na(.x))))

hawkMissing

