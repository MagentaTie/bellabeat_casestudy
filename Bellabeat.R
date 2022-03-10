#installing the required packages that will be required during analysis

install.packages("tidyverse")
install.packages('dplyr')
install.packages('ggplot2')
install.packages('tidyr')
install.packages('stringr')

library(ggplot2)
library(tidyverse)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)

#since the datasets are stored in csv files, we have imported them using read_csv function

dailyActivity_merged <- read_csv("C:/Users/vedan/Desktop/Bellabeat/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")

dailyCalories_merged <- read_csv("C:/Users/vedan/Desktop/Bellabeat/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")

dailySteps_merged <- read_csv("C:/Users/vedan/Desktop/Bellabeat/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")

dailyIntensities_merged <- read.csv("C:/Users/vedan/Desktop/Bellabeat/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")

sleepDay_merged <- read.csv("C:/Users/vedan/Desktop/Bellabeat/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")

weightLogInfo_merged <- read.csv("C:/Users/vedan/Desktop/Bellabeat/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")


sleepDay_new <- sleepDay_merged %>% separate(SleepDay,c("ActivityDay", "Time"), sep=" ")
view(sleepDay_new)
colnames(dailyActivity_merged)

weightLogInfo_new <- weightLogInfo_merged %>% separate(Date,c("ActivityDate", "Time"), sep=" ")
view(weightLogInfo_new)
view(weightLogInfo_merged)

length(unique(dailyActivity_merged$Id))
length(unique(dailyCalories_merged$Id))
length(unique(dailySteps_merged$Id))
length(unique(dailyIntensities_merged$Id))
length(unique(sleepDay_merged$Id))
length(unique(weightLogInfo_merged$Id))


  
ggplot(data=dailyActivity_merged)+ 
  geom_point(mapping=aes(x=TotalDistance, y=TotalSteps), color="brown")+
  geom_smooth(mapping=aes(x=TotalDistance, y=TotalSteps), color="black",  method="lm")+
  ggtitle("Total Steps v/s Total Distance (in km)")

ggplot(data=dailyActivity_merged)+ 
  geom_point(mapping=aes(x=TotalDistance, y= Calories), color="steelblue")+
  geom_smooth(mapping=aes(x=TotalDistance, y=Calories), color="black",  method="lm")+
  ggtitle("Calories v/s Total Distance (in km)")

ggplot(data=dailyActivity_merged)+
  geom_point(mapping=aes(x=TotalSteps, y=Calories), color="#265c41")+
  geom_smooth(mapping=aes(x=TotalSteps, y=Calories), color="black",  method="lm")+
  ggtitle("Calories v/s Total Steps")

ggplot(data=dailyActivity_merged)+
  geom_point(mapping=aes(x=LightlyActiveMinutes, y= Calories), color="#ba9200")+
  geom_smooth(mapping=aes(x=LightlyActiveMinutes, y= Calories), color="black",method="lm")+
  ggtitle("Calories v/s Lightly Active Minutes")
 
ggplot(data=dailyActivity_merged)+
  geom_point(mapping=aes(x=FairlyActiveMinutes, y= Calories), color="#f79d00")+
  geom_smooth(mapping=aes(x=FairlyActiveMinutes, y= Calories), color="black",method="lm")+
  ggtitle("Calories v/s Fairly Active Minutes")

ggplot(data=dailyActivity_merged)+
  geom_point(mapping=aes(x=VeryActiveMinutes, y= Calories), color="#ff1c33")+
  geom_smooth(mapping=aes(x=VeryActiveMinutes, y= Calories), color="black", method="lm")+
  ggtitle("Calories v/s Very Active Minutes")

ggplot(data=dailyActivity_merged)+
  geom_point(mapping=aes(x=SedentaryMinutes, y= Calories), color="#5eb502")+
  geom_smooth(mapping=aes(x=SedentaryMinutes, y= Calories), color="black",method="lm")+
  ggtitle(" Calories v/s Sedentary Minutes")

ggplot(data=dailyActivity_merged)+
  geom_smooth(mapping=aes(x=LightlyActiveMinutes, y= Calories), color="#c7c700",method="lm")+
  geom_smooth(mapping=aes(x=FairlyActiveMinutes, y= Calories), color="#c98308",method="lm")+
  geom_smooth(mapping=aes(x=VeryActiveMinutes, y= Calories), color="#ba3009", method="lm")+
  labs(y="Calories", x="Activity Minutes")+
  annotate(geom="text", x=400, y=2500, label="Lightly Active",color="#c7c700")+
  annotate(geom="text", x=150, y=3000, label="Fairly Active", color="Orange" )+
  annotate(geom="text", x=100, y=4000, label="Very Active", color="red")+
  ggtitle(" Comparison of Calories with differen activity minutes")

sleep_calories <-merge.data.frame(dailyCalories_merged, sleepDay_new, by = c("Id", "ActivityDay"))
view(sleep_calories)

ggplot(data=sleep_calories)+
  geom_point(mapping=aes(x=TotalTimeInBed, y=Calories), color="navyblue")+
  geom_smooth(mapping=aes(x=TotalTimeInBed, y=Calories), color="black", method="lm")

activity_weight <-merge.data.frame(dailyActivity_merged, weightLogInfo_new, by = c("Id", "ActivityDate"))


activity_weight$TotalActiveMinutes <- activity_weight$VeryActiveMinutes+activity_weight$FairlyActiveMinutes+activity_weight$LightlyActiveMinutes
view(activity_weight)
ggplot(data=activity_weight)+
  geom_point(mapping=aes(x=SedentaryMinutes, y=BMI, size=BMI), color="#4d2b8c")+
  geom_smooth(mapping=aes(x=SedentaryMinutes, y=BMI), color="Black", method="lm")

ggplot(data=activity_weight)+
  geom_point(mapping=aes(x=TotalActiveMinutes, y=BMI, size=BMI), color="#4d2b8c")+
  geom_smooth(mapping=aes(x=TotalActiveMinutes, y=BMI), color="Black", method="lm")



