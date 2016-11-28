titanic<-read.csv("titanic3.csv")
summary(titanic)
#1
table(titanic$embarked)
titanic$embarked[titanic$embarked==""]<-"S"

#2
titanic$age[is.na(titanic$age)] <- round(mean(titanic$age, na.rm = TRUE),0)
#there wouldn't have any changes in overall statistical calculations.

#3
table(titanic$boat)
titanic$boat[titanic$boat==""]<-NA

#4
table(titanic$cabin)
library(dplyr)
titanic_clean<-mutate(titanic, has_cabin_number = ifelse(titanic$cabin == "", 0, 1))
summary(titanic_clean)
str(titanic_clean)
  
#5
write.csv(titanic_clean, "titanic_clean.csv")