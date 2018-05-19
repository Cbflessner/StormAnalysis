library(R.utils)
library(data.table)
library(dplyr)
library(ggplot2)

#Data load
setwd("C:/Users/Christian.Flessner/Dropbox (ZirMed)/Christian Flessner/Coursera/Reproducable Research/StormAnalysis")
bunzip2("StormData.csv.bz2")
storms<-fread("StormData.csv")

############Data Processing##################

#public health

#Economic Impact
cleanprop<- storms %>% select(EVTYPE,PROPDMG,PROPDMGEXP) %>%
        filter(PROPDMGEXP %in% c("h","H","k","K","m","M","b","B","","0"))
cleancrop<- storms %>% select(EVTYPE,CROPDMG,CROPDMGEXP) %>%
        filter(CROPDMGEXP %in% c("h","H","k","K","m","M","b","B","","0"))


cleanprop<- cleanprop %>% 
        mutate(Damage=case_when(PROPDMGEXP==""|PROPDMGEXP=="0"~PROPDMG
                                ,PROPDMGEXP=="h"|PROPDMGEXP=="H"~PROPDMG*100
                                ,PROPDMGEXP=="k"|PROPDMGEXP=="K"~PROPDMG*1000
                                ,PROPDMGEXP=="m"|PROPDMGEXP=="M"~PROPDMG*1000000
                                ,PROPDMGEXP=="b"|PROPDMGEXP=="B"
                                ~PROPDMG*1000000000))

cleancrop<- cleancrop %>% 
        mutate(Damage=case_when(CROPDMGEXP==""|CROPDMGEXP=="0"~CROPDMG
                                ,CROPDMGEXP=="h"|CROPDMGEXP=="H"~CROPDMG*100
                                ,CROPDMGEXP=="k"|CROPDMGEXP=="K"~CROPDMG*1000
                                ,CROPDMGEXP=="m"|CROPDMGEXP=="M"~CROPDMG*1000000
                                ,CROPDMGEXP=="b"|CROPDMGEXP=="B"
                                ~CROPDMG*1000000000))

cleanprop$DamageType<-"Property"
cleancrop$DamageType<-"Crop"

cleanprop<- cleanprop %>% select(EVTYPE, Damage, DamageType) %>% 
        arrange(desc(Damage))
cleancrop<- cleancrop %>% select(EVTYPE, Damage, DamageType) %>%
        arrange(desc(Damage))

damage<-rbind(cleancrop,cleanprop)
names(damage)[1]<-"EventType"
damage$DamageType<-as.factor(damage$DamageType)
damage<- damage %>% group_by(EventType, DamageType) %>% summarize(sum(Damage))
names(damage)[3]<-"Damage"


