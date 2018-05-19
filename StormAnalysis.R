library(R.utils)
library(data.table)
library(dplyr)
library(ggplot2)

#Data load
setwd("C:/Users/Christian.Flessner/Dropbox (ZirMed)/Christian Flessner/Coursera/Reproducable Research/StormAnalysis")
#bunzip2("StormData.csv.bz2")
#storms<-fread("StormData.csv")

############Data Processing##################

#public health

#Economic Impact
cleanstorm<- storms %>% select(EVTYPE,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP) %>%
        filter(PROPDMGEXP %in% c("h","H","k","K","m","M","b","B","","0")&
                       CROPDMGEXP %in% c("h","H","k","K","m","M","b","B","","0")
               )




cleanstorm<- cleanstorm %>% 
        mutate(PropertyDamage=case_when(PROPDMGEXP==""|PROPDMGEXP=="0"~PROPDMG
                                ,PROPDMGEXP=="h"|PROPDMGEXP=="H"~PROPDMG*100
                                ,PROPDMGEXP=="k"|PROPDMGEXP=="K"~PROPDMG*1000
                                ,PROPDMGEXP=="m"|PROPDMGEXP=="M"~PROPDMG*1000000
                                ,PROPDMGEXP=="b"|PROPDMGEXP=="B"
                                ~PROPDMG*1000000000))

cleanstorm<- cleanstorm %>% 
        mutate(CropDamage=case_when(CROPDMGEXP==""|CROPDMGEXP=="0"~CROPDMG
                                ,CROPDMGEXP=="h"|CROPDMGEXP=="H"~CROPDMG*100
                                ,CROPDMGEXP=="k"|CROPDMGEXP=="K"~CROPDMG*1000
                                ,CROPDMGEXP=="m"|CROPDMGEXP=="M"~CROPDMG*1000000
                                ,CROPDMGEXP=="b"|CROPDMGEXP=="B"
                                ~CROPDMG*1000000000))
cleanstorm<- cleanstorm %>% mutate(TotalDamage=CropDamage+PropertyDamage)

cleanstorm<- cleanstorm %>% select(EVTYPE, PropertyDamage, CropDamage, 
                                   TotalDamage) %>% group_by(EVTYPE) %>%
        summarize(sum(PropertyDamage),sum(CropDamage),sum(TotalDamage))

names(cleanstorm)<-c("EventType","PropertyDamage","CropDamage","TotalDamage")
cleanstorm<-arrange(cleanstorm, desc(TotalDamage))
cleanstorm<-head(cleanstorm %>% select(EventType, PropertyDamage, CropDamage)
                 , 15)
meltstorm<-melt(cleanstorm,id.var="EventType")

ggplot(meltstorm, aes(x=EventType, y=value, fill=variable))+
        geom_bar(stat="identity")





