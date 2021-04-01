library(readr)
library(tidyverse)
library(ggthemes)
library(plotfunctions)
library(tidyr)
library(ggplot2)
library(moderndive)
library(skimr) 
library(dplyr)
library("TeachingDemos")


cookie_cats <- read_csv("cookie_cats.csv")


##For retention_1 variable


ggplot(cookie_cats)+
  geom_bar(aes(x=version,fill=retention_1))



a_b_data<-cookie_cats%>%
              group_by(version)%>%
                 summarise(retention_1) %>%
                  table()


## proportion of people who left after playing gate_30 and gate_40 after a day.
prop.test(x=c(20034,20019),n=c(44700,45489) ,correct = TRUE,conf.level = 0.95) 


## proportion of people who did'nt leave after playing gate_30 and gate_40 after a day.
prop.test(x=c(24666,25370),n=c(44700,45489) ,correct = TRUE)
## OR
aa<-prop.test(x=a_b_data ,correct = TRUE, conf.level = 0.95)




##For retention_7 variable

ggplot(cookie_cats)+
  geom_bar(aes(x=version,fill=retention_7))


a_b_data2<-cookie_cats%>%
  group_by(version)%>%
  summarise(retention_7) %>%
  table()


## proportion of people who left after playing gate_30 and gate_40 after a week
prop.test(x=c(8502,8279),n=c(44700,45489) ,correct = TRUE,conf.level = 0.95) 


## proportion of people who did'nt leave after playing gate_30 and gate_40 after a week
prop.test(x=c(36198,37210),n=c(44700,45489) ,correct = TRUE)
## OR
aa<-prop.test(x=a_b_data2 ,correct = TRUE, conf.level = 0.95)
