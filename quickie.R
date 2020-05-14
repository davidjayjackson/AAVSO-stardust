library(tidyverse)
library(plotly)
library(lubridate)
library(pracma)



rm(list=ls())
mydata <-data.table::fread("../db/S_CEP.csv") %>% 
  # select(JD,StarName,Band,Magnitude,ValidationFlag) %>%
  mutate( JD = as.numeric(JD)) %>%
  mutate(Ymd =insol::JD(JD,inverse = TRUE)) %>%
  mutate(Ymd = as.Date(Ymd))  %>%
  mutate(Julian = as.integer(JD)) %>%
  mutate(Julian = as_factor(Julian)) %>%
  mutate(Magnitude = as.numeric( gsub("<","", Magnitude) ) + 0.99) %>%
  filter(Band=="Vis." )

mydata$Star_Name <- "S_CEP"

starDust <- mydata %>%
  group_by(Julian) %>%
  summarize(Mean = round(mean(Magnitude),digits=1),
            Brighter = round(Mean -1,digits = 1),
            Fainter = round(Mean +1,digits= 1),
            Obs = n()) %>% 
  ungroup() %>% 
  mutate(Verify = case_when(
    (Brighter <  Mean -1) | (Fainter > Mean + 1) ~ "Yes",TRUE ~ "No")) %>%
  filter(Obs >=3 & Verify=="Yes") 

mydata %>% plot_ly(x=~Julian,y=~Magnitude,name="Magnitude",type="scatter",mode="markers") %>% 
  add_trace(data=starDust,x=~Julian,y=~Fainter,name="Mean +1",type="scatter",mode="markers") %>% 
  add_trace(data=starDust,x=~Julian,y=~Brighter,name="Mean -1",type="scatter",mode="markers") %>%
  layout(yaxis = list(autorange = "reversed")) %>%
  layout(title = "Julian Dates: Daily Mean Magnitude +/- 1")

starDust %>% DT::datatable()


