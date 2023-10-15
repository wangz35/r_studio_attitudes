csv1<-read.csv("BOROUGH.csv")
CSV2<-read.csv("BUILDING_CLASS.csv")
CSV<-read.csv("NEIGHBORHOOD.csv")
library(odbc)
library(DBI)
library(tidyverse)
library(lubridate)
#Q1：
nyc_transaction_data<-read.csv("NYC_TRANSACTION_DATA.csv")
neiborhood_NYC<-left_join(NYC_TRANSACTION_DATA_After2010,CSV,by=c("NEIGHBORHOOD_ID"="NEIGHBORHOOD_ID"))
buildingclasss_NYC<-left_join(neiborhood_NYC,CSV2,multiple = "all",by=c("BUILDING_CLASS_FINAL_ROLL"="BUILDING_CODE_ID"))
Table<-left_join(buildingclasss_NYC,csv1,by=c("BOROUGH_ID"="BOROUGH_ID"))
Table<-filter(Table,TYPE=="RESIDENTIAL")
Tableresult<-filter(Table,NEIGHBORHOOD_ID==218)%>%
  mutate(YEAR = as.numeric(substr(SALE_DATE, 1, 4)))%>%
  group_by(YEAR) %>%
  summarise(SALE_PRICE=sum(SALE_PRICE),Gross=sum(GROSS_SQUARE_FEET))
TableQ1<-mutate(Tableresult,PRICE_PER_SQUARE_FEET = SALE_PRICE/Gross)
View(TableQ1)

#Q2
#回答第二问

#Q3
Table<-drop_na(Table)
Table<-filter(Table,SALE_PRICE>0,GROSS_SQUARE_FEET>0)
Tableresult2<-filter(Table,NEIGHBORHOOD_ID==218)%>%
  mutate(YEAR = as.numeric(substr(SALE_DATE, 1, 4)))%>%
  group_by(YEAR) %>%
  summarise(SALE_PRICE=sum(SALE_PRICE),Gross=sum(GROSS_SQUARE_FEET))
TABLE218<-mutate(Tableresult2,PRICE_PER_SQUARE_FEET = SALE_PRICE/Gross)
view(TABLE218)
#Q4
#把neighorhood换成两个不同的编号
TABLE220<-filter(Table,NEIGHBORHOOD_ID==220)%>%
  mutate(YEAR = as.numeric(substr(SALE_DATE, 1, 4)))%>%
  group_by(YEAR) %>%
  summarise(SALE_PRICE=sum(SALE_PRICE),Gross=sum(GROSS_SQUARE_FEET))
TABLE220<-mutate(TABLE220,PRICE_PER_SQUARE_FEET = SALE_PRICE/Gross)
view(TABLE220)
TABLE200<-filter(Table,NEIGHBORHOOD_ID==200)%>%
  mutate(YEAR = as.numeric(substr(SALE_DATE, 1, 4)))%>%
  group_by(YEAR) %>%
  summarise(SALE_PRICE=sum(SALE_PRICE),Gross=sum(GROSS_SQUARE_FEET))
TABLE200<-mutate(TABLE200,PRICE_PER_SQUARE_FEET = SALE_PRICE/Gross)
view(TABLE200)

#Q5
ggplot()+geom_line(data=TABLE218, aes(x=YEAR, y=SALE_PRICE, color="TABLE218")) +
  geom_line(data=TABLE220, aes(x=YEAR, y=SALE_PRICE, color="TABLE220")) +
  geom_line(data=TABLE200, aes(x=YEAR, y=SALE_PRICE, color="TABLE200")) +
  scale_color_manual(values=c("TABLE218"="red", "TABLE220"="blue", "TABLE200"="green")) +
  labs(color = "Source")

