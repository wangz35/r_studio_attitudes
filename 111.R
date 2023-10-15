csv1<-read.csv("BOROUGH.csv")
CSV2<-read.csv("BUILDING_CLASS.csv")
CSV<-read.csv("NEIGHBORHOOD.csv")
library(odbc)
library(DBI)
library(tidyverse)
library(lubridate)
nyc_transaction_data<-read.csv("NYC_TRANSACTION_DATA.csv")
neiborhood_NYC<-left_join(NYC_TRANSACTION_DATA_After2010,CSV,by=c("NEIGHBORHOOD_ID"="NEIGHBORHOOD_ID"))
buildingclasss_NYC<-left_join(neiborhood_NYC,CSV2,relationship = "many-to-many",by=c("BUILDING_CLASS_FINAL_ROLL"="BUILDING_CODE_ID"))
Table<-left_join(buildingclasss_NYC,csv1,by=c("BOROUGH_ID"="BOROUGH_ID"))
Table<-filter(Table,TYPE=="RESIDENTIAL")
Tableresult<-filter(Table,NEIGHBORHOOD_ID==218)%>%
mutate(YEAR = as.numeric(substr(SALE_DATE, 1, 4)))%>%
group_by(YEAR) %>%
summarise(SALE_PRICE=sum(SALE_PRICE),Gross=sum(GROSS_SQUARE_FEET))
Table<-drop_na(Table)
Table<-filter(Table,SALE_PRICE>0,GROSS_SQUARE_FEET>0)

view(Tableresult)
Tableresult2<-filter(Table,NEIGHBORHOOD_ID==218)%>%
  mutate(YEAR = as.numeric(substr(SALE_DATE, 1, 4)))%>%
  group_by(YEAR) %>%
  summarise(SALE_PRICE=sum(SALE_PRICE),Gross=sum(GROSS_SQUARE_FEET))
view(Tableresult2)
ggplot()+geom_line(data=Tableresult2,aes(x=YEAR,y=SALE_PRICE))
