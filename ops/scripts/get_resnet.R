#!/usr/lib/R/bin/Rscript

rm(list=ls())

suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
suppressMessages(library(httr))
suppressMessages(library(jsonlite))

##credentials file
config.file <- readLines('~/creds/creds.txt')

qry <- function(string, ip, loc){
  
  library(RMySQL)
  
  if(loc == "client"){
    hst <-ip
    usr <- "shane"
    pwd <- "S13240sx91"
    dbTab <- "dynamic"
    prt <- 6603
  } else if (loc == "rds"){
    hst <-gsub(".*=","",config.file[config.file %>% grepl("db.host",.)])
    usr <- gsub(".*=","",config.file[config.file %>% grepl("db.username",.)])
    pwd <- gsub(".*=","",config.file[config.file %>% grepl("db.pwd",.)])
    dbTab <- "lucerna"
    prt <- 3306
  }
  
  con <- dbConnect(dbDriver("MySQL"), host=hst, user=usr, password=pwd ,db= dbTab, port=prt)
  
  result<- dbGetQuery(con, paste0(string))
  
  dbDisconnect(con)
  
  all_cons <- dbListConnections(MySQL())
  for(cons in all_cons){dbDisconnect(cons)}
  
  return(result)
  
}
app <- function(object, table){
  
  library(RMySQL)
  hst <-gsub(".*=","",config.file[config.file %>% grepl("db.host",.)])
  usr <- gsub(".*=","",config.file[config.file %>% grepl("db.username",.)])
  pwd <- gsub(".*=","",config.file[config.file %>% grepl("db.pwd",.)])
  dbTab <- "lucerna"
  prt <- 3306
  
  con <- dbConnect(dbDriver("MySQL"), host=hst, user=usr, password=pwd ,db= dbTab, port=prt)
  
  dbWriteTable(con, name=table, value=object, append=T, row.names=F)
  
  dbDisconnect(con)
  
  all_cons <- dbListConnections(MySQL())
  for(cons in all_cons){dbDisconnect(cons)}
  
}

###ENTER LOGIN INFORMATION HERE
username <- gsub(".*=","",config.file[config.file %>% grepl("resnet.username",.)])
password <- gsub(".*=","",config.file[config.file %>% grepl("resnet.pwd",.)])

###LOGIN TO RESNET
res <- POST("https://www.resnet.co.nz/live/api/login.r", query= list(ID = username, Key = password))

###GET API TOKEN FROM LOGIN RESPONSE
AuthToken <- fromJSON(content(res, "text"))$AuthToken
Session <- fromJSON(content(res, "text"))$Session

###SEND REQUEST WITH TODAY AS END DATE
res <- POST(paste0("https://www.resnet.co.nz/live/rpt/jsl_experiments.w"),
            query = list(
              SearchFromDate= format(as.Date(`attr<-`(Sys.time(),"tzone","NZ"))-5,"%d%m%Y"),
              SearchToDate = format(as.Date(`attr<-`(Sys.time(),"tzone","NZ")),"%d%m%Y"),
              Experiment="ZeVLyYGPReKAP9wsI0D48A.",
              ShowSearchData="on",
              ShowResultData="on",
              ShowStats="on",
              OutputTo="csv",
              Session=Session,
              AuthToken=AuthToken))

###LOGOUT
logout <- GET("https://www.resnet.co.nz/live/api/logout.r?Session=99999999999999")

###READ PAST RESNET DATA AND COMBINE
price_key <- qry("select * from price_key",NULL,"rds")

#cleaning resnet.data
resnet.data <- read.csv(text=content(res, "text"), row.names=NULL) %>% 
  filter(!is.na(Experiment)) %>% mutate(Experiment= as.character(Experiment)) %>%
  filter(!grepl("ZeVLyYGPReKAP9wsI0D48A|Exp. Variant", .$Searched), Conversion =='Ticketed') %>% 
  arrange(Departs,Searched) %>% select(Departs, Searched,Product,Num.Seats,Experiment) %>% 
  setNames(c("Travel.Date","Purchase.Date", "Service","Quantity","Experiment")) %>%
  left_join(price_key, by = c("Experiment" = "ExperimentVariant")) %>% select(-Experiment) %>%
  mutate(Fare = ifelse(Service =='ICZMZM',137.93, ifelse(Service =='ICZMZG',167.23,158.35)),
         Price = Fare * (1 + ExperimentValue), Travel.Date = dmy_hm(Travel.Date), 
         Purchase.Date = dmy_hms(Purchase.Date)) %>% 
  filter(Travel.Date > Purchase.Date, Purchase.Date > ymd_hms('2018-06-15 15:12:00')) %>%
  #mutate(Purchase.Date = as.Date(Purchase.Date)) %>% 
  filter(Service %in% c("ICZMZM", "ICZMZML",'ICZMZG' ,'ICZMZGL', 'ICZMZC', 'ICZMZCL')) %>%
  mutate(Service = gsub('L','',.$Service)) %>% group_by(Travel.Date, Purchase.Date, Service) %>% 
  summarise(Quantity = sum(Quantity), Price = round(mean(Price),4) ) %>% as.data.frame() %>%
  mutate(time = substr(Travel.Date,12,19), Travel.Date = as.Date(Travel.Date))

##send resnet to api
client.ip <- gsub("tcp://|:.*", "",system(paste0("docker-machine ls -f {{.URL}} --filter name=intercity"), intern=T))

z <- qry("select * from intercity_new",NULL,"rds") %>% 
     #filter(as.Date(Purchase.Date) < min(as.Date(resnet.data$Purchase.Date))) %>%
      bind_rows(qry("select * from transactions",client.ip,"client") %>% 
                  mutate(Quantity = as.numeric(Quantity), Price = as.numeric(Price))) %>%
      mutate(Purchase.Date = as.POSIXct(Purchase.Date,tz="NZDT"), Travel.Date = as.Date(Travel.Date))

resnet.data <- resnet.data %>% 
      mutate(Purchase.Date = as.POSIXct(Purchase.Date,tz="NZDT")) %>% 
      filter(!(resnet.data$Purchase.Date %in% z$Purchase.Date))

for(x in 1:nrow(resnet.data)){
  
  purch.date <- gsub(" .*","",resnet.data$Purchase.Date[x])
  purch.time <- gsub(".* ","",resnet.data$Purchase.Date[x])
  travel.date <- as.character(resnet.data$Travel.Date[x])
  travel.time <- as.character(resnet.data$time[x])
  qty <- resnet.data$Quantity[x]
  service <- resnet.data$Service[x]
  buy.price <- resnet.data$Price[x]
  
  seats <- sum(qry(paste0("
       select * from intercity_past 
       where `Travel.Date` ='",travel.date,"' 
       and Service = '",service,"'
       union 
       select * from intercity_new 
       where `Travel.Date` ='", travel.date,"' 
       and Service = '",service,"'"),NULL,"rds")$Quantity) + qty
  
  res <- content(GET(paste0("http://",client.ip,"/dynaprice"), 
                 query = list("purchase.date" = purch.date,
                              "purchase.time" = purch.time,
                              "travel.date" = travel.date,
                              "travel.time" = travel.time,
                              "cumulative" = as.character(seats),
                              "service" = as.character(service),
                              "qty" = as.character(qty),
                              "capacity" = "15",
                              "buy.price" = buy.price,
                              "write" = T)))
  
  print(res[[1]]$OP)
  
}


#qry("drop table resnet")
#app(resnet.data, "resnet")
