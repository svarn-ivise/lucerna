#!/usr/lib/R/bin/Rscript

rm(list=ls())

library(dplyr)

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

client <- "intercity" #system("docker-machine ls -f {{.Name}}",intern=T)

#for(client in clients){
  
  client_ip <- gsub("tcp://|:.*", "",system(paste0("docker-machine ls -f {{.URL}} --filter name=",client), intern = T))
  
  z <- qry(paste0("select * from ",client,"_new"),NULL,"rds") %>%
    bind_rows(qry(paste0("select * from ",client,"_past"),NULL,"rds")) %>% 
    # rename(Quantity = booking_per_day)) %>%
    group_by(Travel.Date, Service) %>% summarise(Quantity = sum(Quantity, na.rm=T)) %>% as.data.frame()
  
  zz <- `attr<-`(Sys.time(),"tzone","NZ")
  travel_dates <- seq(from=Sys.Date() + 1, to=Sys.Date()+191, by="1 day")
  
  demand_cache <- list()
  
  for(x in 1:length(travel_dates)){
    
    for(serv in unique(z$Service)){
      
      travel.date <- travel_dates[x]
      
      cumulative <- sum(z[z$Travel.Date == travel.date & z$Service == serv,]$Quantity, na.rm=T)
      
      purch.date <- Sys.Date() #+1 #gsub(" .*","",z$Purchase.Date[x])
      qty <- 0
      seats <- cumulative
      service <- serv
      
      # travel.date, purchase.date, qty, service
      res <- content(GET(paste0("http://",client_ip,"/nnpred"), 
                         query = list("purchase.date" = as.character(purch.date),
                                      "travel.date" = as.character(travel.date),
                                      "cumulative" = cumulative,
                                      "service" = as.character(service),
                                      "qty" = 0)))
      
      cumulative.pred <- as.numeric(res$prediction[[1]])
      
      print(paste0(serv," Seats Sold: ",cumulative," for ",travel_dates[x],", Total Predicted: ",cumulative.pred))
      
      demand_cache[[length(demand_cache) + 1]] <- data.frame(Travel.Date = travel_dates[x], Sold = cumulative, 
                                                             Service = serv, Predicted = cumulative.pred)
      
    }
    
  }
  
  demand_cache <- bind_rows(demand_cache)
  demand_cache$Cache.Time <- `attr<-`(Sys.time(),"tzone","NZ") 
  
  app(demand_cache,paste0(client,"_demand"))
#}