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

clients <- system("docker-machine ls -f {{.Name}}",intern=T)

for(client in clients){
  
  client_ip <- gsub("tcp://|:.*", "",system(paste0("docker-machine ls -f {{.URL}} --filter name=",client), intern = T))
  
  z <- qry(paste0("select * from ",client,"_new"),NULL,"rds") %>%
    bind_rows(qry(paste0("select * from ",client,"_past"),NULL,"rds")) %>% 
               # rename(Quantity = booking_per_day)) %>%
    group_by(Travel.Date, Service) %>% summarise(Quantity = sum(Quantity, na.rm=T)) %>% as.data.frame()
  
  zz <- `attr<-`(Sys.time(),"tzone","NZ")
  travel_dates <- seq(from=Sys.Date(), to=Sys.Date()+90, by="1 day")
  
  price_cache <- list()
  
  for(x in 1:length(travel_dates)){
    
    for(serv in unique(z$Service)){
      
      cumulative <- sum(z[z$Travel.Date == travel_dates[x] & z$Service == serv,]$Quantity, na.rm=T)
      
      purch.date <- Sys.Date() #gsub(" .*","",z$Purchase.Date[x])
      purch.time <- Sys.time()#NULL #gsub(".* ","",z$Purchase.Date[x])
      travel.date <- travel_dates[x]
      travel.time <- NULL #as.character(z$time[x])
      qty <- NULL
      seats <- cumulative
      service <- serv
      buy.price <- NULL #z$Price[x]
      
      res <- content(GET(paste0("http://",client_ip,"/dynaprice"), 
                              query = list("purchase.date" = purch.date,
                                           "purchase.time" = purch.time,
                                           "travel.date" = travel.date,
                                           "travel.time" = travel.time,
                                           "cumulative" = as.character(seats),
                                           "service" = as.character(service),
                                           "qty" = as.character(qty),
                                           "capacity" = "15",
                                           "buy.price" = buy.price,
                                           "write" = F)))
      
      op.price <- res[[1]]$OP
      cumulative.pred <- res[[1]]$cumsum.final.prediction
      
      print(paste0(serv," Seats Sold: ",cumulative," for ",travel_dates[x],": ",op.price,
                   ", Total Predicted: ",cumulative.pred))
      
      price_cache[[x]] <- data.frame(Travel.Date = travel_dates[x], Sold = cumulative, 
                                Service = serv, Price = op.price)
       
      
    }
    
  }
  
  price_cache <- bind_rows(price_cache)
  price_cache$Cache.Time <- `attr<-`(Sys.time(),"tzone","NZ") 
  
  app(price_cache,paste0(client,"_cache"))
}