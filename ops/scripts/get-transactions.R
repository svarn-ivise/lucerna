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

clients <- system("docker-machine ls -f {{.Name}}",intern=T)

for(client in clients){
  
  client_ip <- gsub("tcp://|:.*", "",system(paste0("docker-machine ls -f {{.URL}} --filter name=",client), intern = T))
  
  if("transactions" %in% as.vector(qry("show tables",client_ip, "client"))[,1]){
  
    trans <- qry("select * from transactions",client_ip, "client")
    max_purch <- qry(paste0("select max(`Purchase.Date`) as date from ",client,"_new"),NULL,"rds")
    app(trans[as.POSIXct(trans$Purchase.Date, tz="NZ") > as.POSIXct(max_purch$date, tz="NZ"),], paste0(client,"_new"))
    
    print(paste0("Transactions stored for ",client))
  } else {print(paste0("No transactions for ",client))}
    
}

print(paste0("Transaction storage complete at ",Sys.time()))
