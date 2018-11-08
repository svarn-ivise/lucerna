rm(list=ls())

library(aws.s3)

config.file <- readLines('~/.aws/credentials')
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = gsub(".*= ","",config.file[config.file %>% grepl("aws_access_key",.)]),
  "AWS_SECRET_ACCESS_KEY" = gsub(".*= ","",config.file[config.file %>% grepl("aws_secret_access_key",.)]),
  "AWS_DEFAULT_REGION" = "us-west-2")

qry <- function(string){
  
  library(RMySQL)
  hst <-"dynamic.cvzzuzs7svpf.us-west-2.rds.amazonaws.com"
  usr <- "ivise"
  pwd <- "ivise2018"
  dbTab <- "lucerna"
  prt <- 3306
  
  con <- dbConnect(dbDriver("MySQL"), host=hst, user=usr, password=pwd ,db= dbTab, port=prt)
  
  result<- dbGetQuery(con, paste0(string))
  
  dbDisconnect(con)
  
  all_cons <- dbListConnections(MySQL())
  for(cons in all_cons){dbDisconnect(cons)}
  
  return(result)
  
}
Create <- function(new,past,client_name,initial_elasticity,goal) {
  
  #preparation
  library(dplyr)
  library(lubridate)
  library(RMySQL)
  
  
  if(is.null(nrow(new))){new <- past[0,]}
  
  #read external data
  master <- qry('select * from master') %>% filter(client ==client_name)
  Price <- qry('select * from price') %>% filter(client ==client_name) %>% select(-client)
  holidays <- qry('select * from holiday') %>% filter(country ==master$country) %>% select(-country)
  school <- qry('select * from school') %>% filter(country ==master$country) %>% select(-country)
  
  if (!is.na(master$weather)){weather <- qry('select * from weather') %>% filter(client ==client_name) %>% select(-client)}
  if (!is.na(master$airport)){airport <- qry('select * from airport') %>% filter(airport ==master$airport) %>% select(-airport)}
  
 #change Character to Date format
  new$Travel.Date <- as.Date(new$Travel.Date)
  past$Travel.Date <- as.Date(past$Travel.Date)
  new$Purchase.Date <- as.Date(new$Purchase.Date)
  past$Purchase.Date <- as.Date(past$Purchase.Date)
  
  holidays$date <- as.Date(holidays$date)
  school$date <- as.Date(school$date)
  
  ##GROUP BY DATES
  new <- new %>% group_by(Travel.Date, Purchase.Date, Service) %>% 
    summarise(booking_per_day = sum(booking_per_day), Price = mean(Price) ) %>% as.data.frame()
  
  old <- old %>% group_by(Travel.Date, Purchase.Date, Service) %>% 
    summarise(booking_per_day = sum(booking_per_day), Price = mean(Price) ) %>% as.data.frame()
  
  #Modify data function
  get_total <- function(elasticity){
    #past dataset
    df1 <- past
    
    #modify past dataset
    final_cumsum <- df1 %>% group_by(Travel.Date) %>% summarise(final_cumsum = sum(booking_per_day))
    df1 <- merge(x = df1, y = final_cumsum, by = "Travel.Date", all.x = TRUE)
    df1 <- df1[complete.cases(df1), ]
    
    df1 <- df1 %>% arrange(Travel.Date, Purchase.Date)
    
    df1$elasticity <- (2 * mean(df1$final_cumsum))/df1$final_cumsum
    
    df1$scaled.elasticity <- (df1$elasticity-min(df1$elasticity))/(max(df1$elasticity)-min(df1$elasticity))
    df1$scaled.elasticity <- df1$scaled.elasticity+elasticity
    
    df1$modified.price <- NA
    for (i in 1:nrow(df1)){  
      df1$modified.price[i] <- df1$Price[i] * runif(1, 0.8, 1.2)}
    
    df1$modified_booking_per_day <- df1$booking_per_day * (1+(((df1$modified.price - df1$Price)/df1$Price)*-df1$scaled.elasticity))
    
    df1 <- df1 %>% select(Travel.Date,Purchase.Date,Service,'booking_per_day'= modified_booking_per_day,'Price' =modified.price ,time )
    
    #merge new and total
    
    total <- rbind(df1,new) %>% arrange(Travel.Date,Purchase.Date,Service)
    
    #feature engineering
    
    total$month <- as.character(month(total$Travel.Date))
    total$day.of.week <- weekdays(total$Travel.Date)
    total$day.in.year <- yday(total$Travel.Date)
    
    total$daydiff <-  as.numeric(difftime(as.Date(total$Travel.Date), total$Purchase.Date, units="days"))
    
    #cumsum
    total <- total %>% arrange(Travel.Date, Purchase.Date)
    
    total$cumsum <- ave(total$booking_per_day, total$Travel.Date, FUN = cumsum) 
    
    total$lag.Travel.Date <- lag(total$Travel.Date, 1)
    
    total$cumsum.previous <- lag(total$cumsum, 1)
    
    total$cumsum.previous <- ifelse(total$lag.Travel.Date != total$Travel.Date, 0 , total$cumsum.previous)
    
    total$cumsum.previous[1] <- 0
    
    total$lag.Travel.Date <- NULL
    
    #adding final_cumsum
    
    final_cumsum <- total %>% group_by(Travel.Date) %>% summarise(final_cumsum = sum(booking_per_day))
    total <- merge(x = total, y = final_cumsum, by = "Travel.Date", all.x = TRUE)
    total <- total[complete.cases(total), ]
    
    total <- total %>% arrange(Travel.Date, Purchase.Date)
    
    #adding holiday variable
    total$date <- as.Date(total$Travel.Date)
    total <- merge(x = total, y = holidays, by = "date", all.x = TRUE)
    
    #adding school variable
    total <- merge(x = total, y = school, by = "date", all.x = TRUE)
    total <- total %>% filter(Travel.Date < Sys.Date())
    return(total)}
  
  #find optimal elasticity
  if (nrow(new)>0) {
    
    e <- data.frame(elasticity = as.numeric() , MAE = as.numeric())
    
    for ( n in 1:100){
      
      total <- get_total(n/10)
      
      #get total1
      if (as.numeric(difftime(max(total$Travel.Date), min(total$Travel.Date), units="days")) > 365) {
        
        if (length(unique(total$Service)) == 1) {
          total1 <- total %>% filter(Travel.Date < Sys.Date()) %>%
            select(daydiff,cumsum.previous, day.of.week, Price,month,final_cumsum,public_holiday,school_holiday,Purchase.Date)
          
        }else{
          
          total1 <- total %>% filter(Travel.Date < Sys.Date()) %>%
            select(daydiff,cumsum.previous, day.of.week,Service, Price,month,final_cumsum,public_holiday,school_holiday,Purchase.Date)}
        
        
      }else{
        
        if (length(unique(total$Service)) == 1) {
          total1 <- total %>% filter(Travel.Date < Sys.Date()) %>%
            select(daydiff,cumsum.previous, day.of.week, Price,final_cumsum,public_holiday,school_holiday,Purchase.Date)
          
        }else{
          
          total1 <- total %>% filter(Travel.Date < Sys.Date()) %>%
            select(daydiff,cumsum.previous, day.of.week,Service, Price,final_cumsum,public_holiday,school_holiday,Purchase.Date)}
      }
      
      #split data
      
      train_data <- total1 %>% filter(Purchase.Date  <= max(past$Purchase.Date)) %>% select(-Purchase.Date)
      
      test_data <- total1 %>% filter(Purchase.Date  > max(past$Purchase.Date)) %>% select(-Purchase.Date)
      
      #evaluation
      #lm
      lm <- lm(final_cumsum ~., train_data)
      test_data$prediction <- predict(lm, test_data)
      
      test_data$prediction <- ifelse(test_data$prediction < test_data$cumsum.previous, test_data$cumsum.previous, test_data$prediction)
      
      print(mean(abs(test_data$final_cumsum - test_data$prediction), na.rm=TRUE)) #2.19921
      
      cor(test_data$final_cumsum, test_data$prediction)  #0.6747503
      
      
      e1 <- data.frame(elasticity = n/10 , MAE = mean(abs(test_data$final_cumsum - test_data$prediction), na.rm=TRUE))
      e <- rbind(e,e1)
    }
    
    elasticity <-  e$elasticity[e$MAE ==min(e$MAE)]
    if (elasticity < 1) {elasticity <- 1}
    
  } else {elasticity <- initial_elasticity}
  
  #apply optimal elasticity
  total <- get_total(elasticity)
  total <- total %>% filter(Travel.Date < Sys.Date())
  #get total1
  if (as.numeric(difftime(max(total$Travel.Date), min(total$Travel.Date), units="days")) > 365) {
    
    if (length(unique(total$Service)) == 1) {
      total1 <- total %>% filter(Travel.Date < Sys.Date()) %>%
        select(daydiff,cumsum.previous, day.of.week, Price,month,final_cumsum,public_holiday,school_holiday)
      
    }else{
      
      total1 <- total %>% filter(Travel.Date < Sys.Date()) %>%
        select(daydiff,cumsum.previous, day.of.week,Service, Price,month,final_cumsum,public_holiday,school_holiday)}
    
  }else{
    
    if (length(unique(total$Service)) == 1) {
      total1 <- total %>% filter(Travel.Date < Sys.Date()) %>%
        select(daydiff,cumsum.previous, day.of.week, Price,final_cumsum,public_holiday,school_holiday)
      
    }else{
      
      total1 <- total %>% filter(Travel.Date < Sys.Date()) %>%
        select(daydiff,cumsum.previous, day.of.week,Service, Price,final_cumsum,public_holiday,school_holiday)}
  }
  
  lm_with_Price <- lm(final_cumsum~.,total1)
  
  #create Price_recommendation model
  
  if (goal == 'max_profit') {
    Price_recommendation <- function(Travel.Date,Purchase.Date,cumsum.previous,Service,capacity){
      # 
      # Travel.Date <- '2018-11-12'
      # cumsum.previous <- 11
      # Service <- 'ICZMZM'
      # capacity <- 15
      # Purchase.Date <- Sys.Date()-1
      
      test1 <- data.frame(date = as.Date(Travel.Date), Purchase.Date = as.Date(Purchase.Date) ,cumsum.previous = cumsum.previous , Service = Service)
      test1$daydiff <- as.numeric(difftime(test1$date, test1$Purchase.Date, units="days"))
      
      test1$month <- as.character(as.numeric(substr(test1$date,6,7)))
      
      test1$day.of.week <- weekdays(test1$date)
      test1$Service <- as.character(test1$Service)
      
      #add price and cost
      test1 <- merge(x = test1, y = Price, by = "Service", all.x = TRUE)
      
      #join holidays
      test1 <- merge(x = test1, y = holidays, by = "date", all.x = TRUE)
      test1 <- merge(x = test1, y = school, by = "date", all.x = TRUE)
      
      #find optimal price
      min <- round(ifelse(is.na(test1$Min),test1$Price/2 ,test1$Min))
      max <- round(ifelse(is.na(test1$Max),test1$Price*2 ,test1$Max))
      
      # min <- round(test1$Price/2)
      # max <- round(test1$Price*2)
      
      
      testing <- test1
      for (i in 1:(max-min)){
        testing <- rbind(testing, test1)
      }
      testing$Price <- min:max
      testing$prediction <- predict(lm_with_Price,testing)
      testing <- testing[testing$prediction>testing$cumsum.previous,]
      
      
      OP <- min(testing$Price[abs(testing$prediction -capacity) == min(abs(testing$prediction -capacity))])
      
      return(data.frame(Travel.Date = Travel.Date, Purchase.Date= Purchase.Date, cumsum.previous= cumsum.previous,Service=Service,capacity=capacity,OP=OP))
    }
  } else if (goal == 'reach_capacity'){
    Price_recommendation <- function(Travel.Date,Purchase.Date,cumsum.previous,Service,capacity){
      # 
      # Travel.Date <- '2018-11-12'
      # cumsum.previous <- 11
      # Service <- 'ICZMZM'
      # capacity <- 15
      # Purchase.Date <- Sys.Date()-1
      
      test1 <- data.frame(date = as.Date(Travel.Date), Purchase.Date = as.Date(Purchase.Date) ,cumsum.previous = cumsum.previous , Service = Service)
      test1$daydiff <- as.numeric(difftime(test1$date, test1$Purchase.Date, units="days"))
      
      test1$month <- as.character(as.numeric(substr(test1$date,6,7)))
      
      test1$day.of.week <- weekdays(test1$date)
      test1$Service <- as.character(test1$Service)
      
      #add price and cost
      test1 <- merge(x = test1, y = Price, by = "Service", all.x = TRUE)
      
      #join holidays
      test1 <- merge(x = test1, y = holidays, by = "date", all.x = TRUE)
      test1 <- merge(x = test1, y = school, by = "date", all.x = TRUE)
      
      #find optimal price
      min <- round(ifelse(is.na(test1$Min),test1$Price/2 ,test1$Min))
      max <- round(ifelse(is.na(test1$Max),test1$Price*2 ,test1$Max))
      
      # min <- round(test1$Price/2)
      # max <- round(test1$Price*2)
      
      
      testing <- test1
      for (i in 1:(max-min)){
        testing <- rbind(testing, test1)
      }
      testing$Price <- min:max
      testing$prediction <- predict(lm_with_Price,testing)
      testing <- testing[testing$prediction>testing$cumsum.previous,]
      
      
      OP <- min(testing$Price[abs(testing$prediction -capacity) == min(abs(testing$prediction -capacity))])
      
      return(data.frame(Travel.Date = Travel.Date, Purchase.Date= Purchase.Date, cumsum.previous= cumsum.previous,Service=Service,capacity=capacity,OP=OP))
    }
  } 
  
  attr(Price_recommendation,'model') <- lm_with_Price
  attr(Price_recommendation,'holidays') <- holidays
  attr(Price_recommendation,'school') <- school
  attr(Price_recommendation,'Price') <- Price
  attr(Price_recommendation,'Version') <- Sys.time()
  
  return(Price_recommendation)
  #saveRDS(Price_recommendation, file="model/Price_recommendation.RDS")
  
}

client_name <- "intercity"

past <- qry(paste0("select * from ",client_name,"_past")) 
new <- qry(paste0("select * from ",client_name,"_new")) 

#objectives are reach_capacity and max_profit
model <- Create(new=new,past=past,client_name = 'intercity',initial_elasticity = 3, goal = 'reach_capacity')

#model(Travel.Date=Sys.Date()+30,Purchase.Date = Sys.Date(),  cumsum.previous = 5,Service = 'ICZMZM',capacity = 15)

model_name <- paste0(client_name,"_model_v",format(Sys.Date(),"%Y_%m_%d"),"_",format(Sys.time(),"%H"),".rds")
file_name <- paste0("/tmp/",model_name)

###Push to S3
s3saveRDS(model, bucket = "lucerna", object=model_name)

