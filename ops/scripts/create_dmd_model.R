rm(list=ls())

#preparation
library(dplyr)
library(lubridate)
library(RMySQL)
library(keras)
library(aws.s3)

#if(is.null(nrow(new))){new <- past[0,]}

client_name <- "intercity"

build_stats <- function(total1){
  
  holidays <- qry('select * from holiday')
  school <- qry('select * from school')
  
  z <- total1 %>%
    filter(Service == "ICZMZM") %>%
    select(-daydiff, cumsum.previous) %>% distinct()
  
  avg.dmd <- mean(z$final_cumsum)
  
  day_of_week <- lapply(unique(z$day.of.week), function(w){
    mean(z$final_cumsum[z$day.of.week==w])/avg.dmd - 1
  })
  month_dmd <- lapply(sort(unique(as.numeric(z$month))), function(w){
    mean(z$final_cumsum[z$month==as.character(w)])/avg.dmd - 1
  })
  
  (holiday <- mean( z$final_cumsum[z$public_holiday == "Yes" | z$school_holiday == "Yes"])/avg.dmd)
  (nonholiday <- mean( z$final_cumsum[z$public_holiday == "No" & z$school_holiday == "No"])/avg.dmd)
  
  daydiff <- total1 %>% filter(daydiff < 61) %>% group_by(daydiff) %>%
    summarise(cumsum = sum(final_cumsum -cumsum.previous)) %>%
    mutate(cumsum = cumsum/mean(cumsum)) %>% as.data.frame()
  
  return(list(daydiff = daydiff, holiday = holiday, 
              nonholiday = nonholiday, day_of_week = day_of_week, 
              month_dmd = month_dmd, holidays = holidays,
              school=school))
  
}
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
create_model <- function(){

#read external data
new <- qry("select * from intercity_new")
past <- qry("select * from intercity_past")
weather <- qry("select * from weather")
airport <- qry("select * from airport")

#qry("show tables;")
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
weather$date <- as.Date(weather$date)
airport$date <- as.Date(airport$date)

  #past dataset
  df1 <- past
  total <- rbind(df1,new) %>% arrange(Travel.Date,Purchase.Date,Service)
  
  #feature engineering
  total$month <- as.character(month(total$Travel.Date))
  total$day.of.week <- weekdays(total$Travel.Date)
  total$day.in.year <- yday(total$Travel.Date)
  total$daydiff <-  as.numeric(difftime(as.Date(total$Travel.Date), total$Purchase.Date, units="days"))
  total$day.of.month <- as.numeric(format(total$Travel.Date, "%d"))
  
  #cumsum
  total <- total %>% arrange(Travel.Date, Purchase.Date)
  total$cumsum <- ave(total$Quantity, total$Travel.Date, FUN = cumsum) 
  total$lag.Travel.Date <- lag(total$Travel.Date, 1)
  total$cumsum.previous <- lag(total$cumsum, 1)
  total$cumsum.previous <- ifelse(total$lag.Travel.Date != total$Travel.Date, 0 , total$cumsum.previous)
  total$cumsum.previous[1] <- 0
  total$lag.Travel.Date <- NULL

  #adding final_cumsum
  final_cumsum <- total %>% group_by(Travel.Date) %>% summarise(final_cumsum = sum(Quantity))
  total <- merge(x = total, y = final_cumsum, by = "Travel.Date", all.x = TRUE)
  total <- total[complete.cases(total), ]
  total <- total %>% arrange(Travel.Date, Purchase.Date)
  
  #adding holiday variable
  total$date <- as.Date(total$Travel.Date)
  total <- merge(x = total, y = holidays, by = "date", all.x = TRUE)
  
  #adding school variable
  total <- merge(x = total, y = school, by = "date", all.x = TRUE)
  
  #adding weather variable
 # total <- left_join(x = total, y = weather, by = "date", all.x = TRUE)

  #apply optimal elasticity
  total <- total %>% filter(Travel.Date < Sys.Date())

#get total1
if (as.numeric(difftime(max(total$Travel.Date), min(total$Travel.Date), units="days")) > 365*2) {
  
  if (length(unique(total$Service)) == 1) {
    total1 <<- total %>% filter(Travel.Date < Sys.Date()) %>%
      select(Travel.Date, daydiff,cumsum.previous, day.of.week, Price,month,day.of.month,final_cumsum,
             public_holiday,school_holiday)#,tempF,precipMM,windspeedKmph)
    
  }else{
    
    total1 <<- total %>% filter(Travel.Date < Sys.Date()) %>%
      select(Travel.Date, daydiff,cumsum.previous, day.of.week,Service, Price,month,day.of.month,final_cumsum,
             public_holiday,school_holiday)}#,tempF,precipMM,windspeedKmph)}
  
}else{
  
  if (length(unique(total$Service)) == 1) {
    total1 <<- total %>% filter(Travel.Date < Sys.Date()) %>%
      select(Travel.Date, daydiff,cumsum.previous, day.of.week, Price,final_cumsum,
             public_holiday,school_holiday,tempF,precipMM,windspeedKmph)
    
  }else{
    
    total1 <<- total %>% filter(Travel.Date < Sys.Date()) %>%
      select(Travel.Date, daydiff,cumsum.previous, day.of.week,Service, Price, month,day.of.month, final_cumsum,
             public_holiday,school_holiday) }#,tempF,precipMM,windspeedKmph)}
}

one_hot <- function(df, key) {
  key_col <- dplyr::select_var(names(df), !! rlang::enquo(key))
  df <- df %>% mutate(.value = 1, .id = seq(n()))
  df <- df %>% tidyr::spread_(key_col, ".value", fill = 0, sep = "_") %>% select(-.id)
}
numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}

######intercity
#train_df <- read.csv("G:/My Drive/Ivise/Clients/Intercity/Price Testing/Development/data (1)/train.csv", stringsAsFactors = F)
train_df <- total1
train_df$public_holiday <- ifelse(train_df$public_holiday=="No", 0, 1)
train_df$school_holiday <- if_else(train_df$school_holiday=="No", 0, 1)
#train_df$month <- as.numeric(train_df$month)
#train_df$month.cos <- cos((2*pi)/12*as.numeric(train_df$month))
train_df$day.of.month <- train_df$day.of.month/sapply(train_df$Travel.Date, numberOfDays)
train_df$Travel.Date <- NULL

#####One hot encoding
train_df <- one_hot(train_df, "day.of.week")
train_df <- one_hot(train_df, "Service")
train_df <- one_hot(train_df, "month")
train_df <<- train_df

train_rows <- sample(1:nrow(train_df),ceiling(.8*nrow(train_df)))
test_rows <- setdiff(1:nrow(train_df),train_rows)

train_labels <- as.numeric(train_df[train_rows,"final_cumsum"])
test_labels <- as.numeric(train_df[test_rows,"final_cumsum"])

train_data <- as.matrix(train_df[train_rows,!(names(train_df) %in% c("final_cumsum"))])
test_data <- as.matrix(train_df[test_rows,!(names(train_df) %in% c("final_cumsum"))])

scale.cols <- c("daydiff","cumsum.previous","Price") #,"tempF","precipMM","windspeedKmph")

# Normalize training data
col_means_train <<- attr(scale(train_data[,scale.cols]), "scaled:center")
col_stddevs_train <<- attr(scale(train_data[,scale.cols]), "scaled:scale")
train_data[,scale.cols] <- scale(train_data[,scale.cols]) 

# Use means and standard deviations from training set to normalize test set
test_data[,scale.cols] <- scale(test_data[,scale.cols],center = col_means_train, scale = col_stddevs_train)

build_model <- function() {
  
  model <- keras_model_sequential() %>%
    # layer_dense(units = 64, activation = "relu",
    #             input_shape = dim(train_data)[2]) %>%
    # layer_dense(units = 64, activation = "relu") %>%
    # layer_dense(units = 1)
    layer_dense(units = 64, activation = "relu",
                input_shape = dim(train_data)[2]) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1, activation= "linear")
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model <- build_model()
model %>% summary()

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 500

# Fit the model and store training stats
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)

library(ggplot2)

plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(ylim = c(0, 5))

# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

model <- build_model()
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)

plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(xlim = c(0, 150), ylim = c(0, 5))

c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 0))

print(paste0("Mean absolute error on test set: ", sprintf("%.2f", mae)))

attr(model, "sd") <- col_stddevs_train
attr(model, "mu") <- col_means_train
attr(model, "sample") <- train_df[1,!(names(train_df) %in% "final_cumsum")]

return(model)

}

model <- create_model()

stats <- build_stats(total1)
saveRDS(stats, "~/stats.rds")

    # Day.of.Week = stats$day_of_week[[dow]],
    # Travel.Month = stats$month_dmd[[travel.month]],
    # Lead.Time = stats$daydiff$cumsum[stats$daydiff$daydiff==daydiff] - 1,
    # School.Holiday = school_holiday - 1,
    # Public.Holiday = public_holiday - 1)

#results <- matrix(NA, nrow=60,ncol=16)
#results <- matrix(NA, nrow=101,ncol=16)

#scale.cols <- c("daydiff","cumsum.previous","Price","month")

#for(p in 100:200){
# for(lead in 1:60){
#   for(cum in 15:0){
#     sample_day <- attr(model, "sample")
#     sample_day$daydiff <- lead
#     sample_day$cumsum.previous <- cum
#     sample_day[,c("Service_ICZMZC","Service_ICZMZG", "Service_ICZMZM")] <- 0
#     sample_day$Service_ICZMZM <- 1
# # #    sample_day$Price <- p
#     sample_day$month <- 8
#     sample_day[,scale.cols] <- scale(sample_day[,scale.cols], center = attr(model, "mu"), scale = attr(model, "sd"))
# 
#     (pred <- model %>% predict(as.matrix(sample_day)))
#     results[lead,cum+1] <- pred
#     #results[p-99,cum+1] <- pred
#   }
# }
# 
# results <- as.data.frame(results)
# names(results) <- 0:15
# # #row.names(results) <- 100:200
# row.names(results) <- 1:60

#client_name <- "intercity"

model_name <- paste0(client_name,"_model_v",format(Sys.Date(),"%Y_%m_%d"),"_",format(Sys.time(),"%H"),".h5")
file_name <- paste0("/tmp/",model_name)
save_model_hdf5(model, file_name, include_optimizer = TRUE)

holidays <- qry("select * from holiday")
school <- qry("select * from school")

##model params
model_params <- list(
  sample = attr(model, "sample"),
  sd = attr(model,"sd"),
  mu = attr(model,"mu"),
  holidays = holidays,
  school = school
)

##push model to s3
put_object(file_name, bucket = "lucerna", object=model_name)

###Push params to S3
s3saveRDS(model_params, bucket = "lucerna", object=paste0("params_",model_name))

###update model and params on client server
system(paste0("~/lucerna/ops/scripts/get-models ",model_name," ",client_name))
