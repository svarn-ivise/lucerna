library(plumber)
library(RMySQL)
library(ranger)

##TEST
db.ip <- "mysql"

model <- readRDS("/models/rf.rds")
Price_recommendation <- readRDS("/models/dynamic.rds")

lm_with_Price <- attr(Price_recommendation,'model')
holidays <- attr(Price_recommendation,'holidays')
school <- attr(Price_recommendation,'school')
Price <- attr(Price_recommendation,'Price')
Version <- attr(Price_recommendation,'Version')


app <- function(df, table.name){

con <-  dbConnect(RMySQL::MySQL(),
                  username = "shane",
                  password = "S13240sx91",
                  host = db.ip,
                  dbname="dynamic",
                  port = 3306)

dbWriteTable(con, table.name, df, append = TRUE, row.names=F)

dbDisconnect(con)
}

#* @get /version
function(){
  return(Version)
}


#* @get /testModel
function(){
  return(model$num.trees)
}

#* @get /dynaprice
function(purchase.date, purchase.time, travel.date, travel.time, 
         cumulative,service,capacity,qty,buy.price=NULL,write=T){
  
  cumulative <- as.numeric(cumulative)
  travel.date <- as.Date(travel.date)
  purchase.date <- as.Date(purchase.date)
  service <- as.character(service)
  capacity <- as.numeric(capacity)
  purchase.datetime <- paste(purchase.date,purchase.time)
  
  price <- Price_recommendation(Travel.Date = travel.date,
                       Purchase.Date = purchase.date,
                       cumsum.previous = cumulative,
                       Service = service,
                       capacity = capacity)
  
  if(write){
    app(data.frame(Travel.Date = travel.date,
                   Purchase.Date = purchase.datetime,
                   Service = service,
                   Quantity = qty,
                   Price = buy.price,
                   time = travel.time), "transactions")
  }
  
  return(price)
  
  }

#* @get /updateModel
function(){
  model <<- readRDS("/models/rf.rds")
  return("Model updated")
}

#* @get /prediction
function(bookdt, traveldt, cumulative){
  
 rf.pred <- predictions(predict(model, data.frame(Lead.Time = as.numeric(as.Date(traveldt) - as.Date(bookdt)),
                                  Booking.Month = as.numeric(format(as.Date(bookdt),"%m")),
                                  Travel.Month = as.numeric(format(as.Date(traveldt),"%m")),
                                  Travel.Year = as.numeric(format(as.Date(traveldt),"%Y")),
                                  Travel.Weekday = as.numeric(format(as.Date(traveldt),"%w")),
                                  Cumulative = cumulative)))
  return(rf.pred)
  
  }

n <- 100
start <- .1
end <- .5
x <- seq(from=.0, to=.5, by=(end-start)/n)
y <- 100 + -log10(x)*20 #+ rnorm(length(x),sd=.75)
price_model <- lm(data=data.frame(x, y)[-1,], 
            formula=y ~ poly(x, 2, raw=TRUE))

#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#' Return msg
#' @get /dynamic
function(date,seats,searches){
  
  searches <- as.numeric(searches)#300 #sample(500:1500,1)
  
  if(searches > 1000||!is.numeric(searches)){
    return("Invalid Search Input")
  }
  
  date <- as.Date(date)
  seats <- as.numeric(seats)
  
  days <- as.numeric(date - Sys.Date())
  cr <- 1/days*(seats/searches)
  x <- list(x=as.numeric(cr))
  price <- as.numeric(predict(price_model, x))
  
  df <- data.frame(Date = c(date), Seats = c(seats) , Days = c(days), Conversion = c(cr), Price = c(price))
  app(df, "dynamic")
            
  return(list(cr=cr,price=price))
  #return(paste0("To attain a conversion ratio of ",cr,", Price should be equal to $",price))
}


#' Return randomPrice
#' @get /random
function(load, upper, lower, base, size){
  
  load <- as.numeric(load) #.5 #rep(seq(from=.01,to=1,by=.01),4)
  size <- as.numeric(size)
  basePrice <- 103.50 + (load * 20) - 10
  upperBound <- as.numeric(upper)
  lowerBound <- as.numeric(lower)
  
  multiplier <- -(lowerBound) + runif(size, min=0, max=1)*(upperBound + lowerBound)
  df <- data.frame(price = basePrice * (1 + multiplier), load = load)
  
  return(list(price = df$price, load = df$load))
}

#' Return randomPrice
#' @get /price
function(date){

}

#' Return randomPrice
#' @get /purchase
function(quantity, date){
  
  purch.date <- as.Date(date)
  
  df <- data.frame(Date = c(purch.date), Quantity = c(quantity) , Time = c(Sys.time()))
  
  app(df, "trans")
  
  return(paste0("Purchase complete; ",quantity," seats bought for ",date))
  
}
