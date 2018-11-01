#preparation

setwd('C:/Intercity/Development/model')


#Load function
Price_recommendation <- readRDS("Price_recommendation.Rds")

lm_with_Price <- attr(Price_recommendation,'model') 
holidays <- attr(Price_recommendation,'holidays') 
school <- attr(Price_recommendation,'school') 
Price <- attr(Price_recommendation,'Price') 
Version <- attr(Price_recommendation,'Version') 


#Fare Recommendation
Price_recommendation(Travel.Date ='2018-11-12',Purchase.Date = Sys.Date()-1,  cumsum.previous = 4,Service = 'ICZMZM',capacity = 15)


