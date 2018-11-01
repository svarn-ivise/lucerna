#preparation
library(lubridate)
setwd('C:/Intercity/development')

past <- read.csv('data/cleaned_df.csv', na.strings = c("", "NA"),stringsAsFactors = FALSE)

new <- read.csv('data/cleaned.resnet.data.csv', na.strings = c("", "NA"),stringsAsFactors = FALSE)

Create <- readRDS("model/Create.RDS")

#Create Optimal price selector function
Create(new=new,past=past,initial.elasticity = 2 ,base_dir ='C:/Intercity/development')

#Create(new=NULL,past=past,initial.elasticity = 2 ,base_dir ='C:/Intercity/development')



