
############################ Functions #################################
## Function: return updated input indices ##
inx <- function (data, inp.n) { # data: current dataframe; inp.n: position for non-inputs
  # numeric input indicator
  indx <- sapply(data, is.numeric)
  indx[inp.n]<-FALSE
  
  # nominal input indicator
  index.cat<-sapply(data, is.factor)
  index.cat[inp.n]<-FALSE
  
  # missing value indicator
  index.na<-sapply(data,function(x) any(is.na(x)))
  index.na[inp.n]<-FALSE
  
  data.frame(indx, index.cat, index.na)
}

######################### Exploratory Data Analysis ###################

model <- read.csv("~/Data Mining/Project/modeling_data.csv", header=TRUE)
dim(model)#23459 rows, 93 columns
summary(as.factor(model$Book_12Mo))#18,207 didn't rebook within the next 12 months, 5,252 did rebook
5252/length(model$Book_12Mo)#22.4 % of customers book within the next 12 months



#Combining good and excellent into 3 or over
model$Hotels_3orOver<-model$Good_Hotels + model$Excellent_Hotels
model$Meals_3orOver<-model$Good_Meals + model$Excellent_Meals
model$GUSS_3orOver <-model$Good_GUSS + model$Excellent_GUSS
model$Optionals_3orOver <-model$Good_Optionals + model$Excellent_Optionals

summary(model$TourPriceCat)

names(model[indx])#55 numeric and ordinal 
names(model[index.cat])#33 categorical

library(plyr)
summary(model$TourPriceCat)

#Renaming the levels of TourPriceCat
levels(model$TourPriceCat) <- list("1"="Under 2000", "2"="2000 - 2500", "3"="2501 - 3000", "4"="3001 - 3500", "5"="3501 - 4000", "6"="4001 - 4500", "7"="4501 - 5000", "8"="More than 50")

#Creating new variables for returning and outbound connect time
model$Return_Connect_Time_Mins <- model$Return_Connect_Time_Mins_1 + model$Return_Connect_Time_Mins_2
model$Outbound_Connect_Time_Mins <- model$Outbound_Connect_Time_Mins_1 + model$Outbound_Connect_Time_Mins_2

#Categorizing all the departure and arrival times as morning, afternoon, evening and night. If you'd like to write a function, please do so!
model$Domestic_Arrival_Hour <-format(as.POSIXct(model$Domestic_Arrival_Time, tz = "" , format = "%H: %M"), "%H")
model$Domestic_Arrival_Hour <- as.numeric(model$Domestic_Arrival_Hour)
model$Domestic_Arrival_Time_of_Day <- ifelse(model$Domestic_Arrival_Hour >= 05 & model$Domestic_Arrival_Hour <=12, "Morning", ifelse(model$Domestic_Arrival_Hour > 12 & model$Domestic_Arrival_Hour <= 16, "Afternoon", ifelse(model$Domestic_Arrival_Hour > 16 & model$Domestic_Arrival_Hour <= 20, "Evening", "Night")))


model$Domestic_Dep_Hour <-format(as.POSIXct(model$Domestic_Depart_Time, tz = "" , format = "%H: %M"), "%H")
model$Domestic_Dep_Hour <- as.numeric(model$Domestic_Dep_Hour)
model$Domestic_Dep_Time_of_Day <- ifelse(model$Domestic_Dep_Hour >= 05 & model$Domestic_Dep_Hour <=12, "Morning", ifelse(model$Domestic_Dep_Hour > 12 & model$Domestic_Dep_Hour <= 16, "Afternoon", ifelse(model$Domestic_Dep_Hour > 16 & model$Domestic_Dep_Hour <= 20, "Evening", "Night")))

model$Intr_Arrival_Hour <-format(as.POSIXct(model$Intr_Arrival_Time, tz = "" , format = "%H: %M"), "%H")
model$Intr_Arrival_Hour <- as.numeric(model$Intr_Arrival_Hour)
model$Intr_Arrival_Time_of_Day <- ifelse(model$Intr_Arrival_Hour >= 05 & model$Intr_Arrival_Hour <=12, "Morning", ifelse(model$Intr_Arrival_Hour > 12 & model$Intr_Arrival_Hour <= 16, "Afternoon", ifelse(model$Intr_Arrival_Hour > 16 & model$Intr_Arrival_Hour <= 20, "Evening", "Night")))


model$Intr_Dep_Hour <-format(as.POSIXct(model$Intr_Depart_Time, tz = "" , format = "%H: %M"), "%H")
model$Intr_Dep_Hour <- as.numeric(model$Intr_Dep_Hour)
model$Intr_Dep_Time_of_Day <- ifelse(model$Intr_Dep_Hour >= 05 & model$Intr_Dep_Hour <=12, "Morning", ifelse(model$Intr_Dep_Hour > 12 & model$Intr_Dep_Hour <= 16, "Afternoon", ifelse(model$Intr_Dep_Hour > 16 & model$Intr_Dep_Hour <= 20, "Evening", "Night")))

#Dropping extraneous columns...
#columns to be dropped: end in Hour, end in Gateway, end in ID, end in Time, end in Gateway1, end in Gateway2, 

#this code unfortunately didn't work...maybe someone can find a better way of doing it...
drop <- c(model$Intr_Arrival_Hour, model$Intr_Dep_Hour, model$Domestic_Arrival_Hour, model$Domestic_Dep_Hour, model$Intr_Arrival_Time, model$Intr_Depart_Time, model$Domestic_Arrival_Time, model$Domestic_Depart_Time, model$EvalID, model$Cus_ID, model$ProdTour_ID, model$SalesTourID, model$Trip_no, model$HH_ID, model$Grp_Size_Cat, model$Return_Connect_Gateway1, model$Return_Connect_Gateway2, model$Outbound_Connect_Gateway1, model$Outbound_Connect_Gateway2, model$Intr_Arrival_Hour, model$Intr_Dep_Hour, model$Domestic_Dep_Hour, model$Domestic_Arrival_Hour, model$Intr_Arrival_Time, model$Intr_Depart_Time, model$Domestic_Depart_Time, model$Domestic_Arrival_Time, model$Intr_Arrival_Time, model$Outbound_Connect_Gateway1, model$Outbound_Connect_Gateway2, model$Outbound_Domestic_Gateway, model$Return_Intr_Gateway, model$Return_Domestic_Gateway, model$Return_Connect_Gateway1, model$Return_Connect_Gateway2, model$Outbound_Connect_Gateway1, model$Outbound_Connect_Gateway2, model$Outbound_Connect_Time_Mins_1, model$Outbound_Connect_Time_Mins_2)




                                         
