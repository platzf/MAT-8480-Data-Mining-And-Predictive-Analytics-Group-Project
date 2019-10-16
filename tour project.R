
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

#Preparing data and drop variables
library(readr)
library(dplyr)
tour <- read_csv("/modeling_data.csv")
drop.vars <- c(grep("ID$|Trip_no|Grp_Size_Cat|^(Poor|Fair|Good|Excellent)", names(tour)))
tour %>%
  mutate("Hotel_3orAbove" = Good_Hotels + Excellent_Hotels,
         "Meals_3orAbove" = Good_Meals + Excellent_Meals,
         "GUSS_3orAbove" = Good_GUSS + Excellent_GUSS,
         "Optionals_3orAbove" = Good_Optionals + Excellent_Optionals,
         "Bus_3orAbove" = Good_Buses + Excellent_Buses) %>%
  select(-drop.vars, -contains("Gateway")) %>%
  select(1:31, 59:63, everything()) -> tour

# Allocating variables
## Temporal variables
tour %>% select(1:2, 5:7, 13, 53:62) -> james

## Demographical/geographical/info variables
tour %>% select(3:4, 8:12, 37:45) -> faith

## Evaluation variables
tour %>% select(22:36) -> irina

## Binary and some overall evaluation variables
tour %>% select(14:21, 46:52, 63) -> alex  
