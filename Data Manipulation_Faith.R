# All changes to Faith's variables
tour <- mutate_if(tour, is.character, as.factor)

binning <- woe.binning(as.data.frame(tour), 'Book_12Mo', 
                       c('State', 'TourCode', 'Tour_Region', 'SourceType'),
                       min.perc.total=0.05, min.perc.class=0.01, stop.limit=0.1)

tour.bin <- woe.binning.deploy(as.data.frame(tour), binning, add.woe.or.dum.var = 'woe')

levels(tour.bin$State.binned) <- c("NY+CA+misc", "Other", "PA+FL", NA)
levels(tour.bin$TourCode.binned) <- c("Other", "VFR+MIT+misc", "VIB", NA)
levels(tour.bin$Tour_Region.binned) <- c("FS+MD+CNE+AM+misc", "Other", "BI+IT+EU+EC", NA)
levels(tour.bin$SourceType.binned) <- c("Other", "Internet+Referral", "Old Src IDs+misc", NA)

tour.xf <- tour.bin %>%
  # change all character variables into factors
  mutate_if(is.character, as.factor) %>%
  mutate(Book_12Mo = as.factor(Book_12Mo)) %>%
         # fix typo in TourPriceCat
  mutate(TourPriceCat = as.factor(if_else(as.character(TourPriceCat) == "More than 50", 
                                          "More than 5000", as.character(TourPriceCat))),
         # turn Past_Trips into numeric variable
         Past_Trips = ifelse(as.character(Past_Trips) == "0 Trips", 0,
                      ifelse(as.character(Past_Trips) == "1 Trip", 1,
                      ifelse(as.character(Past_Trips) == "2 Trips", 2,
                      ifelse(as.character(Past_Trips) == "3 Trips", 3,
                      ifelse(as.character(Past_Trips) == "4 Trips", 4, NA))))),
         Book_Months_num = ifelse(as.numeric(Book_Months) == 1, 4,
                           ifelse(as.numeric(Book_Months) == 4, 5,
                           ifelse(as.numeric(Book_Months) == 5, 1, as.numeric(Book_Months)))),
         Age_num = ifelse(as.numeric(Age) == 1, 2,
                   ifelse(as.numeric(Age) == 2, 3,
                   ifelse(as.numeric(Age) == 3, 4,
                   ifelse(as.numeric(Age) == 4, 5,
                   ifelse(as.numeric(Age) == 5, 6,
                   ifelse(as.numeric(Age) == 6, 7,
                   ifelse(as.numeric(Age) == 7, 8,
                   ifelse(as.numeric(Age) == 8, NA,
                   ifelse(as.numeric(Age) == 9, 9,
                   ifelse(as.numeric(Age) == 10, 1, NA)))))))))),
         DB_Enter_Months_num = ifelse(as.numeric(DB_Enter_Months) == 1, 2,
                               ifelse(as.numeric(DB_Enter_Months) == 2, 3,
                               ifelse(as.numeric(DB_Enter_Months) == 3, 4,
                               ifelse(as.numeric(DB_Enter_Months) == 4, 1, NA)))),
         TourPriceCat_num = ifelse(as.numeric(TourPriceCat) == 1, 2,
                            ifelse(as.numeric(TourPriceCat) == 2, 3,
                            ifelse(as.numeric(TourPriceCat) == 3, 4,
                            ifelse(as.numeric(TourPriceCat) == 4, 5,
                            ifelse(as.numeric(TourPriceCat) == 5, 6,
                            ifelse(as.numeric(TourPriceCat) == 6, 7,
                            ifelse(as.numeric(TourPriceCat) == 7, 8,
                            ifelse(as.numeric(TourPriceCat) == 8, 1, NA))))))))) %>%
  # if email is missing, make it unavailable
  mutate(Email = as.factor(if_else(is.na(as.character(Email)), "Unavailable", as.character(Email))))


# now, impute the Age variable with the mode
tour.imp <- tour.xf

impute <- function(x,y) {
  x[is.na(x)]<-y
  x
}

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode<-sapply(tour.imp["Age_num"],mode)

tour.imp["Age_num"]<-as.data.frame(mapply(impute,x=tour.xf["Age_num"],y = Mode))
