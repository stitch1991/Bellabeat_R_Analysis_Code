
#minuteMETsNarrow_merged Data Analysis Code

#The following analyzes METs per minute information

#---------------------------------------------------------------#

#a)Create Backup Data Frame Of Data Set -----------------------------------

  minuteMETs_df <- data.frame(minuteMETsNarrow_merged_SQL.CLEANED)

#---------------------------------------------------------------#

#b)Check Column Data Types -----------------------------------------------

  sapply(minuteMETs_df, class)

#---------------------------------------------------------------#

#c)Set Columns To Appropriate Data Types ---------------------------------

#i) Convert Id column to character
  
  minuteMETs_df$Id <- as.character(minuteMETs_df$Id)

#ii)Split ActivityMinute into separate Date and Time columns. 
#   Then remove ActivityMinute column and Row_id_MET column
  
  minuteMETs_df$ActivityMinute <- as.POSIXct(minuteMETs_df$ActivityMinute, format = "%Y-%m-%d %H:%M:%S")

  x3_date <- as.Date(minuteMETs_df$ActivityMinute)
  y3_time <- as_hms(minuteMETs_df$ActivityMinute)

  minuteMETs_df <- mutate(minuteMETs_df, ActivityDate = x3_date)
  minuteMETs_df <- mutate(minuteMETs_df, Time = y3_time)

  minuteMETs_df <- select(minuteMETs_df, -c(ActivityMinute))
  minuteMETs_df <- select(minuteMETs_df, -c(Row_id_MET))

#---------------------------------------------------------------#

#d)Average Daily METs ----------------------------------------------------

#i)Calculate daily average METs for each respondent

  mean_daily_METS_average <- aggregate(METs ~ ActivityDate + Id, data = copy_df, FUN = mean, na.rm = TRUE)

#ii)Code to check that daily averages were calculated correctly. Randomly selecting a respondent Id and 
#   checking that the calculated average is right. 
  
  check1 <- subset(copy_df, Id == "5553957443" & ActivityDate == "2016-05-10")
  mean(check1$METs)

#---------------------------------------------------------------#

#e)Check For Normal Distribution -----------------------------------------

#i)Check if METs (daily average) is normally distributed

  ggplot(data=mean_daily_METS_average, mapping=aes(x=METs))+
    geom_histogram(aes(y =..density..), bins = 30)+
    geom_density(col="green", size = 2)+
    ylab("Density")
    # Not normally distributed - skewed right

#---------------------------------------------------------------#

#f)Merge With Other Tables -----------------------------------------------

#i)Merge mean_daily_METs_average with dailyActivity_merged table. To ensure dates are correct, merge 
#  on both Id and ActivityDate
  
  meanMETs_dailyActivity_combined <- merge(mean_daily_METS_average, dailyActivity_merged_df, by=c("Id","ActivityDate"))

#ii)Merge sleepDay_METs_average with sleepDay_merged_df table. To ensure dates are correct, merge 
#  on both Id and ActivityDate
  
  meanMETs_sleepDay_combined <- merge(sleepDay_merged_df, mean_daily_METS_average, by=c("Id","ActivityDate"))
  
  