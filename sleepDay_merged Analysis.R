
#sleepDay_merged Data Frame Analysis Code

#The following analyzes sleep information.

#---------------------------------------------------------------------#

#a)Create Backup Copy Of Current Data Frame  -----------------------------

  sleepDay_merged_df <- data.frame(sleepDay_merged_data_SQL_CLEAN)
  
#---------------------------------------------------------------------#

#b)Check Column Data Types -----------------------------------------------

  sapply(sleepDay_merged_df, class)
  
#---------------------------------------------------------------------#

#c)Convert Column To Appropriate Data Type -------------------------------

#i)Convert Id column to character
  
  sleepDay_merged_df$Id <- as.character(sleepDay_merged_df$Id)
  
#ii)Split sleepDay column into separate Date and Time columns. Then delete SleepDay column.
  
  sleepDay_merged_df$SleepDay <- as.POSIXct(sleepDay_merged_df$SleepDay, format = "%Y-%m-%d %H:%M:%S")
  
  x_date <- as.Date(sleepDay_merged_df$SleepDay)
  y_time <- as_hms(sleepDay_merged_df$SleepDay)
  
  sleepDay_merged_df <- mutate(sleepDay_merged_df, ActivityDate = x_date)
  sleepDay_merged_df <- mutate(sleepDay_merged_df, Time = y_time)
  
  sleepDay_merged_df <- select(sleepDay_merged_df, -c(SleepDay))
  sleepDay_merged_df <- select(sleepDay_merged_df, -c(Row_id_sleep))
  
#---------------------------------------------------------------------#

#d)Check Number Of Distinct Respondents ----------------------------------

  n_distinct(sleepDay_merged_df$Id)
  
#---------------------------------------------------------------------#

#e)Number Of Observations (Rows) -----------------------------------------

  nrow(sleepDay_merged_df)
  
#---------------------------------------------------------------------#

#f)Summary Statistics ----------------------------------------------------

  sleepDay_merged_df %>% select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>% summary()
  
#---------------------------------------------------------------------#

#g)Check For Normal Distribution -----------------------------------------

#i)Check if data are normally distributed, using histograms
    
  hist.data.frame(sleepDay_merged_df)
  
#ii)The following variables' histograms appear curved. They were checked for normality
  
  #TotalMinutesAsleep - approximately normally distributed
  ggplot(data=sleepDay_merged_df, mapping=aes(x=TotalMinutesAsleep))+
    geom_histogram(aes(y =..density..), bins = 30)+
    geom_density(col="green", size = 2)+
    ylab("Density")

  #TotalTimeInBed - approximately normally distributed
  ggplot(data=sleepDay_merged_df, mapping=aes(x=TotalTimeInBed))+
    geom_histogram(aes(y =..density..), bins = 30)+
    geom_density(col="green", size = 2)+
    ylab("Density")
  
  #TotalSleepRecords - approximately not normally distributed
  ggplot(data=sleepDay_merged_df, mapping=aes(x=TotalSleepRecords))+
    geom_histogram(aes(y =..density..), bins = 30)+
    geom_density(col="green", size = 2)+
    ylab("Density")
  
#---------------------------------------------------------------------#

#h)Total Time In Bed vs Total Minutes Asleep -----------------------------

#i)Compare TotalTimeInBed with TotalMinutesAsleep
  
  ggplot(data=sleepDay_merged_df, mapping=aes(x=TotalMinutesAsleep, y=TotalTimeInBed))+
    geom_point(color="navy")+
    geom_smooth(method=lm)
  
#ii)Determine Correlation Coefficient. Use Pearson method as variables appear to be normally distributed
  
  cor(sleepDay_merged_df$TotalTimeInBed, sleepDay_merged_df$TotalMinutesAsleep, method=c("pearson"))
  
#iii)Test significance of Pearson coefficient
  
  cor.test(sleepDay_merged_df$TotalTimeInBed, sleepDay_merged_df$TotalMinutesAsleep, alternative="two.sided", method="pearson", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------------#

#i)Merge With Other Tables -----------------------------------------------

#   Merge dailyActivity_merged_df and sleepDay_merged_df. Since there are more respondents in dailyActivity_merged than sleepDay_merged,
#   the respondents that are NOT in sleepDay_merged, will be removed in the merged dataset. To ensure the dates
#   are correct in the combined dataset, merge was done on both Id and ActivityDate
  
  sleepDay_dailyActivity_combined <- merge(sleepDay_merged_df, dailyActivity_merged_df, by=c("Id","ActivityDate"))


   


 
