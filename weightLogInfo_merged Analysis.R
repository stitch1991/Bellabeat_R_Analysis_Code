
#weightLogInfo_merged Data Frame Analysis Code

#The following analyzes weight information

#---------------------------------------------------------------------------#

#a)Create Backup Copy Of Current Data Frame -----------------------------

  weightLogInfo_merged_df <- weightLogInfo_merged_SQLCLEANED

#---------------------------------------------------------------------------#

#b)Check Column Data Types -----------------------------------------------

  sapply(weightLogInfo_merged_df, class)

#---------------------------------------------------------------------------#

#c)Convert Column To Appropriate Data Type -------------------------------

#i)Convert Id column to character
  
  weightLogInfo_merged_df$Id <- as.character(weightLogInfo_merged_df$Id)

#ii)Split Date column into separate Date and Time columns. Then delete Date column.
  
  weightLogInfo_merged_df$Date <- as.POSIXct(weightLogInfo_merged_df$Date, format = "%Y-%m-%d %H:%M:%S")

  x2_date <- as.Date(weightLogInfo_merged_df$Date)
  y2_time <- as_hms(weightLogInfo_merged_df$Date)
  format(as.POSIXct(weightLogInfo_merged_df$Date), format="%H:%M:%S")

  weightLogInfo_merged_df <- mutate(weightLogInfo_merged_df, ActivityDate = x2_date)
  weightLogInfo_merged_df <- mutate(weightLogInfo_merged_df, Times = y2_time)

  weightLogInfo_merged_df <- select(weightLogInfo_merged_df, -c(Date))
  weightLogInfo_merged_df <- select(weightLogInfo_merged_df, -c(Row_id))

#---------------------------------------------------------------------------#

#d)Check Number Of Distinct Respondents  ---------------------------------

  n_distinct(weightLogInfo_merged_df$Id)

#---------------------------------------------------------------------------#

#e)Number Of Observations (Rows) -----------------------------------------

  nrow(weightLogInfo_merged_df)

#---------------------------------------------------------------------------#

#f)Summary Statistics ----------------------------------------------------

  weightLogInfo_merged_df %>% select(WeightKg, WeightPounds, BMI) %>% summary()

#---------------------------------------------------------------------------#

#g)Check For Normal Distribution -----------------------------------------

#i)Check if data are normally distributed, using histograms
  
  hist.data.frame(weightLogInfo_merged_df)
  
#ii)The following variables' histograms appear curved. They were then checked for normality
  
  #WeightKg
  ggplot(data=weightLogInfo_merged_df, mapping=aes(x=WeightKg))+
    geom_histogram(aes(y =..density..), bins = 30)+
    geom_density(col="green", size = 2)+
    ylab("Density")
    #INCLUDING OUTLIER - not normally distributed - bimodal and skewed right
  
  ggplot(data=WLI_DA_without_outlier_df, mapping=aes(x=WeightKg))+
    geom_histogram(aes(y =..density..), bins = 30)+
    geom_density(col="green", size = 2)+
    ylab("Density")
    #EXCLUDING OUTLIER - appears to be normally distributed and bimodal
  
  #BMI
  ggplot(data=weightLogInfo_merged_df, mapping=aes(x=BMI))+
    geom_histogram(aes(y =..density..), bins = 30)+
    geom_density(col="green", size = 2)+
    ylab("Density")
    #INCLUDING OUTLIER - not normally distributed - bimodal and skewed right
    
  ggplot(data=WLI_DA_without_outlier_df, mapping=aes(x=BMI))+
    geom_histogram(aes(y =..density..), bins = 30)+
    geom_density(col="green", size = 2)+
    ylab("Density")
    #EXCLUDING OUTLIER - appears to be normally distributed and bimodal
  
#---------------------------------------------------------------------------#

#h)BMI vs WeightKg, WITH OUTLIER -----------------------------------------

#i)Compare BMI with WeightKg, including outlier
  
  ggplot(data=weightLogInfo_merged_df, mapping=aes(x=WeightKg, y=BMI))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method as neither variables are normally distributed and includes a potential outlier
  
  cor(weightLogInfo_merged_df$WeightKg, weightLogInfo_merged_df$BMI, method=c("spearman"))

#---------------------------------------------------------------------------#

#i)BMI vs WeightKg, WITHOUT OUTLIER --------------------------------------

#i)Compare BMI with WeightKg, excluding outlier
  
  ggplot(data=WLI_DA_without_outlier_df, mapping=aes(x=WeightKg, y=BMI))+
    geom_point(color="navy")+
    geom_smooth(method=lm)
  
#ii)Determine Correlation Coefficient. Use Pearson method as variables appear to be normally distributed when outlier is excluded
  
  cor(WLI_DA_without_outlier_df$WeightKg, WLI_DA_without_outlier_df$BMI, method=c("pearson"))

#---------------------------------------------------------------------------#

#j)Merge With Other Tables -----------------------------------------------

#i)Merge with daily_Activity_merged.
#  To ensure the dates are correct in the combined dataset, merge was done on both Id and ActivityDate
  
  weightLogInfo_dailyActivity_combined <- merge(weightLogInfo_merged_df, dailyActivity_merged_df, by=c("Id","ActivityDate"))

#ii)Merge with sleepDay_merged
#   To ensure the dates are correct in the combined dataset, merge was done on both Id and ActivityDate
  
  weightLogInfo_sleepDay_combined <- merge(weightLogInfo_merged_df, sleepDay_merged_df, by=c("Id", "ActivityDate"))
  
  
