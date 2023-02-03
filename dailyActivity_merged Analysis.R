
#dailyActivity_merged Data Analysis Code

#The following analyzes daily activity information to identify potential trends

#---------------------------------------------------------------#

#a)Preview Data Set -----------------------------------------------------

  glimpse(dailyActivity_merged)

#---------------------------------------------------------------#

#b)Backup Data -----------------------------------------------------------

#i)Create backup copy of data frame
  
  dailyActivity_merged_df <- data.frame(dailyActivity_merged)
  
#ii)Preview data frame backup
  
  glimpse(dailyActivity_merged_df)
  
#---------------------------------------------------------------#

#c)Check Data Types Of Columns ------------------------------------------

  sapply(dailyActivity_merged_df, class)

#---------------------------------------------------------------#

#d)Set Columns To Correct Data Type -------------------------------------

#i)Convert Id column to character type
  dailyActivity_merged_df$Id <- as.character(dailyActivity_merged_df$Id)
  
#ii)Convert ActivityDate to datetime object
  
  dailyActivity_merged_df$ActivityDate <- as.Date(dailyActivity_merged_df$ActivityDate, "%Y-%m-%d")
  
  dailyActivity_merged_df$ActivityDate <- as.character(dailyActivity_merged_df$ActivityDate)
  
#---------------------------------------------------------------#

#e)Modify Global Options To See All Decimal Values ----------------------

  options(digits = 15)

#---------------------------------------------------------------#

#f)Check For Number Of Distinct Respondents ------------------------------

  n_distinct(dailyActivity_merged_df$Id)

#---------------------------------------------------------------#

#g)Number Of Observations (Rows) In The Table ----------------------------

  nrow(dailyActivity_merged_df)

#---------------------------------------------------------------#

#h)Summary Statistics ----------------------------------------------------

  dailyActivity_merged_df %>% select(TotalSteps, TotalDistance, SedentaryMinutes, c) %>% summary()

#---------------------------------------------------------------#

#i)Check For Normal Distribution --------------
  
#i)Check if data are normally distributed, using histograms
  
  hist.data.frame(dailyActivity_merged)

#The following variables show a curved distribution, so they were checked for normality
  
  #TotalSteps - not normally distributed - skewed left
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=TotalSteps))+
    geom_histogram(aes(y =..density..), bins = 30)+
    geom_density(col="green", size = 2)+
    ylab("Density")
  
  #TotalDistance - not normally distributed - skewed right
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=TotalDistance))+
    geom_histogram(aes(y =..density..), bins = 30)+
    geom_density(col="green", size = 2)+
    ylab("Density")
  
  #TrackerDistance - not normally distributed - skewed right
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=TrackerDistance))+
    geom_histogram(aes(y =..density..), bins = 30)+
    geom_density(col="green", size = 2)+
    ylab("Density")
  
  #LightActiveDistance - not normally distributed - slightly skewed left
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=LightActiveDistance))+
    geom_histogram(aes(y =..density..), bins = 30)+
    geom_density(col="green", size = 2)+
    ylab("Density")
  
  #LightlyActiveMinutes - approximately normally distributed
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=LightlyActiveMinutes))+
    geom_histogram(aes(y =..density..), bins = 30)+
    geom_density(col="green", size = 2)+
    ylab("Density")
  
  #SedentaryMinutes - not normally distributed - bimodal and skewed left
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=SedentaryMinutes))+
    geom_histogram(aes(y =..density..), binwidth = 30)+
    geom_density(col="green", size = 2)+
    ylab("Density")
  
  #Calories - approximately normally distributed and bimodal
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=Calories))+
    geom_histogram(aes(y =..density..), binwidth = 40)+
    geom_density(col="green", size = 2)+
    ylab("Density")
  
  #VeryActiveMinutes - not normally distributed - skewed right
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=VeryActiveMinutes))+
    geom_histogram(aes(y =..density..), binwidth = 40)+
    geom_density(col="green", size = 2)+
    ylab("Density")
  
  #VeryActiveDistance - not normally distributed - skewed right
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=VeryActiveDistance))+
    geom_histogram(aes(y =..density..), binwidth = 40)+
    geom_density(col="green", size = 2)+
    ylab("Density")
  
  #FairlyActiveMinutes - not normally distributed - skewed right
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=FairlyActiveMinutes))+
    geom_histogram(aes(y =..density..), binwidth = 40)+
    geom_density(col="green", size = 2)+
    ylab("Density")
  
  #ModeratelyActiveDistance - not normally distributed
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=ModeratelyActiveDistance))+
    geom_histogram(aes(y =..density..), binwidth = 40)+
    geom_density(col="green", size = 2)+
    ylab("Density")

#---------------------------------------------------------------#

#j)Total Steps vs Sedentary Minutes --------------------------------------

#i)Compare TotalSteps and SedentaryMinutes

  TotalSteps_SedentaryMinutes_scpl <- ggplot(data=dailyActivity_merged_df, aes(x=TotalSteps, y=SedentaryMinutes)) +
    geom_point(color="navy") +
    geom_smooth(method=lm, se=FALSE)

#ii)Determine Correlation Coefficient. Use Spearman method as neither variables are normally distributed
  
  cor(dailyActivity_merged_df$TotalSteps, dailyActivity_merged_df$SedentaryMinutes, method=c("spearman")) 
    
#iii)Test the significance of Spearman correlation coefficient
  
  cor.test(dailyActivity_merged_df$TotalSteps, dailyActivity_merged_df$SedentaryMinutes, alternative="two.sided", method="spearman", exact=FALSE)

#---------------------------------------------------------------#

#k)Total Steps vs Calories -----------------------------------------------

#i)Compare TotalSteps and Calories

  TotalSteps_Calories_scplggplot <- ggplot(data=dailyActivity_merged_df, mapping=aes(x=TotalSteps, y=Calories)) + 
  geom_point(color="navy") +
  geom_smooth(method=lm, se=FALSE)

#ii)Determine Correlation Coefficient. Use Spearman method as TotalSteps is not normally distributed
    
  cor(dailyActivity_merged_df$TotalSteps, dailyActivity_merged_df$Calories, method=c("spearman")) 
    
#iii)Test the significance of Spearman correlation coefficient
    
  cor.test(dailyActivity_merged_df$TotalSteps, dailyActivity_merged_df$Calories, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------#

#l)Sedentary Minutes vs Calories -----------------------------------------

#i)Compare SedentaryMinutes to Calories

  ggplot(data=dailyActivity_merged_df, mapping=aes(x=SedentaryMinutes, y=Calories)) + 
    geom_point(color="navy") +
    geom_smooth(method=lm, se=FALSE)

#ii)Determine Correlation Coefficient. Use Spearman method as SedentaryMinutes is not normally distributed
  
  cor(dailyActivity_merged_df$SedentaryMinutes, dailyActivity_merged_df$Calories, method=c("spearman"))

#iii)Test the significance of Spearman correlation coefficient
    
  cor.test(dailyActivity_merged_df$SedentaryMinutes, dailyActivity_merged_df$Calories, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------#

#m)Very Active Minutes vs Calories ---------------------------------------

#i)Compare VeryActiveMinutes to Calories
  
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=VeryActiveMinutes, y=Calories)) + 
    geom_point(color="navy") +
    geom_smooth(method=lm, se=FALSE)
  
#ii)Mean number of minutes respondents spent being Very Active
    
  mean(dailyActivity_merged_df$VeryActiveMinutes)
  
#iii)Median number of minutes respondents spent being Very Active
    
  median(dailyActivity_merged_df$VeryActiveMinutes)
    
#iv)Range of Calories and Average Calories burned while being Very Active for LESS than 50 minutes
  
  less_than_50_VA <- filter(dailyActivity_merged_df, dailyActivity_merged_df$VeryActiveMinutes < 50)
  
  min(less_than_50_VA$Calories)
  
  max(less_than_50_VA$Calories)
  
  mean(less_than_50_VA$Calories)
  
#v)Range of Calories and Average Calories burned while being Very Active for MORE than 50 minutes
  
  more_than_50_VA <- filter(dailyActivity_merged_df, dailyActivity_merged_df$VeryActiveMinutes > 50)
  
  min(more_than_50_VA$Calories)
  
  max(more_than_50_VA$Calories)
  
  mean(more_than_50_VA$Calories)
  
#vi)Determine Correlation Coefficient. Use Spearman method as VeryActiveMinutes is not normally distributed
  
  cor(dailyActivity_merged_df$VeryActiveMinutes, dailyActivity_merged_df$Calories, method=c("spearman")) 
  
#vii)Test significance of Spearman coefficient
  cor.test(dailyActivity_merged_df$VeryActiveMinutes, dailyActivity_merged_df$Calories, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------#

#n)Fairly Active Minutes vs Calories -------------------------------------

#i)Compare FairlyActiveMinutes to Calories
  
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=FairlyActiveMinutes, y=Calories)) + 
    geom_point(color="navy") +
    geom_smooth(method=lm, se=FALSE)

#ii)Mean number of minutes respondents spent Fairly Active
  
  mean(dailyActivity_merged_df$FairlyActiveMinutes)
  
#iii)Median number of minutes respondents spent Fairly Active
  
  median(dailyActivity_merged_df$FairlyActiveMinutes)
  
#iv)Range of Calories and Average Calories burned when respondents were Fairly Active for LESS than 50 minutes
  
  less_than_50_FA <- filter(dailyActivity_merged_df, dailyActivity_merged_df$FairlyActiveMinutes < 50)
  
  min(less_than_50_FA$Calories)

  max(less_than_50_FA$Calories)

  mean(less_than_50_FA$Calories)
  
#v)Range of Calories and Average Calories burned while being Fairly Active for MORE than 50 minutes
  
  more_than_50_FA <- filter(dailyActivity_merged_df, dailyActivity_merged_df$FairlyActiveMinutes > 50)
  
  min(more_than_50_FA$Calories)
    
  max(more_than_50_FA$Calories)
    
  mean(more_than_50_FA$Calories)
  
#vi)Percentage increase in Mean Calories burned when Very Active for MORE than 50 minutes, when compared with being Fairly Active for MORE than 50 minutes
  
  ((mean(more_than_50_VA$Calories)-mean(more_than_50_FA$Calories))/mean(more_than_50_VA$Calories))*100

#vii)Determine Correlation Coefficient. Use Spearman method as FairlyActiveMinutes is not normally distributed
  
  cor(dailyActivity_merged_df$FairlyActiveMinutes, dailyActivity_merged_df$Calories, method=c("spearman"))
  
#viii)Test significance of Spearman coefficient
  
  cor.test(dailyActivity_merged_df$FairlyActiveMinutes, dailyActivity_merged_df$Calories, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------#

#o)Lightly Active Minutes vs Calories ------------------------------------

#i)Compare LightlyActiveMinutes to Calories
    
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=LightlyActiveMinutes, y=Calories)) + 
    geom_point(color="navy") +
    geom_smooth(method=lm)
    
#ii)Determine Correlation Coefficient. Use Pearson method as variables appear to be normally distributed
  
  cor(dailyActivity_merged_df$LightlyActiveMinutes, dailyActivity_merged_df$Calories, method=c("pearson"))
  
#iii)Test significance of Pearson coefficient
  
  cor.test(dailyActivity_merged_df$LightlyActiveMinutes, dailyActivity_merged_df$Calories, alternative="two.sided", method="pearson", exact=FALSE, conf.level = 0.95)
  
#---------------------------------------------------------------#

#p)Very Active Distance vs Calories --------------------------------------

#i)Compare VeryActiveDistance to Calories

  ggplot(data=dailyActivity_merged_df, mapping=aes(x=VeryActiveDistance, y=Calories)) +
    geom_point(color="navy") +
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method as VeryActiveDistance is not normally distributed
    
  cor(dailyActivity_merged_df$VeryActiveDistance, dailyActivity_merged_df$Calories, method=c("spearman"))

#iii)Test significance of Spearman coefficient
    
  cor.test(dailyActivity_merged_df$VeryActiveDistance, dailyActivity_merged_df$Calories, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#vi)Calculate Average Calories burned when VeryActiveDistance is LESS than 5km
  
    less_than_5_VAD <- filter(dailyActivity_merged_df, dailyActivity_merged_df$VeryActiveDistance < 5)
    
    mean(less_than_5_VAD$Calories)
    
#---------------------------------------------------------------#

#q)Moderately Active Distance vs Calories -------------------------------

#i)Compare ModeratelyActiveDistance to Calories
  
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=ModeratelyActiveDistance, y=Calories)) +
    geom_point(color="navy") +
    geom_smooth(method=lm)
    
#ii)Determine Correlation Coefficient. Use Spearman method as ModeratelyActiveDistance is not normally distributed
    
  cor(dailyActivity_merged_df$ModeratelyActiveDistance, dailyActivity_merged_df$Calories, method=c("spearman"))

#iii)Test significance of Spearman coefficient
  
  cor.test(dailyActivity_merged_df$ModeratelyActiveDistance, dailyActivity_merged_df$Calories, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------#

#r)Light Active Distance vs Calories -------------------------------------

#i)Compare LightActiveDistance to Calories

  ggplot(data=dailyActivity_merged_df, mapping=aes(x=LightActiveDistance, y=Calories)) +
    geom_point(color="navy") +
    geom_smooth(method=lm)
      
#ii)Determine Correlation coefficient. Use Spearman method as LightActiveDistance is not normally distributed
    
  cor(dailyActivity_merged_df$LightActiveDistance, dailyActivity_merged_df$Calories, method=c("spearman"))
    
#iii)Test significance of Spearman coefficient
  
  cor.test(dailyActivity_merged_df$LightActiveDistance, dailyActivity_merged_df$Calories, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------#

#s)Very Active Minutes vs Dates ------------------------------------------

#i)Compare the dates when people spent most time being Very Active

  a_df <- dailyActivity_merged_df %>% group_by(Id) %>% slice(which.max(VeryActiveMinutes))

#ii)Convert ActivityDate in a_df from Date format to Character in order to plot on bar chart,
#    to see which dates had the highest number of Very Active Minutes

  a_df$ActivityDate <- as.character(a_df$ActivityDate)

#iii)Plot a_df on a bar chart to see which date had the highest number of Very Active Minutes

  ggplot(data=a_df, mapping=aes(x=ActivityDate)) +
    geom_bar(fill="navy") +
    theme(axis.text.x = element_text(angle=90, size=10)) +
    ylab("Count")

#iv)Dates with highest number of Very Active Minutes for each respondent
  
  ggplot(data=a_df, mapping=aes(x=ActivityDate, y=VeryActiveMinutes)) +
    geom_col(fill="navy") +
    theme(axis.text.x = element_text(angle=90, size=10))

#v)Compare Activity Date to Very Active Minutes to see which dates people were Very Active
  
  ggplot(data=a_df, mapping=aes(x=ActivityDate, y=VeryActiveMinutes)) +
    geom_point(color="navy") +
    geom_smooth(method=lm)+
    theme(axis.text.x = element_text(angle=90, size=10))

#---------------------------------------------------------------#

#t)Very Active Distance vs Light Active Distance -------------------------

#i)Compare VeryActiveDistance to LightActiveDistance
  
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=LightActiveDistance, y=VeryActiveDistance)) +
    geom_point(color="navy") +
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method as neither variables are normally distributed
  
  cor(dailyActivity_merged_df$LightActiveDistance, dailyActivity_merged_df$VeryActiveDistance, method=c("spearman"))

#iii)Test significance of Spearman coefficient

  cor.test(dailyActivity_merged_df$LightActiveDistance, dailyActivity_merged_df$VeryActiveDistance, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------#

#u)Very Active Distance vs Moderately Active Distance --------------------

#i)Compare VeryActiveDistance to ModeratelyActiveDistance
  
  ggplot(data=dailyActivity_merged_df, mapping=aes(x=ModeratelyActiveDistance, y=VeryActiveDistance)) +
    geom_point(color="navy") +
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method as neither variables are normally distributed

  cor(dailyActivity_merged_df$ModeratelyActiveDistance, dailyActivity_merged_df$VeryActiveDistance, method=c("spearman"))

#iii)Test significance of Spearman coefficient

  cor.test(dailyActivity_merged_df$ModeratelyActiveDistance, dailyActivity_merged_df$VeryActiveDistance, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------#

#v)Moderately Active Distance vs Light Active Distance ------------------

#i)Compare ModeratelyActiveDistance to LightActiveDistance

  ggplot(data=dailyActivity_merged_df, mapping=aes(x=LightActiveDistance, y=ModeratelyActiveDistance)) +
    geom_point(color="navy") +
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method as neither variables are normally distributed
  
  cor(dailyActivity_merged_df$LightActiveDistance, dailyActivity_merged_df$ModeratelyActiveDistance, method=c("spearman"))

#iii)Test significance of Spearman coefficient
  
  cor.test(dailyActivity_merged_df$ModeratelyActiveDistance, dailyActivity_merged_df$LightActiveDistance, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

