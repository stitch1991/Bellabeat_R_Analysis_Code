
#sleepDay_dailyActivity_combined Data Analysis Code

#This is a combined data frame of the sleepDay and dailyActivity data frames,
#to compare sleep and daily activity information.

#-------------------------------------------------------------------------#

#a)Check Number Of Respondents, And Data Types --------------------------

#i)Check number of unique Id numbers (respondents). 
  
  n_distinct(sleepDay_dailyActivity_combined$Id)
 
#ii)Check column data types
  
  sapply(sleepDay_dailyActivity_combined, class)
  
#-------------------------------------------------------------------------#

#b)Calories vs Total Minutes Asleep --------------------------------------

#i)Compare Calories with TotalMinutesAsleep
  
  ggplot(data=weightLogInfo_dailyActivity_combined, aes(x=TotalS, y=Calories))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Pearson method as variables appear to be normally distributed
  
  cor(sleepDay_dailyActivity_combined$TotalMinutesAsleep, sleepDay_dailyActivity_combined$Calories, method=c("pearson"))

#iii)Test significance of Pearson coefficient
  
  cor.test(sleepDay_dailyActivity_combined$TotalMinutesAsleep, sleepDay_dailyActivity_combined$Calories, alternative="two.sided", method="pearson", exact=FALSE, conf.level = 0.95)

#-------------------------------------------------------------------------#

#c)Total Minutes Asleep vs Total Steps -----------------------------------

#i)Compare TotalMinutesAsleep with TotalSteps
  
  ggplot(data=sleepDay_dailyActivity_combined, aes(x=TotalSteps, y=TotalMinutesAsleep))+
    geom_point(color="navy")+
    geom_smooth(method=lm)
  
#ii)Determine Correlation Coefficient. Use Spearman method as TotalSteps is not normally distributed
  
  cor(sleepDay_dailyActivity_combined$TotalSteps, sleepDay_dailyActivity_combined$TotalMinutesAsleep, method=c("spearman"))
  
#iii)Test significance of Spearman coefficient
  
  cor.test(sleepDay_dailyActivity_combined$TotalSteps, sleepDay_dailyActivity_combined$TotalMinutesAsleep, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#-------------------------------------------------------------------------#

#d)Total Minutes Asleep vs Very Active Minutes ---------------------------

#i)Compare TotalMinutesAsleep with VeryActiveMinutes
  
  ggplot(data=sleepDay_dailyActivity_combined, aes(x=VeryActiveMinutes, y=TotalMinutesAsleep))+
    geom_point(color="navy")+
    geom_smooth(method=lm)
  
#ii)Determine Correlation Coefficient. Use Spearman method since VeryActiveMinutes is not normally distributed
  
  cor(sleepDay_dailyActivity_combined$VeryActiveMinutes, sleepDay_dailyActivity_combined$TotalMinutesAsleep, method=c("spearman"))
  
#iii)Test significance of Spearman coefficient
  
  cor.test(sleepDay_dailyActivity_combined$VeryActiveMinutes, sleepDay_dailyActivity_combined$TotalMinutesAsleep, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#-------------------------------------------------------------------------#

#e)Total Minutes Asleep vs Sedentary Minutes -----------------------------

#i)Compare TotalMinutesAsleep with SedentaryMinutes
  
  ggplot(data=sleepDay_dailyActivity_combined, aes(x=SedentaryMinutes, y=TotalMinutesAsleep))+
    geom_point(color="navy")+
    geom_smooth(method=lm)
  
#ii)Determine Correlation Coefficient. Use Spearman method since SedentaryMinutes is not normally distributed
  
  cor(sleepDay_dailyActivity_combined$TotalMinutesAsleep, sleepDay_dailyActivity_combined$SedentaryMinutes, method=c("spearman"))
  
#iii)Test significance of Spearman coefficient
  
  cor.test(sleepDay_dailyActivity_combined$TotalMinutesAsleep, sleepDay_dailyActivity_combined$SedentaryMinutes, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#-------------------------------------------------------------------------#

#f)Lightly Active Minutes vs Total Minutes Asleep ------------------------

#i)Compare LightlyActiveMinutes with TotalMinutesAsleep
  
  ggplot(data=sleepDay_dailyActivity_combined, aes(x=LightlyActiveMinutes, y=TotalMinutesAsleep))+
    geom_point(color="navy")+
    geom_smooth(method=lm)
  
#ii)Determine Correlation Coefficient. Use Pearson method as both variables appear to be normally distributed
  
  cor(sleepDay_dailyActivity_combined$TotalMinutesAsleep, sleepDay_dailyActivity_combined$LightlyActiveMinutes, method=c("pearson"))
  
#iii)Test significance of Pearson coefficient
  
  cor.test(sleepDay_dailyActivity_combined$TotalMinutesAsleep, sleepDay_dailyActivity_combined$LightlyActiveMinutes, alternative="two.sided", method="pearson", exact=FALSE, conf.level = 0.95)
  
  