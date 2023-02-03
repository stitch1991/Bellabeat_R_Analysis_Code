
#meanMETs_sleepDay_combined Data Frame Analysis Code

#This is a combined data frame of the mean_daily_METS_average and sleepDay data frames,
#to compare average METs rate and sleep information. 

#---------------------------------------------------------------#

#a)Check Number Of Respondents ------------------------------------------

  n_distinct(meanMETs_sleepDay_combined$Id)

#---------------------------------------------------------------#

#b)Total Minutes Asleep vs METs --------------------------------------------

#i)Compare TotalMinutesAsleep with METs
  
  ggplot(data=meanMETs_sleepDay_combined, mapping=aes(x=TotalMinutesAsleep, y=METs))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since neither variables are normally distributed

  cor(meanMETs_sleepDay_combined$METs, meanMETs_sleepDay_combined$TotalMinutesAsleep, method=c("spearman"))

#iii)Test significance of Spearman coefficient

  cor.test(meanMETs_sleepDay_combined$METs, meanMETs_sleepDay_combined$TotalMinutesAsleep, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------#

#c)Total Time In Bed vs METs ------------------------------------------------

#i)Compare TotalTimeInBed with METs
  
  ggplot(data=meanMETs_sleepDay_combined, mapping=aes(x=TotalTimeInBed, y=METs))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since neither variables are normally distributed

  cor(meanMETs_sleepDay_combined$METs, meanMETs_sleepDay_combined$TotalTimeInBed, method=c("spearman"))

#iii)Test significance of Spearman coefficient

  cor.test(meanMETs_sleepDay_combined$METs, meanMETs_sleepDay_combined$TotalTimeInBed, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)
