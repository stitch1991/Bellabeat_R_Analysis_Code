
#meanMETs_dailyActivity_combined Data Frame Analysis Code

#This is a combined data frame of the mean_daily_METS_average and dailyActivity data frames,
#to compare average METs and daily activity information. 

#---------------------------------------------------------------#

#a)METs vs Total Steps ---------------------------------------------------

#i)Compare mean METs with TotalSteps

  ggplot(data=meanMETs_dailyActivity_combined, mapping=aes(x=TotalSteps, y=METs))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since neither variables are normally distributed

  cor(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$TotalSteps, method=c("spearman"))

#iii)Test significance of Spearman coefficient

  cor.test(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$TotalSteps, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------#

#b)METs vs Total Distance ------------------------------------------------

#i)Compare mean METs with TotalDistance

  ggplot(data=meanMETs_dailyActivity_combined, mapping=aes(x=TotalDistance, y=METs))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since neither variables are normally distributed

  cor(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$TotalDistance, method=c("spearman"))

#iii)Test significance of Spearman coefficient

  cor.test(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$TotalDistance, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------#

#c)METs vs Very Active Distance ------------------------------------------

#i)Compare mean METs with VeryActiveDistance

  gplot(data=meanMETs_dailyActivity_combined, mapping=aes(x=VeryActiveDistance, y=METs))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since neither variables are normally distributed

  cor(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$VeryActiveDistance, method=c("spearman"))

#iii)Test significance of Spearman coefficient

  cor.test(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$VeryActiveDistance, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)
  
#---------------------------------------------------------------#

#d)METs vs Moderately Active Distance ------------------------------------

#i)Compare mean METs with ModeratelyActiveDistance

  ggplot(data=meanMETs_dailyActivity_combined, mapping=aes(x=ModeratelyActiveDistance, y=METs))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since neither variables are normally distributed

  cor(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$ModeratelyActiveDistance, method=c("spearman"))

#iii)Test significance of Spearman coefficient

  cor.test(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$ModeratelyActiveDistance, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------#

#e)METs vs Light Active Distance -----------------------------------------

#i)Compare mean METs with LightActiveDistance

  ggplot(data=meanMETs_dailyActivity_combined, mapping=aes(x=LightActiveDistance, y=METs))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since neither variables are normally distributed

  cor(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$LightActiveDistance, method=c("spearman"))

#iii)Test significance of Spearman coefficient

  cor.test(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$LightActiveDistance, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------#

#f)METs vs Very Active Minutes -------------------------------------------

#i)Compare mean METs with VeryActiveMinutes

  ggplot(data=meanMETs_dailyActivity_combined, mapping=aes(x=VeryActiveMinutes, y=METs))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since neither variables are normally distributed

  cor(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$VeryActiveMinutes, method=c("spearman"))

#iii)Test significance of Spearman coefficient

  cor.test(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$VeryActiveMinutes, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------#

#g)METs vs Fairly Active Minutes -----------------------------------------

#i)Compare mean METs with FairlyActiveMinutes

  ggplot(data=meanMETs_dailyActivity_combined, mapping=aes(x=FairlyActiveMinutes, y=METs))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since neither variables are normally distributed

  cor(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$FairlyActiveMinutes, method=c("spearman"))

#iii)Test significance of Spearman coefficient

  cor.test(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$FairlyActiveMinutes, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------#

#h)METs vs Lightly Active Minutes ----------------------------------------

#i)Compare mean METs with LightlyActiveMinutes
  
  ggplot(data=meanMETs_dailyActivity_combined, mapping=aes(x=LightlyActiveMinutes, y=METs))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since neither variables are normally distributed

  cor(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$LightlyActiveMinutes, method=c("spearman"))

#iii)Test significance of Spearman coefficient

  cor.test(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$LightlyActiveMinutes, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#---------------------------------------------------------------#

#i)METs vs Sedentary Minutes ---------------------------------------------

#i)Compare mean METs with SedentaryMinutes
  
  ggplot(data=meanMETs_dailyActivity_combined, mapping=aes(x=SedentaryMinutes, y=METs))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since neither variables are normally distributed

  cor(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$SedentaryMinutes, method=c("spearman"))

#iii)Test significance of Spearman coefficient

  cor.test(meanMETs_dailyActivity_combined$METs, meanMETs_dailyActivity_combined$SedentaryMinutes, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)
