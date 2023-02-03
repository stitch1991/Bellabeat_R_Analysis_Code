
#weightLogInfo_dailyActivity Data Frame Analysis Code 

#This is a combined data frame of the weightLogInfo and dailyActivity data frames,
#to compare weight and daily activity information. 

#----------------------------------------------------------------#

#A potential outlier was identified in the weightLogInfo table in Row 3. 
#The analysis will be conducted on the data frame both with and without the outlier for comparative reasons. 
#A copy of the data frame without the potential outlier is created below. 

WLI_DA_without_outlier_df <- weightLogInfo_dailyActivity_combined[-c(3),]

#----------------------------------------------------------------#

#a)Check Number Of Respondents, And Data Types ---------------------------

#i)Check number of unique Id numbers/respondents.

  n_distinct(weightLogInfo_dailyActivity_combined$Id)
  #There are 8 respondents

#ii)Check column data types
  
  sapply(sleepDay_dailyActivity_combined, class)

#----------------------------------------------------------------#

#b)Plot Bar Chart Of WeightKg With Counts --------------------------------

  ggplot(data=WLI_DA_without_outlier_df, aes(x=WeightKg))+
    geom_bar(stat="bin", bindwidth = "40")

#----------------------------------------------------------------#

#c)WeightKg vs Total Steps - WITH OUTLIER ---------------------------------

#i)Compare WeightKg with TotalSteps (INCLUDING outlier in Row 3)
  
  ggplot(data=weightLogInfo_dailyActivity_combined, aes(x=TotalSteps, y=WeightKg))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since neither variables are normally distributed, and 
#   contains a potential outlier
  
  cor(weightLogInfo_dailyActivity_combined$TotalSteps, weightLogInfo_dailyActivity_combined$WeightKg, method=c("spearman"))
  
#iii)Test significance of Spearman Coefficient
  
  cor.test(weightLogInfo_dailyActivity_combined$TotalSteps, weightLogInfo_dailyActivity_combined$WeightKg, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#----------------------------------------------------------------#

#d)WeightKg vs Total Steps - WITHOUT OUTLIER -----------------------------

#i)Compare WeightKg with TotalSteps (EXCLUDING outlier in Row 3)
  
  ggplot(data=WLI_DA_without_outlier_df, aes(x=TotalSteps, y=WeightKg))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since TotalSteps is not normally distributed
  
  cor(WLI_DA_without_outlier_df$TotalSteps, WLI_DA_without_outlier_df$WeightKg, method=c("spearman"))

#iii)Test significance of Spearman Coefficient
  
  cor.test(WLI_DA_without_outlier_df$TotalSteps, WLI_DA_without_outlier_df$WeightKg, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#----------------------------------------------------------------#

#e)WeightKg vs Total Distance - WITH OUTLIER -----------------------------

#i)Compare WeightKg with TotalDistance (INCLUDING outlier in Row 3)

  ggplot(data=weightLogInfo_dailyActivity_combined, aes(x=TotalDistance, y=WeightKg))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since neither variables are normally distributed,
#   and contains a potential outlier
  
  cor(weightLogInfo_dailyActivity_combined$TotalDistance, weightLogInfo_dailyActivity_combined$WeightKg, method=c("spearman"))

#iii)Test significance of Spearman Coefficient
  
  cor.test(weightLogInfo_dailyActivity_combined$TotalDistance, weightLogInfo_dailyActivity_combined$WeightKg, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#----------------------------------------------------------------#

#f)WeightKg vs Total Distance - WITHOUT OUTLIER --------------------------

#i)Compare WeightKg with TotalDistance (EXCLUDING outlier in Row 3)
  
  ggplot(data=WLI_DA_without_outlier_df, aes(x=TotalDistance, y=WeightKg))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since TotalDistance is not normally distributed
  
  cor(WLI_DA_without_outlier_df$TotalDistance, WLI_DA_without_outlier_df$WeightKg, method=c("spearman"))

#iii)Test significance of Spearman Coefficient
  
  cor.test(WLI_DA_without_outlier_df$TotalDistance, WLI_DA_without_outlier_df$WeightKg, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#----------------------------------------------------------------#

#g)BMI vs Total Distance - WITH OUTLIER ----------------------------------

#i)Compare BMI with TotalDistance (INCLUDING outlier in Row 3)
  
  ggplot(data=weightLogInfo_dailyActivity_combined, aes(x=TotalDistance, y=BMI))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since neither variables are normally distributed, 
#   and contains a potential outlier
  
  cor(weightLogInfo_dailyActivity_combined$TotalDistance, weightLogInfo_dailyActivity_combined$BMI, method=c("spearman"))

#iii)Test significance of Spearman Coefficient
  
  cor.test(weightLogInfo_dailyActivity_combined$TotalDistance, weightLogInfo_dailyActivity_combined$BMI, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#----------------------------------------------------------------#

#h)BMI vs Total Distance - WITHOUT OUTLIER -------------------------------

#i)Compare BMI with TotalDistance (EXCLUDING outlier in Row 3)
  
  ggplot(data=WLI_DA_without_outlier_df, aes(x=TotalDistance, y=BMI))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since TotalDistance is not normally distributed
  
  cor(WLI_DA_without_outlier_df$TotalDistance, WLI_DA_without_outlier_df$BMI, method=c("spearman"))

#iii)Test significance of Spearman Coefficient
  
  cor.test(WLI_DA_without_outlier_df$TotalDistance, WLI_DA_without_outlier_df$BMI, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#----------------------------------------------------------------#

#i)WeightKg vs Very Active Minutes - WITH OUTLIER ------------------------

#i)Compare WeightKg with VeryActiveMinutes (INCLUDING outlier in Row 3)
  
  ggplot(data=weightLogInfo_dailyActivity_combined, aes(x=VeryActiveMinutes, y=WeightKg))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since neither variables are normally distributed, 
#   and contains a potential outlier

  cor(weightLogInfo_dailyActivity_combined$VeryActiveMinutes, weightLogInfo_dailyActivity_combined$WeightKg, method=c("spearman"))

#iii)Test significance of Spearman Coefficient
  
  cor.test(weightLogInfo_dailyActivity_combined$VeryActiveMinutes, weightLogInfo_dailyActivity_combined$WeightKg, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#----------------------------------------------------------------#

#j)WeightKg vs Very Active Minutes - WITHOUT OUTLIER ---------------------

#i)Compare WeightKg with VeryActiveMinutes (EXCLUDING outlier in Row 3)

  ggplot(data=WLI_DA_without_outlier_df, aes(x=VeryActiveMinutes, y=WeightKg))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since VeryActiveMinutes is not normally distributed
  
  cor(WLI_DA_without_outlier_df$VeryActiveMinutes, WLI_DA_without_outlier_df$WeightKg, method=c("spearman"))

#iii)Test significance of Spearman Coefficient
  
  cor.test(WLI_DA_without_outlier_df$VeryActiveMinutes, WLI_DA_without_outlier_df$WeightKg, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#----------------------------------------------------------------#

#k)WeightKg vs Sedentary Minutes - WITH OUTLIER --------------------------

#i)Compare WeightKg with SedentaryMinutes (INCLUDING outlier in Row 3)
  
  ggplot(data=weightLogInfo_dailyActivity_combined, aes(x=SedentaryMinutes, y=WeightKg))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since neither variables are normally distributed, 
#   and contains a potential outlier
  
  cor(weightLogInfo_dailyActivity_combined$SedentaryMinutes, weightLogInfo_dailyActivity_combined$WeightKg, method=c("spearman"))

#iii)Test significance of Spearman Coefficient
  
  cor.test(weightLogInfo_dailyActivity_combined$SedentaryMinutes, weightLogInfo_dailyActivity_combined$WeightKg, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#----------------------------------------------------------------#

#l)WeightKg vs Sedentary Minutes - WITHOUT OUTLIER -----------------------

#i)Compare WeightKg with SedentaryMinutes (EXCLUDING outlier in Row 3)
  
  ggplot(data=WLI_DA_without_outlier_df, aes(x=SedentaryMinutes, y=WeightKg))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since SedentaryMinutes is not normally distributed

  cor(WLI_DA_without_outlier_df$SedentaryMinutes, WLI_DA_without_outlier_df$WeightKg, method=c("spearman"))

#iii)Test significance of Spearman Coefficient
  
  cor.test(WLI_DA_without_outlier_df$SedentaryMinutes, WLI_DA_without_outlier_df$WeightKg, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#----------------------------------------------------------------#

#m)Calories vs WeightKg - WITH OUTLIER -----------------------------------

#i)Compare Calories with WeightKg (INCLUDING outlier in Row 3)
  
  ggplot(data=weightLogInfo_dailyActivity_combined, aes(x=Calories, y=WeightKg))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Spearman method since WeightKg is not normally distributed, 
#   and contains a potential outlier
  
  cor(weightLogInfo_dailyActivity_combined$WeightKg, weightLogInfo_dailyActivity_combined$Calories, method=c("spearman"))

#iii)Test significance of Spearman Coefficient
  
  cor.test(weightLogInfo_dailyActivity_combined$WeightKg, weightLogInfo_dailyActivity_combined$Calories, alternative="two.sided", method="spearman", exact=FALSE, conf.level = 0.95)

#----------------------------------------------------------------#

#n)Calories vs WeightKg - WITHOUT OUTLIER --------------------------------

#i)Compare Calories with WeightKg (EXCLUDING outlier in Row 3)
  
  ggplot(data=WLI_DA_without_outlier_df, aes(x=Calories, y=WeightKg))+
    geom_point(color="navy")+
    geom_smooth(method=lm)

#ii)Determine Correlation Coefficient. Use Pearson method as variables appear to be normally distributed
  
  cor(WLI_DA_without_outlier_df$Calories, WLI_DA_without_outlier_df$WeightKg, method=c("pearson"))

#iii)Test significance of Pearson Coefficient
  
  cor.test(WLI_DA_without_outlier_df$Calories, WLI_DA_without_outlier_df$WeightKg, alternative="two.sided", method="pearson", exact=FALSE, conf.level = 0.95)







