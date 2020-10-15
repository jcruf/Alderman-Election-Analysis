
#Step 1: Load Relevant Libraries
library(tidyverse)
library(ggplot2)
library(stargazer)
library(lmtest)

#1. Tidyverse to tidy
# 2. ggplot2 to create scatterplot and densityplot
# 3. stargazer for tables
# 4. lmtest for heteroskedasticity tests


#Step 2 load and tidy data
RD_DF<-read.csv('RD_DF.csv')
RD_DF<-RD_DF %>%
  mutate(
    X=NULL,
    First_Primary=ifelse(First_Primary==TRUE,1,0),
    First_General=ifelse(First_General==TRUE,1,0),
    Second_General=ifelse(Second_General==TRUE,1,0),
    Second_Primary=ifelse(Second_Primary==TRUE,1,0),
  )
RD_DF<-RD_DF %>%
  mutate(
    First_Election_Interaction=First_Election*Incumbency
  )


#Step 3: Create descriptive statistics Table

stats<-as.data.frame(cbind(First_Election,Second_Election,Second_Win,Incumbency,First_Primary,Second_Primary,First_General,Second_General,First_Special,Second_Special))

stargazer(stats,title="Table 1: Descriptive Statistics of Key Variables", type = 'html', out = "descstats.htm")

#Step 4: Create the Density Plot

ggplot(RD_DF, aes(x=First_Election)) + 
  geom_density()+
  geom_vline(xintercept=100, linetype="longdash")+
  ggtitle('Figure 1: Running Variable Density Plot')+
  xlab("First Election Percent Lead over Runnerup") +
  ylab("Density") +
  theme(plot.title = element_text(hjust = 0.5))

#Step 4: Simple Regression Discontinuity Plot
ggplot(RD_DF, aes(x=First_Election,y=Second_Win, colour=as.character(Incumbency)))+
  geom_point(shape=19)+
  geom_smooth(aes(x=RD_DF$First_Election,y=predict(Simple)))+
  geom_vline(xintercept=100, linetype="longdash")+
  ylim(0,1)+
  ggtitle('Figure 2: Graphic Illustration of The Simple Regression Discontinuity Model')+
  xlab("First Election Percent Lead over Runnerup") +
  ylab("Second Election Win") +
  scale_colour_discrete(name="Incumbency\nCondition",
                        breaks=c("0", "1"), labels=c("Not Incumbent", "Incumbent"))+
  theme(plot.title = element_text(hjust = 0.5))

#Step 5: Create models, test with breusch-pagan heteroskedasticity test
Simple<-lm(Second_Win~First_Election+Incumbency+Incumbency*First_Election,data = RD_DF)
bptest(Simple)
Simple<-coeftest(Simple, vcov = vcovHC(Simple, type = "HC0"))


FEYear<-lm(Second_Win~First_Election+Incumbency+First_Election_Interaction+factor(First_Year)+factor(Second_Year),data = RD_DF) 
bptest(FEYear)
FEYear<-coeftest(FEYear, vcov = vcovHC(FEYear, type = "HC0"))

FEWard<-lm(Second_Win~First_Election+Incumbency+First_Election_Interaction+factor(First_Year)+factor(Second_Year)+factor(Ward),data=RD_DF)
bptest(FEWard)
FEWard<-coeftest(FEWard,vcov=vcovHC(FEWard, type= "HC0"))

#Display models with stargazer
stargazer(Simple,FEYear,FEWard,title="Table 2: Summary of Models", type='html', out="models.htm")

