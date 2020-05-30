
#install.packages("sas7bdat")
#install.packages("rms")
library(rms)
library(sas7bdat)
library(summarytools)

########################################################
# Create a link to the directory where your files are ##
# 1. Create a data frame and name it "drink"          ##
########################################################

#set directory as follow
#mylib<-"E:\\Re__Tuesday_Class\\Lecture 2Descriptive"


setwd(mylib)
#read.sas7bdat("binge_drinking")->drink
#dir()   # shows the file names of what is in folder
drink=read.csv("Binge_Drinking.csv")
freq(drink$gender)
freq(drink$age)
freq(drink$class)
freq(drink$fratsoro)
freq(drink$livewith)
freq(drink$drinks5)
freq(drink$drinkprob)




#######################################################################################
#2. Review characteristics of the data set to determine type of                       #
#variables (continuous or categorical (character or numeric), number of variables,    #
#what the variables are, etc.)                                                        #
########################################################################################
head(drink)

###########################################
## 3.Create ID variable if necessary    ###                                                 
###########################################
ID=c(1:1400)
drink=cbind(ID,drink)
head(drink)
str(drink)

complete.cases(drink)

#  report position of NAs
which(is.na(drink$gender))
which(is.na(drink$age))
which(is.na(drink$class))
which(is.na(drink$fratsoro))
which(is.na(drink$livewith))
which(is.na(drink$drinks5))
which(is.na(drink$drinkprob))

freq(drink$gender)


##############################################################################
# 4. Format categorical variables with numeric responses, so that the numeric#
#responses are meaningful when printed in results                            #
##############################################################################

drink$gender = gsub(1, "Male",drink$gender)
drink$gender = gsub(2, "Female",drink$gender)

drink$class = gsub(1, "Freshman",drink$class)
drink$class = gsub(2, "Sophomore",drink$class)
drink$class = gsub(3, "Junior",drink$class)
drink$class = gsub(4, "Senior",drink$class)
drink$class = gsub(5, "Equal or greater than 5th year",drink$class)

drink$fratsoro = gsub(1, "Yes",drink$fratsoro)
drink$fratsoro = gsub(2, "NO",drink$fratsoro)

drink$drinks5 = gsub(1, "None",drink$drinks5)
drink$drinks5 = gsub(2, "Once",drink$drinks5)
drink$drinks5 = gsub(3, "Twice",drink$drinks5)
drink$drinks5 = gsub(4, "Three to five",drink$drinks5)
drink$drinks5 = gsub(5, "Six to nine",drink$drinks5)
drink$drinks5 = gsub(6, "More than ten",drink$drinks5)


appear.order=c("None","Once","Twice","Three to five","Six to nine","More than ten")
drink$drinks5 = factor(drink$drinks5, levels=unique(appear.order))
################################
# Check for changes in data ###
###############################
head(drink)

#############################################################
## Univariate Analysis for the Total Sample                ##
#############################################################


#1. For categorical variables, we will use "table" & "prop.table" to obtain summary statistics
gender.freq=table(drink$gender)
gender.prop=prop.table(gender.freq)

gender.table=as.data.frame(cbind(gender.freq,gender.prop*100))# Change to percent***
colnames(gender.table)=c("Frequency","Percent")
gender.table


class.freq=table(drink$class)
class.prop=prop.table(class.freq)
class.table=as.data.frame(cbind(class.freq,class.prop*100))
colnames(class.table)=c("Frequency","Percent")
class.table

##################################################################################################
#For Class (Year in School), we have 1393 respondents who answered this question and 7 respondents
#who did not (i.e., missing). The majority of students classified themselves as Juniors (25.1%)
#followed by Seniors (24.6%). The class with the fewest respondents is those students who are in
# their fifth year or more. This is an ordinal variable.
###################################################################################################

fratsoro.freq=table(drink$fratsoro)
fratsoro.prop=prop.table(fratsoro.freq)
fratsoro.table=as.data.frame(cbind(fratsoro.freq,fratsoro.prop*100))
colnames(fratsoro.table)=c("Frequency","Percent")

fratsoro.table

######################################################################################################
#Fraternity or Sorority Member has two categories and is nominal scale. Here we have 1392 respondents
#with 8 missing. The majority of respondents do not belong to a fraternity or sorority (85.8%).
######################################################################################################
drinks5.freq=table(drink$drinks5)
drinks5.prop=prop.table(drinks5.freq)
drinks5.table=as.data.frame(cbind(drinks5.freq,drinks5.prop*100))
colnames(drinks5.table)=c("Frequency","Percent")

drinks5.table
##               Frequency    Percent
## None                850 0.61728395
## Once                186 0.13507625
## Twice               123 0.08932462
## three to five       151 0.10965868
## six to nine          48 0.03485839
## more than ten        19 0.01379811

#
#The Drinks5 variable, or binge drinking, is ordinal scaled. The majority of the respondents indicate that
#they do not binge drink (61.7%) while 13.5% indicate they binged drank once. The fewest respondents
#reported binge drinking 10 times or more in the last two weeks (1.4%).
#
#########################################################################################################
# 2. For continuous variables, we will use "summary" to obtain summary statistics and use "hist" to get a hsitogram
###################################################################################################################
age.summary = summary(drink$age)
age.summary

#For Age, we summarize with the mean and standard deviation above; however, the frequency table allows us to see if there
#are any potential outliers. Since it runs continuously from 17 to 26, there are no outliers.
#Get a histogram of Age.
#The histogram for age has a spike at the right tail and therefore normality does not seem reasonable.

hitsgram=hist(drink$age, main="Histogram of age",xlab="Age in years")

#Plot the PP plot to check normality. Use the "qqnorm" function.

qqnorm(drink$age)
qqline(drink$age)

#

#NOTE: The Total sample size is 1400 respondents (e.g., in Gender we have 1394 that are valid responses and 6 respondents who are missing. When we add those together, we have a total sample size of 1400. Notice that the majority of missing data is on the binge drinking variable with 23 respondents. Age is the only continuous, ratio variable, so that is the only variable we would interpret from this table. Notice the mean is 21.16 and the median is 21-so we have nearly a symmetrical distribution. Here we would report the mean (21.16), standard deviation (2.40), min (17), and max (26). Sample size is 1394 with 6 respondents missing.

#Graphical Summary That Addresses the Research Question:
#Students who belong to a fraternity or sorority compared to students who do not belong to a Greek organization will have reported higher binge drinking.
#You may need to install R package "ggplot2", using "install.package("ggplot2")"
#1. If we wish to provide the frequencies in the y-axis
library(ggplot2)
drink=subset(drink,!is.na(drink$fratsoro))

drink=subset(drink,!is.na(drink$drinks5))
ggplot(drink, aes(x=drinks5,fill=fratsoro)) + 
      geom_bar(stat="count",position="dodge",colour="black")+ 
      geom_text(aes(label=..count..),stat="count", position=position_dodge(width = 0.8),vjust = -0.2)+
       guides(fill=guide_legend(title="Fraternity/Sorority Member"))+
       labs(x = "Binge drinking",title="Relationship between binge drinking and Fraternity/Sorority membership")

#2. If we wish to provide the percentages in the y-axis
library(ggplot2)

ggplot(drink, aes(x= drinks5,  fill=fratsoro)) + 
      geom_bar(aes(y = ..prop.., group=fratsoro), stat="count",position="dodge",colour="black")+
        scale_y_continuous(labels = scales::percent)+guides(fill=guide_legend(title="Fraternity/Sorority Member"))+
    labs(y="Percentage",x = "Binge drinking",title="Relationship between binge drinking and Fraternity/Sorority membership")

library(psych)
test <- chisq.test(table(drink$fratsoro, drink$gender))
test
phi(table(drink$fratsoro, drink$gender))

test <- chisq.test(table(drink$fratsoro, drink$class))
test
phi(table(drink$fratsoro, drink$class))

test <- chisq.test(table(drink$fratsoro, drink$drinks5))
test
phi(table(drink$fratsoro, drink$drinks5))


test <- chisq.test(table(drink$fratsoso, drink$livewith))
test
phi(table(drink$fratsoro, drink$livewith))




t.test (age ~ fratsoro , var.equal=FALSE, data = drink)

library(tidyverse)

object = drink %>% group_by(fratsoro) %>% summarise(mean = mean(age, na.rm=TRUE), sd = sd(age, na.rm=TRUE), 
                                                    median = median(age, na.rm=TRUE) , min=min(age, na.rm=TRUE),
                                                    max=max(age, na.rm=TRUE),n = n(), non_na_count = sum(!is.na(age)))




t.test (drinkprob ~ fratsoro , var.equal=FALSE, data = drink)

object = drink %>% group_by(fratsoro) %>% summarise(mean = mean(drinkprob, na.rm=TRUE), sd = sd(drinkprob, na.rm=TRUE), 
                                                    median = median(drinkprob, na.rm=TRUE) , min=min(drinkprob, na.rm=TRUE),
                                                    max=max(drinkprob, na.rm=TRUE),n = n(), non_na_count = sum(!is.na(drinkprob)))
