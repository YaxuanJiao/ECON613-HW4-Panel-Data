#======================================================================================================================================================================
# HW 4 Linear Panel Data
# Yaxuan Jiao
# NetID: yj124
#======================================================================================================================================================================
#import data
KTData <- read.csv(file.choose(), header=TRUE,stringsAsFactors = FALSE)
attach(KTData)
#======================================================================================================================================================================
# Exercise 1 Data
#======================================================================================================================================================================
#Represent the panel dimension of wages for 5 randomly selected individuals
#Random pick individual 1,13,113,133,1333
WT <- KTData[,c(1,5,3)]
#for individual 1
individual1_timetrend <- as.vector(WT[1:4,2])
individual1_logwage <- as.vector(WT[1:4,3])
plot(individual1_timetrend,individual1_logwage)
#for individual 13
individual13_timetrend <- as.vector(WT[84:95,2])
individual13_logwage <- as.vector(WT[84:95,3])
plot(individual13_timetrend,individual13_logwage)
#for individual 113
individual113_timetrend <- as.vector(WT[953:963,2])
individual113_logwage <- as.vector(WT[953:963,3])
plot(individual113_timetrend,individual113_logwage)
#for individual 133
individual133_timetrend <- as.vector(WT[1155:1165,2])
individual133_logwage <- as.vector(WT[1155:1165,3])
plot(individual133_timetrend,individual133_logwage)
#for individual 1333
individual1333_timetrend <- as.vector(WT[11072:11076,2])
individual1333_logwage <- as.vector(WT[11072:11076,3])
plot(individual1333_timetrend,individual1333_logwage)
#======================================================================================================================================================================
# Exercise 2 Random Effects Model
#======================================================================================================================================================================
#construct data for random effect model
Rdata <- KTData[,c(3,2,4)]
install.packages("nlme")
library(nlme)
gls(LOGWAGE~EDUC+POTEXPER,data = Rdata)
#check the estimate result by package
install.packages("plm")
library(plm)
Randata <- KTData[,c(1,5,3,2,4)]
paneldata <- plm.data(Randata, indexes = c("PERSONID","TIMETRND"))
plm(LOGWAGE~EDUC+POTEXPER,data = paneldata,model = "random")
#======================================================================================================================================================================
# Exercise 3 Fixed Effects Model
#======================================================================================================================================================================
#calculate the individual mean wage
install.packages("dplyr")
library(dplyr)
sum_wi <- KTData %>% 
  group_by(PERSONID) %>% 
  summarize(sum_wi = sum(LOGWAGE)) 
num_of_job <- as.matrix(table(KTData$PERSONID))
avg_wage_i <- sum_wi[,2]/num_of_job[,1]
#calculate the individual mean education 
sum_ei <- KTData %>% 
  group_by(PERSONID) %>% 
  summarize(sum_ei = sum(EDUC)) 
avg_edu_i <- sum_ei[,2]/num_of_job[,1]
#calculate the individual mean potential experience  
sum_pi <- KTData %>% 
  group_by(PERSONID) %>% 
  summarize(sum_pi = sum(POTEXPER)) 
avg_pe_i <- sum_pi[,2]/num_of_job[,1]
#Q1:Between Estimator 
BEDATA <- as.data.frame(c(avg_wage_i,avg_edu_i,avg_pe_i))
colnames(BEDATA) <- c("avg_wi","avg_ei","avg_pi")
lm(BEDATA[,1]~BEDATA[,2]+BEDATA[,3],BEDATA)
#check the estimate result by plm package
paneldata <- plm.data(Randata, indexes = c("PERSONID","TIMETRND"))
plm(LOGWAGE~EDUC+POTEXPER,data = paneldata,model = "between")

#Q2:Within Estimator
p_id <- as.data.frame(c(1:2178),ncol = 1)
WEdata <- as.data.frame(cbind(p_id,BEDATA))
colnames(WEdata) <- c("PERSONID","i_avg_wage","i_avg_edu","i_avg_pe")
WEDATA <- Randata %>% left_join(WEdata, by = "PERSONID")
#calculate the difference between yit and yibar
wit_wibar <- matrix(WEDATA[,3]-WEDATA[,6])
eit_eibar <- matrix(WEDATA[,4]-WEDATA[,7])
pit_pibar <- matrix(WEDATA[,5]-WEDATA[,8])
within_data <- as.data.frame(cbind(wit_wibar,eit_eibar,pit_pibar))  
lm(wit_wibar~eit_eibar+pit_pibar-1,within_data)

#check the estimate result by plm package
plm(LOGWAGE~EDUC+POTEXPER,data = paneldata,model = "within")

#Q3:First-difference Estimator
Data1 <-  Randata[,c(1,3,4,5)]
Data2 <- Data1[1:17918,]
Data3 <- Data1[2:17919,]
Data4 <- Data3-Data2
Data5 <- Data4[!Data4[,1]==1,]
Data6 <- Data5[,c(2,3,4)]
lm(Data6$LOGWAGE~Data6$EDUC+Data6$POTEXPER,Data6)
#check the estimate result by plm package
plm(LOGWAGE~EDUC+POTEXPER,data = paneldata,model = "fd")
#======================================================================================================================================================================
# Exercise 4 Understanding Fixed Effects
#======================================================================================================================================================================
#random select 100 individual data from the Randata 
ran_individual <- as.matrix(sample(1:2178,100,replace = TRUE))
ran_individual
ran_individual_data <- Randata[Randata[,1] %in% ran_individual,]
#Q1:write and optimize the likelihood associated to the problem and estimate the individual fixed effect parameters
#write the likelihood function 
xit <- as.matrix(ran_individual_data[,4:5])
yit <- as.matrix(ran_individual_data[,3])
logit_like <- function(beta){  
  y <- sum(yit*log(pnorm(xit%*%beta))) + sum((1-yit)*log(1-pnorm(xit%*%beta)))
  return(-y)
}
#optimize the logit likelihood function
beta = c(0.05,0.01)
logit_optim <- optim(par = beta, logit_like)$par
print(logit_optim)

#Q2:calculate individual fixed effect
coe <- matrix(logit_optim)
avg_ei_pi <- cbind(avg_edu_i,avg_pe_i)
eipi <- as.matrix(avg_ei_pi,ncol = 2)
alphai <- as.matrix(avg_wage_i - eipi%*%coe)
Idata1 <- KTData[,c(1,6,7,8,9,10)]
Idata2 <- Idata1[!duplicated(Idata1[,1]),]
attach(Idata2)
lm(alphai~ABILITY +BRKNHOME+FATHERED+MOTHERED+SIBLINGS)

#Q3:Robust Standard Error 
install.packages("sandwich")
library(sandwich)
sandwich_se <- diag(vcovHC(m_for_se, type = "HC3"))^0.5
sandwich_se


