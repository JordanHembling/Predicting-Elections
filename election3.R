

election <- read.csv('cleanElectiond.csv')

eNames <- read.csv('Enames.csv')
names(election)
eNames
#create 2016 subset for testing
sub16 <- election[,!names(election) %in% c('mfgEmp12', 'DrugFR12','Obese12','Pov12',
                                           'medInc12','incomePercent12','pop12','intMig12','RorD08','mfgEmp12', 'DrugFR08','Obese08','Pov08',
                                           'medInc08','incomePercent08','pop08','intMig08','empQuoMfg12','empQuoTtl2012',
                                           'EmpTtl2012','WageMfg2012','WageTtl2012','County.State','Dem12','Rep08','Rep12','Total08','Total12','emp08','civLab08',
                                           'emp12','domMig08','unemp08','enempR08','percentDem12','unempR08','Dem08','unemp12','domMig12','civLab12',
                                           'netMig12','netMig08','unempR12','popCHG08','popCHG12','percentDem08')]

names(sub16) <- names(eNames)
year <- rep('2016',nrow(sub16))
sub16 <- cbind(sub16,year)

#variables as numeric
sub16[,10:73] <- sapply(sub16[,10:73], as.numeric)

sub16$DrugFR<- sub16[,"DrugFR"]/100
sub16$unempR<- sub16[,"unempR"]/100
sub16[,41:72]<- sub16[,41:72]/100


#2012 subset
sub12 <- election[,!names(election) %in% c('mfgEmp16', 'DrugFR16','Obese16','Pov16',
                                           'medInc16','incomePercent16','pop16','intMig16','RorD16','mfgEmp16', 'DrugFR08','Obese08','Pov08',
                                           'medInc08','incomePercent08','pop08','intMig08','empQuoMfg16','empQuoTtl2016',
                                           'EmpTtl2016','WageMfg2016','WageTtl2016','County.State','Dem16','Rep08','Rep16','Total08','Total16','emp08','civLab08',
                                           'emp16','domMig08','unemp08','enempR08','percentDem16','unempR08','Dem08','unemp16','domMig16','civLab16',
                                           'netMig16','netMig08','unempR16','popCHG08','popCHG16','percentDem08')]


names(sub12) <- names(eNames)
year <- rep('2012',nrow(sub12))
sub12 <- cbind(sub12,year)


#variables as numeric
sub12[,10:73] <- sapply(sub12[,10:73], as.numeric)

sub12$DrugFR<- sub12[,"DrugFR"]/100
sub12$unempR<- sub12[,"unempR"]/100
sub12[,41:72]<- sub12[,41:72]/100

summary(sub12)

#removing nulls because I'm lazy for now  (about 10 counties)
colSums(is.na(sub12))
colSums(is.na(sub16))

#sub16 <- subset(sub16,!is.na(longitude))
#sub16 <- subset(sub16,!is.na(mfgEmp))

sub12t <- subset(sub12,!is.na(longitude))
sub12t <- subset(sub12t,!is.na(mfgEmp))
sub12t <- subset(sub12t,!is.na(pctWhite))
sub12t <- subset(sub12t,!is.na(incomePercent))


colSums(is.na(sub12t))
colSums(is.na(sub16))

#Test/Validation with even RorD 
#install.packages('caTools')
library(caTools)
help(package = 'caTools')

Y = sub16[,'RorD'] # extract labels from the data
msk = sample.split(Y, SplitRatio=8/10)
table(Y,msk)
t=sum( msk)  # number of elements in one class
f=sum(!msk)  # number of elements in the other class

validation <- sub16[msk,]
test <- sub16[!msk,]

sum(validation$RorD)/nrow(validation)
sum(test$RorD)/nrow(test)

#outcome factor
sub16$outcome <- ifelse(sub16$RorD == 1,'Dem','Rep')
sub16$outcome <- as.factor(sub16$outcome)
sub12$outcome <-ifelse(sub12$RorD ==1,'Dem','Rep')
sub12$outcome <- as.factor(sub12$outcome)
validation$outcome <- ifelse(validation$RorD == 1,'Dem','Rep')
validation$outcome <-as.factor(validation$outcome)
test$outcome <- ifelse(test$RorD == 1,'Dem','Rep')
test$outcome <- as.factor(test$outcome)
sub12t$outcome <-ifelse(sub12t$RorD ==1,'Dem','Rep')
sub12t$outcome <- as.factor(sub12t$outcome)


#correlation, might want to remove some variables 
cor(subset(sub12t,select = -c(fips, county, outcome, RorD,state,year,region)))
names(sub12)
#add population density
sub12$popDens <- sub12$pop/sub12$landArea
sub16$popDens <- sub16$pop/sub16$landArea
sub12t$popDens <- sub12t$pop/sub12t$landArea
validation$popDens <- validation$pop/validation$landArea
test$popDens <- test$pop/test$landArea
  
names(sub12)
#normalize data
sub12$popN <- scale(sub12$pop)
sub12$mfgEmpN <- scale(sub12$mfgEmp)
sub12$medIncN <- scale(sub12$medInc)
sub12$intMigN <- scale(sub12$intMig)
sub12$domMigN <- scale(sub12$domMig)
sub12$civLabN <- scale(sub12$civLab)
sub12$EmpTtlN <- scale(sub12$EmpTtl)
sub12$WageMfgN <- scale(sub12$WageMfg)
sub12$WageTtlN <- scale(sub12$WageTtl)
sub12$landAreaN <- scale(sub12$landArea)
sub12$popDensN <- scale(sub12$popDens)
sub12$latitudeN <- scale(sub12$latitude)
sub12$longitudeN <- scale(sub12$longitude)

sub12t$popN <- scale(sub12t$pop)
sub12t$mfgEmpN <- scale(sub12t$mfgEmp)
sub12t$medIncN <- scale(sub12t$medInc)
sub12t$intMigN <- scale(sub12t$intMig)
sub12t$domMigN <- scale(sub12t$domMig)
sub12t$civLabN <- scale(sub12t$civLab)
sub12t$EmpTtlN <- scale(sub12t$EmpTtl)
sub12t$WageMfgN <- scale(sub12t$WageMfg)
sub12t$WageTtlN <- scale(sub12t$WageTtl)
sub12t$landAreaN <- scale(sub12t$landArea)
sub12t$popDensN <- scale(sub12t$popDens)
sub12t$latitudeN <- scale(sub12t$latitude)
sub12t$longitudeN <- scale(sub12t$longitude)

sub16$popN <- scale(sub16$pop)
sub16$mfgEmpN <- scale(sub16$mfgEmp)
sub16$medIncN <- scale(sub16$medInc)
sub16$intMigN <- scale(sub16$intMig)
sub16$domMigN <- scale(sub16$domMig)
sub16$civLabN <- scale(sub16$civLab)
sub16$EmpTtlN <- scale(sub16$EmpTtl)
sub16$WageMfgN <- scale(sub16$WageMfg)
sub16$WageTtlN <- scale(sub16$WageTtl)
sub16$landAreaN <- scale(sub16$landArea)
sub16$popDensN <- scale(sub16$popDens)
sub16$latitudeN <- scale(sub16$latitude)
sub16$longitudeN <- scale(sub16$longitude)

validation$popN <- scale(validation$pop)
validation$mfgEmpN <- scale(validation$mfgEmp)
validation$medIncN <- scale(validation$medInc)
validation$intMigN <- scale(validation$intMig)
validation$domMigN <- scale(validation$domMig)
validation$civLabN <- scale(validation$civLab)
validation$EmpTtlN <- scale(validation$EmpTtl)
validation$WageMfgN <- scale(validation$WageMfg)
validation$WageTtlN <- scale(validation$WageTtl)
validation$landAreaN <- scale(validation$landArea)
validation$popDensN <- scale(validation$popDens)
validation$latitudeN <- scale(validation$latitude)
validation$longitudeN <- scale(validation$longitude)

test$popN <- scale(test$pop)
test$mfgEmpN <- scale(test$mfgEmp)
test$medIncN <- scale(test$medInc)
test$intMigN <- scale(test$intMig)
test$domMigN <- scale(test$domMig)
test$civLabN <- scale(test$civLab)
test$EmpTtlN <- scale(test$EmpTtl)
test$WageMfgN <- scale(test$WageMfg)
test$WageTtlN <- scale(test$WageTtl)
test$landAreaN <- scale(test$landArea)
test$popDensN <- scale(test$popDens)
test$latitudeN <- scale(test$latitude)
test$longitudeN <- scale(test$longitude)
#class tree
library(tree)
tree.elect = tree(RorD~RorDpy + mfgEmp+medInc+intMig+domMig+civLab+EmpTtl+WageMfg+WageTtl+
                  landArea+popDens+latitude+longitude+YdiscussOppose+Ydiscuss+	YCO2limits+ YCO2limitsOppose+	YtrustclimsciSST+	YtrustclimsciSSTOppose+
                  Yregulate+	YregulateOppose+	YsupportRPS+	YsupportRPSOppose+	Yfundrenewables+	YfundrenewablesOppose+ Yhappening+
                  YhappeningOppose+	Yhuman+	YhumanOppose+	Yconsensus+	YconsensusOppose+	Yworried+	YworriedOppose+	Ypersonal+
                  YpersonalOppose+	YharmUS+	YharmUSOppose+	Ydevharm+	YdevharmOppose+	Yfuturegen+	YfuturegenOppose+
                  Yharmplants+	YharmplantsOppose+	Ytiming+	YtimingOppose,
                  data=sub12,control=tree.control(nobs=nrow(sub12),mindev=0.001))

help(package ='tree')
#original tree only did one split and stopped, by default it doesn't just build a large tree. 
#we can change this using control parameters. 
tree.elect
summary(tree.elect)
# plot and label the tree
plot(tree.elect)
text(tree.elect,pretty=0)
#PCA
#install.packages('stats')
library(stats)
names(sub12t)
sub12sub <- subset(sub12t,select = c(RorDpy,mfgEmp,medInc,intMig,domMig,civLab,EmpTtl,WageMfg,WageTtl,
                                       landArea,popDens,latitude,longitude,YdiscussOppose,Ydiscuss,	YCO2limits, YCO2limitsOppose,	YtrustclimsciSST,	YtrustclimsciSSTOppose,
                                     Yregulate,	YregulateOppose,	YsupportRPS,	YsupportRPSOppose,	Yfundrenewables,	YfundrenewablesOppose, Yhappening,
                                     YhappeningOppose,	Yhuman,	YhumanOppose,	Yconsensus,	YconsensusOppose,	Yworried,	YworriedOppose,	Ypersonal,
                                     YpersonalOppose,	YharmUS,	YharmUSOppose,	Ydevharm,	YdevharmOppose,	Yfuturegen,	YfuturegenOppose,
                                     Yharmplants,	YharmplantsOppose,	Ytiming,	YtimingOppose, RorD))

logsub <- log(sub12sub)
logsub$RorD <- sub12sub$RorD

pc.RorD <- sub12sub[,'RorD']
sub12.pca <- prcomp(sub12sub,
                 center = TRUE,
                 scale. = TRUE) 

summary(sub12.pca)
biplot(sub12.pca, scale = 0)

print(sub12.pca)

plot(sub12.pca, type = 'l')
trans$rotation

# library(devtools)
# install_github("ggbiplot", "vqv")
# 
# library(ggbiplot)
# g <- ggbiplot(sub12.pca, obs.scale = 1, var.scale = 1, 
#               groups = pc.RorD, ellipse = TRUE, 
#               circle = TRUE)
# g <- g + scale_color_discrete(name = '')
# g <- g + theme(legend.direction = 'horizontal', 
#                legend.position = 'top')
# print(g)

#install.packages('caret')
#install.packages('e1071')
require(caret)
require(e1071)
trans = preProcess(subset(logsub, select = -RorD), 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC = predict(trans,subset(logsub, select = -RorD))
summary(PC)
names(PC)

#random forest 1 --> accuracy: 87%
names(sub12t)
library(randomForest)
forest.vote = randomForest(outcome~  pop + mfgEmp + medInc + intMig + domMig + civLab + EmpTtl + WageMfg + WageTtl + 
                             landArea + popDens + latitude + longitude + Ydiscuss + YCO2limits + Yfundrenewables +Yhappening +
                           Yhuman +	YharmplantsOppose + Ytiming + Yconsensus +	Yworried+	Ypersonal +
                          YharmUS,data=sub12t)

pred.forest = predict(forest.vote)
table(sub12t$outcome,pred.forest)


#predict 2016 forest 
pred.forest16 <- predict(forest.vote,validation,type ='response')
#2016 outcomes vs. predict
table(validation$outcome,pred.forest16)
forest16 <- table(validation$outcome,pred.forest16)

#Accuracy
(forest16[1] + forest16[4])/(forest16[1]+forest16[2]+forest16[3] + forest16[4])


#random forest 2 --> probability
forest.vote2 = randomForest(outcome~  pop + mfgEmp + medInc + intMig + domMig + civLab + EmpTtl + WageMfg + WageTtl + 
                              landArea + popDens + latitude + longitude + Ydiscuss + YCO2limits + Yfundrenewables +Yhappening +
                              Yhuman +	YharmplantsOppose + Ytiming + Yconsensus +	Yworried+	Ypersonal +
                              YharmUS,data=sub12t)

pred.forest2 = predict(forest.vote2)
table(sub12t$outcome,pred.forest2)
#help(package = 'randomForest')

#predict 2016 forest
pred.forest16.2 <- predict(forest.vote2,validation,type ='prob')


#boosting --> accuracy: 89.3%
library(gbm)
set.seed(300)

boost.vote <- gbm(RorD~  mfgEmp + medInc + intMig + domMig + civLab + EmpTtl + WageMfg + WageTtl + 
                    landArea + popDens + latitude + longitude + Ydiscuss + YCO2limits + Yfundrenewables +Yhappening +
                    Yhuman +	YharmplantsOppose + Ytiming + Yconsensus +	Yworried+	Ypersonal +
                    YharmUS,data=sub12t,distribution = "bernoulli",n.trees=1000)

pred.vote <- predict(boost.vote,n.trees=1000,type="response")
pred.vote <- round(pred.vote,0)
table(sub12t$RorD,pred.vote)
summary(boost.vote)
#predict 2016 boosting
pred.vote16 <- predict(boost.vote,validation,n.trees=1000,type="response")
pred.vote16 <- round(pred.vote16,0)
vote16 <- table(validation$RorD,pred.vote16)

#accuracy
(vote16[1] + vote16[4]) / (vote16[1] + vote16[2] + vote16[3] + vote16[4])

#boosting 2: probability
library(gbm)
set.seed(300)

boost.vote2 <- gbm(RorD~ mfgEmp + medInc + intMig + domMig + civLab + EmpTtl + WageMfg + WageTtl + 
                     landArea + popDens + latitude + longitude + Ydiscuss + YCO2limits + Yfundrenewables +Yhappening +
                     Yhuman +	YharmplantsOppose + Ytiming + Yconsensus +	Yworried+	Ypersonal +
                     YharmUS,data=sub12t,distribution = "bernoulli",n.trees=1000)

pred.vote2 <- predict(boost.vote2,n.trees=1000,type="response")
table(sub12t$RorD,pred.vote2)
summary(boost.vote2)
#predict 2016 boosting
pred.vote16.2 <- predict(boost.vote2,sub16,n.trees=1000,type="response")
vote16.2 <- table(sub16$RorD,pred.vote16.2)
sub16v <-cbind(sub16,pred.vote16.2)

sub16v$Dvote <- round(sub16v$Totalvote*pred.vote16.2,0)
sub16v$Rvote <- sub16v$Totalvote - sub16v$Dvote

library("data.table")
sub16v <- as.data.table(sub16v)
setkey(sub16v,state)
head(sub16)
sub16v <- sub16v[,list(republicanP = sum(Rvote, na.rm = TRUE),republicanA = sum(rep, na.rm = TRUE),
                       democratP = sum(Dvote, na.rm = TRUE), democratA = sum(dem, na.rm = TRUE)), by = 'state']

sub16v$RorDp <- ifelse(sub16v$republicanP > sub16v$democratP, 0 ,1)  
sub16v$RorDa <- ifelse(sub16v$republicanA > sub16v$democratA, 0 ,1)  

#predicted democratic states
sum(sub16v$RorDp)
#actual democratic states
sum(sub16v$RorDa)

#majority class proportion 2016
1- sum(validation$RorD)/nrow(validation)
1- sum(sub12t$RorD)/nrow(sub12t)


#neural net 
library(neuralnet)
names(sub12)

mfgEmp + medInc + intMig + domMig + civLab + EmpTtl + WageMfg + WageTtl + 
  landArea + popDens + latitude + longitude + Ydiscuss + YCO2limits + Yfundrenewables +Yhappening +
  Yhuman +	YharmplantsOppose + Ytiming + Yconsensus +	Yworried+	Ypersonal +
  YharmUS

elections.net <- neuralnet(RorD ~ mfgEmpN + medIncN + popDensN + civLabN  + intMigN + YCO2limitsOppose + Yfundrenewables + Yhappening + 
                             Yhuman + YharmplantsOppose + Ytiming + Yconsensus + Yworried + Ypersonal,
                           data=sub12t,err.fct='ce',linear.output=FALSE,hidden=3, stepmax = 1e6)

dem.pred <- compute(elections.net, sub16t[,c('mfgEmpN','medIncN','popDensN','civLabN', 
                                             'intMigN','YCO2limitsOppose','Yfundrenewables','Yhappening', 
                                              'Yhuman','YharmplantsOppose','Ytiming','Yconsensus','Yworried','Ypersonal')])
sort(dem.pred$net.result,decreasing=TRUE) 
pred = data.frame(fips=1:nrow(sub16t),RorDa=sub16t$RorD,vote.prob = dem.pred$net.result)
pred = pred[order(pred$vote.prob,decreasing=TRUE),]

# create another column in the data frame pred to store the predicted cancellation outcome
pred$vote.nn = as.numeric(pred$vote.prob>0.5)

# confusion matrix of the prediction
votenn <- table(pred$RorD,pred$vote.nn)
accuracynn <- (votenn[1] + votenn[4]) / (votenn[1] + votenn[4] + votenn[3] + votenn[2])
accuracynn
