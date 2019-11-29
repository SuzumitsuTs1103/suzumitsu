## Basic Step Statistics Project Based Learning
## Bank Marketing

# 出力したCSVデータを読み込めます
bank_data <- read.csv("bank_marketing_train.csv")


summary(bank_data)
dim(bank_data)
colnames(bank_data)

# any() usage：
is_na <- sapply(bank_data, function(y) any(is.na(y)))
is_na

#hist 
hist(bank_data$age)

#Category chk
str(bank_data)

table(bank_data$y)

###############################
# feature Engineering
###############################

#age
bank_data$age_g  <- 0
bank_data$age_g [which(bank_data$age >= 10)] = 1
bank_data$age_g [which(bank_data$age >= 20)] = 2
bank_data$age_g [which(bank_data$age >= 30)] = 3
bank_data$age_g [which(bank_data$age >= 40)] = 4
bank_data$age_g [which(bank_data$age >= 50)] = 5
bank_data$age_g <- as.factor(bank_data$age_g)


bank_data$duration_flg  <- 0
bank_data$duration_flg [which(bank_data$duration  > 168)] = 1
bank_data$duration_flg <- as.factor(bank_data$duration_flg)

bank_data$pdays_flg  <- 0
bank_data$pdays_flg [which(bank_data$pdays  > 15)] = 1
bank_data$pdays_flg [which(bank_data$pdays  == 999)] = 2
bank_data$pdays_flg <- as.factor(bank_data$pdays_flg)

hist(bank_data$campaign)
bank_data$campaign_g  <- 0
bank_data$campaign_g [which(bank_data$campaign == 0)] = 0
bank_data$campaign_g [which(bank_data$campaign == 1)] = 1
bank_data$campaign_g [which(bank_data$campaign == 2)] = 2
bank_data$campaign_g [which(bank_data$campaign >= 3)] = 3
bank_data$campaign_g <- as.factor(bank_data$campaign_g)


#education  : illiterate
bank_data$educationilliterate  <- 0
bank_data$educationilliterate [which(bank_data$education  == 'illiterate')] = 1
bank_data$educationilliterate <- as.factor(bank_data$educationilliterat)

#job : no_job
bank_data$no_job <- 0
bank_data$no_job[which(bank_data$job == 'student' | bank_data$job == 'retired')] = 1
bank_data$no_job <- as.factor(bank_data$no_job)

#contact cellular
bank_data$contactcellular  <- 0
bank_data$contactcellular [which(bank_data$contact  == 'cellular')] = 1
bank_data$contactcellular <- as.factor(bank_data$contactcellular)

#poutcome 以前のマーケティングキャンペーンの結果
bank_data$poutcomesuccess  <- 0
bank_data$poutcomesuccess [which(bank_data$poutcome  == 'success')] = 1
bank_data$poutcomesuccess <- as.factor(bank_data$poutcomesuccess)

#euribor3m
mean(bank_data$euribor3m)
bank_data$euri_num<-0
bank_data$euri_num [which(bank_data$euribor3m  <  mean(bank_data$euribor3m))] = 0
bank_data$euri_num [which(bank_data$euribor3m  >=  mean(bank_data$euribor3m))] = 1
bank_data$euri_num <- as.factor(bank_data$euri_num)


#emp.var.rate
mean(bank_data$emp.var.rate)
bank_data$emp_num<-0
bank_data$emp_num [which(bank_data$emp.var.rate  <  mean(bank_data$emp.var.rate))] = 0
bank_data$emp_num [which(bank_data$emp.var.rate  >=  mean(bank_data$emp.var.rate))] = 1
bank_data$emp_num <- as.factor(bank_data$emp_num)

#nr.employed
mean(bank_data$nr.employed)
bank_data$nr.emp_num<-0
bank_data$nr.emp_num [which(bank_data$nr.employed  <  mean(bank_data$nr.employed))] = 0
bank_data$nr.emp_num [which(bank_data$nr.employed  >=  mean(bank_data$nr.employed))] = 1
bank_data$nr.emp_num <- as.factor(bank_data$nr.emp_num)


#cons.conf.idx 
#mean(bank_data$cons.conf.idx)
#bank_data$cons.conf_num<-0
#bank_data$cons.conf_num [which(bank_data$cons.conf.idx  <  mean(bank_data$cons.conf.idx))] = 0
#bank_data$cons.conf_num [which(bank_data$cons.conf.idx  >=  mean(bank_data$cons.conf.idx))] = 1
#bank_data$cons.conf_num <- as.factor(bank_data$cons.conf_num)

#cons.price.idx 
#mean(bank_data$cons.price.idx)
#bank_data$cons.price_num<-0
#bank_data$cons.price_num [which(bank_data$cons.price.idx  <  mean(bank_data$cons.price.idx))] = 0
#bank_data$cons.price_num [which(bank_data$cons.price.idx  >=  mean(bank_data$cons.price.idx))] = 1
#bank_data$cons.price_num <- as.factor(bank_data$cons.price_num)

#
#bank_data$cons.price.idx_div_euribor3m <-0
#bank_data$cons.price.idx_div_euribor3m<-(bank_data$cons.price.idx /(bank_data$euribor3m) + 0.01)

#
#bank_data$euribor3m_div_nr.employed <-0
#bank_data$euribor3m_div_nr.employed<-(bank_data$euribor3m /(bank_data$nr.employed) + 0.01)

#default
bank_data$default <- as.factor(bank_data$default)


################################################################################

#Category chk
str(bank_data)

#jobで相関を確認
y_job <- table(bank_data$y, bank_data$job) 
print(y_job)
round(prop.table(y_job,2),digit=2)

#maritalで相関を確認
y_marital <- table(bank_data$y, bank_data$marital) 
print(y_marital)
round(prop.table(y_marital,2),digit=2)


#educationで相関を確認
y_education <- table(bank_data$y, bank_data$education) 
print(y_education)
round(prop.table(y_education,2),digit=2)

#duration_flgで相関を確認
y_duration <- table(bank_data$y, bank_data$duration_flg) 
print(y_duration)
round(prop.table(y_duration,2),digit=2)

#campaign_g で相関を確認
y_campaign_g <- table(bank_data$y, bank_data$campaign_g) 
print(y_campaign_g)
round(prop.table(y_campaign_g,2),digit=2)


#previous で相関を確認
y_previous <- table(bank_data$y, bank_data$previous) 
print(y_previous)
round(prop.table(y_previous,2),digit=2)
hist(bank_data$previous)

#contact で相関を確認
y_contact <- table(bank_data$y, bank_data$contact) 
print(y_contact)
round(prop.table(y_contact,2),digit=2)

#pdays で相関を確認   pdays =999 -> 2
y_pdays <- table(bank_data$y, bank_data$pdays_flg) 
print(y_pdays)
round(prop.table(y_pdays,2),digit=2)

#poutcome で相関を確認
y_poutcome <- table(bank_data$y, bank_data$poutcome) 
print(y_poutcome)
round(prop.table(y_poutcome,2),digit=2)

#euribor3m で相関を確認
y_euribor3m <- table(bank_data$y, bank_data$euri_num) 
print(y_euribor3m)
round(prop.table(y_euribor3m,2),digit=2)

hist(bank_data$euribor3m)

#nr.employed で相関を確認
y_emp_num <- table(bank_data$y, bank_data$emp_num) 
print(y_emp_num)
round(prop.table(y_emp_num,2),digit=2)
hist(bank_data$emp.var.rate)




#bank_data <- subset(bank_data, select = -c(contact,day_of_week,duration,campaign,pdays,previous,poutcome))
#bank_data <- subset(bank_data, select = -c(job,education,house,loan))
#bank_data <- subset(bank_data, select = -c(duration_flg,pdays_flg,campaign_g,poutcomesuccess))
#bank_data <- subset(bank_data, select = -c(education,contact,day_of_week,duration,poutcome,campaign_g,pdays))

bank_data <- subset(bank_data, select = -c(default))



str(bank_data)
summary(bank_data)
colnames(bank_data)

############################################################################################
# ペルソナ　　仮説
#　職業:ライフステージの変更による口座開設につながるのでは？
#　リタイア：退職後に資産運用や年金の利用するのでは？
#　学生??：学生が就職し、給与用の口座を開設するのでは？
#   ローンの有無や婚姻は関係なさそう
# 学歴：２分以上通話 &cellular　は過去の結果からは効果あり。
# 年齢層は１０代、２０代、５０才以上　をターゲット
#　以前のキャンペーンから、１５日以内に架電するべし&接触回数は５回以内


#############################################################################################
#  Sampling   smote
#############################################################################################
#PRE-PROCESSING:
library(DMwR)
library(caret)

#Splitting the training data into train and validation set
#Because it will maintain same class distribution in the resulting datasets as same as in the original data while splitting
set.seed(123)

table(bank_data$y)
dim(bank_data)

train <- subset(bank_data)
dim(train)

i <- createDataPartition(train$y, p = 3/4,list = FALSE)
new_train_pre <- train[i,]
table(new_train_pre$y)
dim(new_train_pre)

new_test <- train[-i,]
table(new_test$y)
dim(new_test)

#Dealing with Imbalanced Data
#Synthetic Data generation method is used to balance the classes
#Specifically, SMOTE Technique is used
new_train <-SMOTE(y~.,new_train_pre,perc.over = 400, perc.under = 150,k=5)
train_smote <- SMOTE(y~.,train, perc.over = 400, perc.under = 150, k=5)



#checking the proportion of classes before and after SMOTE
prop.table(table(new_train_pre$y)) # No-89%, Yes-11%
table(new_train_pre$y)

prop.table(table(new_train$y)) #No-55%, Yes-45%
table(new_train$y)

prop.table(table(train$y)) # No-89%, Yes-11%
table(train$y)

prop.table(table(train_smote$y)) #No-55%, Yes-45%
table(train_smote$y)

############################################################################################
#経済指標　->平均値　ｓｅｔ
############################################################################################

#new_test$emp.var.rate <-  mean(new_test$emp.var.rate)
#new_test$euribor3m <- mean(new_test$euribor3m)
#new_test$cons.price.idx <- mean(new_test$cons.price.idx)
#new_test$cons.conf.idx <- mean(new_test$cons.conf.idx)
#new_test$nr.employed <- mean(new_test$nr.employed)
#summary(new_test)

################################################################################################
# full model   Glm
################################################################################################
library(pscl)
library(car)


#多重共線性が強いため削除　-emp.var.rate -euribor3m  
#回帰係数＝0の帰無仮説を棄却できない項目を外す　-loan -default -marital -housing 

my_model1 <-glm(y~.
                -contact-day_of_week-duration-campaign-pdays-previous-poutcome
                -age-job -education -marital -housing -loan
                -educationilliterate
                -emp.var.rate -nr.employed-euribor3m-cons.price.idx-cons.conf.idx 
                -euri_num -nr.emp_num
                -campaign_g-duration_flg-pdays_flg
                ,data=new_train,family = 'binomial')


##回帰係数
summary(my_model1)
## オッズ比の計算
exp(my_model1$coefficients)
##AIC
AIC(my_model1)  #
##VIF
vif(my_model1)
#McFaddenの疑似決定係数
pR2(my_model1) #

alias(my_model1)

################################################################################################
#  Model Next
################################################################################################
my_model2 <-step(my_model1)
summary(my_model2)
AIC(my_model2)
#13737

vif(my_model2)


################################################################################################
#  Test Data
################################################################################################

new_test <- read.csv("bank_marketing_test2.csv")
str(new_test)

#age
new_test$age_g  <- 0
new_test$age_g [which(new_test$age >= 10)] = 1
new_test$age_g [which(new_test$age >= 20)] = 2
new_test$age_g [which(new_test$age >= 30)] = 3
new_test$age_g [which(new_test$age >= 40)] = 4
new_test$age_g [which(new_test$age >= 50)] = 5
new_test$age_g <- as.factor(new_test$age_g)


new_test$duration_flg  <- 0
new_test$duration_flg [which(new_test$duration  > 168)] = 1
new_test$duration_flg <- as.factor(new_test$duration_flg)

new_test$pdays_flg  <- 0
new_test$pdays_flg [which(new_test$pdays  > 15)] = 1
new_test$pdays_flg [which(new_test$pdays  == 999)] = 2
new_test$pdays_flg <- as.factor(new_test$pdays_flg)

hist(new_test$campaign)
new_test$campaign_g  <- 0
new_test$campaign_g [which(new_test$campaign == 0)] = 0
new_test$campaign_g [which(new_test$campaign == 1)] = 1
new_test$campaign_g [which(new_test$campaign == 2)] = 2
new_test$campaign_g [which(new_test$campaign >= 3)] = 3
new_test$campaign_g <- as.factor(new_test$campaign_g)


#education  : illiterate
new_test$educationilliterate  <- 0
new_test$educationilliterate [which(new_test$education  == 'illiterate')] = 1
new_test$educationilliterate <- as.factor(new_test$educationilliterat)

#job : no_job
new_test$no_job <- 0
new_test$no_job[which(new_test$job == 'student' | new_test$job == 'retired')] = 1
new_test$no_job <- as.factor(new_test$no_job)

#contact cellular
new_test$contactcellular  <- 0
new_test$contactcellular [which(new_test$contact  == 'cellular')] = 1
new_test$contactcellular <- as.factor(new_test$contactcellular)

#poutcome 以前のマーケティングキャンペーンの結果
new_test$poutcomesuccess  <- 0
new_test$poutcomesuccess [which(new_test$poutcome  == 'success')] = 1
new_test$poutcomesuccess <- as.factor(new_test$poutcomesuccess)

#euribor3m
mean(new_test$euribor3m)
new_test$euri_num<-0
new_test$euri_num [which(new_test$euribor3m  <  mean(new_test$euribor3m))] = 0
new_test$euri_num [which(new_test$euribor3m  >=  mean(new_test$euribor3m))] = 1
new_test$euri_num <- as.factor(new_test$euri_num)


#emp.var.rate
mean(new_test$emp.var.rate)
new_test$emp_num<-0
new_test$emp_num [which(new_test$emp.var.rate  <  mean(new_test$emp.var.rate))] = 0
new_test$emp_num [which(new_test$emp.var.rate  >=  mean(new_test$emp.var.rate))] = 1
new_test$emp_num <- as.factor(new_test$emp_num)

#nr.employed
mean(new_test$nr.employed)
new_test$nr.emp_num<-0
new_test$nr.emp_num [which(new_test$nr.employed  <  mean(new_test$nr.employed))] = 0
new_test$nr.emp_num [which(new_test$nr.employed  >=  mean(new_test$nr.employed))] = 1
new_test$nr.emp_num <- as.factor(new_test$nr.emp_num)


str(new_test)



################################################################################################
#テストデータを使って確認する
ypred <- predict(my_model2, newdata = new_test, type = 'response')
hist(ypred)
mean(ypred)


############################################################################################
# ROI/ AUC /ROC_Curve   /Cut_off  
############################################################################################
library(ROCR)
pred <- prediction(ypred,new_test$y)
perf <- ROCR::performance(pred, "tpr", "fpr")
plot(perf)
auc.tmp <- ROCR::performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc   

library(precrec)
library(ggplot2)

sscurves <- evalmod(scores = ypred, labels = new_test$y)
autoplot(sscurves)
auc(sscurves)



######
cutoff_values<- seq(0.1,1,length=200)
x<-c()
y<-c()

for(i in cutoff_values){
  y_flag <- ifelse(ypred>i,1,0)
  # 混同行列の作成
  conf_table <- table(y_flag,new_test$y)
  # 成約と予想して実際に成約した人数
  subscribed <-conf_table[4]
  # 成約すると予想して電話をかけた人数
  tel_num <- conf_table[2]+conf_table[4]
  x <- append(x,i)
  y <- append(y,subscribed*5000-tel_num*500)
}
plot(x,y,ylab="ROI",xlab="cutoff_values")
res <- data.frame(x,y)
# ROIが最大になっているカットオフ値を出力
max(res$y,na.rm = T)  #1919500  1969000   2529500
res[res$y == max(res$y,na.rm = T),]


# カットオフ値
y_flag <- ifelse(ypred>0.3216080    ,1,0)

#混同行列
conf_table <- table(y_flag,new_test$y)
conf_table

#電話かける人tta
tel_count <- conf_table[2] + conf_table[4]
tel_count
#成約した人
subscribed <- conf_table[4]
subscribed
(subscribed*5000)-(tel_count*500)

paste("Accuracy:",round((conf_table[1]+conf_table[4])/sum(conf_table)*100,2),"%",sep = "")
Accuracy_v <-(conf_table[1]+conf_table[4])/sum(conf_table)
paste("precision:",round((conf_table[4])/(conf_table[2]+conf_table[4])*100,2),"%",sep = "")

precision_v <-(conf_table[4])/(conf_table[2]+conf_table[4])
paste("recall:",round((conf_table[4])/(conf_table[3]+conf_table[4])*100,2),"%",sep = "")
recall_v <-(conf_table[4])/(conf_table[3]+conf_table[4])
paste("F-measure:",round((2*recall_v*precision_v)/(recall_v+precision_v)*100,2),"%",sep = "")


############################################################################################
#csv output
Tel_list <- data.frame(new_test,Attack=y_flag,AckRate=ypred)
dim(Tel_list)
write.csv(Tel_list, 'Tel_list.csv', row.names=FALSE)





############################################################################################
# train : test   Data Separate
############################################################################################
train_idx<-sample(c(1:dim(bank_data)[1]), size = dim(bank_data)[1]*0.7)
new_train<-bank_data[train_idx, ]
dim(new_train)
table(new_train$y.num)

new_test<- new_train[-train_idx, ]
dim(new_test)
table(new_test$y)