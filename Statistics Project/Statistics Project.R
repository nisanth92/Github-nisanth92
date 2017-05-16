library(readxl)
setwd("C:/Carlson/Statistics/Project")
lowbwt<-read_excel("lowbwt.xls")

colnames(lowbwt)=c('ID','weight_flag','age','mother_weight','race','smoke_flag','pre_birth','hyper_flag','uinf_flag','visits','baby')
lowbwt$babywt=lowbwt$baby*0.00220462

#Age
hist(lowbwt$age)
lowbwt<-lowbwt[lowbwt$age!=45,]
summary(lowbwt$age)
plot(lowbwt$age,lowbwt$babywt,pch=16)

summary(lowbwt$mother_weight)
hist(lowbwt$mother_weight)
#Postive skewed, mean>median
plot(lowbwt$mother_weight,lowbwt$babywt,pch=16)


#Race
summary(lowbwt$race)
table(lowbwt$race)
barplot(table(lowbwt$race))
boxplot(lowbwt$babywt~lowbwt$race)
#Mean of white baby is more than black and other
lowbwt$race_factor=as.factor(lowbwt$race)
fit<-aov(babywt~race_factor,data=lowbwt)
print(model.tables(fit, "means"))
TukeyHSD(fit, conf.level = .90)
#There is 
shapiro.test(fit$residuals)
qqnorm(fit$residuals)
qqline(fit$residuals,col="red")

#Smoke
summary(lowbwt$smoke_flag)
table(lowbwt$smoke_flag)
barplot(table(lowbwt$smoke_flag))
boxplot(lowbwt$babywt~lowbwt$smoke_flag)
#Mean of non smoke_flagrs baby weight is more than smoke_flagrs
fit<-aov(babywt~factor(smoke_flag),data=lowbwt)
print(model.tables(fit, "means"))
TukeyHSD(fit, conf.level = .90)
#There is relationship b/n smoke_flag and baby weight


#Pre birth
summary(lowbwt$pre_birth)
hist(lowbwt$pre_birth)
#Postive skewed, mean>median
t.test(lowbwt$pre_birth)
plot(lowbwt$pre_birth,lowbwt$babywt,pch=16)
t.test(lowbwt$babywt,lowbwt$pre_birth)

#Hypertension
summary(lowbwt$hyper_flag)
table(lowbwt$hyper_flag)
barplot(table(lowbwt$hyper_flag))
boxplot(lowbwt$babywt~lowbwt$hyper_flag)
#Mean of non smoke_flagrs baby weight is more than smoke_flagrs
fit<-aov(babywt~factor(hyper_flag),data=lowbwt)
print(model.tables(fit, "means"))
TukeyHSD(fit, conf.level = .90)
#There is relationship b/n smoke_flag and baby weight

#Uterine Irritability
summary(lowbwt$uinf_flag)
table(lowbwt$uinf_flag)
barplot(table(lowbwt$hyper_flag))
boxplot(lowbwt$babywt~lowbwt$uinf_flag)
#Mean of non smoke_flagrs baby weight is more than smoke_flagrs
fit<-aov(babywt~factor(uinf_flag),data=lowbwt)
print(model.tables(fit, "means"))
TukeyHSD(fit, conf.level = .90)
#There is relationship b/n smoke_flag and baby weight

#Doctor visits
summary(lowbwt$visits)
hist(lowbwt$visits)
#Postive skewed, mean>median
t.test(lowbwt$visits)
plot(lowbwt$visits,lowbwt$babywt,pch=16)
t.test(lowbwt$babywt,lowbwt$visits)


hist(lowbwt$babywt)
pairs(~babywt+mother_weight+age+hyper_flag+uinf_flag+pre_birth+visits,data=lowbwt)


#Regression
r=lm(babywt~mother_weight+age+smoke_flag+hyper_flag+uinf_flag+pre_birth+visits+race_factor,data=lowbwt)
summary(r)
#Collinearity
cor(lowbwt[c(3,4,7,10)])

r=lm(babywt~mother_weight+smoke_flag+hyper_flag+uinf_flag+race_factor,data=lowbwt)
summary(r)

r=lm(babywt~mother_weight+smoke_flag*race_factor+uinf_flag+hyper_flag,data=lowbwt)
summary(r)

r=lm(babywt~mother_weight+race_factor+uinf_flag*smoke_flag+hyper_flag,data=lowbwt)
summary(r)


r=lm(babywt~mother_weight+smoke_flag+hyper_flag+uinf_flag+race_factor,data=lowbwt)
summary(r)


fit<-aov(babywt~factor(race)+factor(smoke_flag)+factor(uinf_flag)+factor(hyper_flag),data=lowbwt)
summary(fit)
(anova(fit)[["Sum Sq"]][1]+anova(fit)[["Sum Sq"]][2]+anova(fit)[["Sum Sq"]][3]+anova(fit)[["Sum Sq"]][4])/(anova(fit)[["Sum Sq"]][1]+anova(fit)[["Sum Sq"]][2]+anova(fit)[["Sum Sq"]][3]+anova(fit)[["Sum Sq"]][4]+anova(fit)[["Sum Sq"]][5])
sqrt(anova(fit)[["Sum Sq"]][2]/fit$df.residual)

#model ends

#Assumptions check

r=lm(babywt~smoke_flag+hyper_flag+uinf_flag+race_factor,data=lowbwt)
summary(r)

#Race and smoke
table <- table(lowbwt[,5:6])
chisq.test(table)

table <- table(lowbwt[c(5,8)])
chisq.test(table)
lowbwt$comb_race=ifelse(lowbwt$race==1,1,2)
table <- table(lowbwt[c(8,14)])
chisq.test(table)
#No relation between race and hypertension

table <- table(lowbwt[c(14,9)])
chisq.test(table)

table <- table(lowbwt[c(8,9)])
chisq.test(table)

table <- table(lowbwt[c(8,6)])
chisq.test(table)

table <- table(lowbwt[c(9,6)])
chisq.test(table)


fit<-aov(mother_weight~factor(smoke_flag),data=lowbwt)
print(model.tables(fit, "means"))
TukeyHSD(fit, conf.level = .90)
anova(fit)[["Sum Sq"]][1]/(anova(fit)[["Sum Sq"]][1]+anova(fit)[["Sum Sq"]][2])
fit<-aov(mother_weight~factor(hyper_flag),data=lowbwt)
print(model.tables(fit, "means"))
TukeyHSD(fit, conf.level = .90)
anova(fit)[["Sum Sq"]][1]/(anova(fit)[["Sum Sq"]][1]+anova(fit)[["Sum Sq"]][2])
fit<-aov(mother_weight~factor(uinf_flag),data=lowbwt)
print(model.tables(fit, "means"))
TukeyHSD(fit, conf.level = .90)
anova(fit)[["Sum Sq"]][1]/(anova(fit)[["Sum Sq"]][1]+anova(fit)[["Sum Sq"]][2])
fit<-aov(mother_weight~factor(race),data=lowbwt)
print(model.tables(fit, "means"))
TukeyHSD(fit, conf.level = .90)
anova(fit)[["Sum Sq"]][1]/(anova(fit)[["Sum Sq"]][1]+anova(fit)[["Sum Sq"]][2])


fit<-aov(babywt~factor(race)+factor(smoke_flag)+factor(uinf_flag)+factor(hyper_flag),data=lowbwt)
summary(fit)
anova(fit)[["Sum Sq"]][1]/(anova(fit)[["Sum Sq"]][1]+anova(fit)[["Sum Sq"]][2])
sqrt(anova(fit)[["Sum Sq"]][2]/fit$df.residual)

fit<-aov(babywt~factor(uinf_flag)+factor(hyper_flag)+factor(smoke_flag)+factor(race),data=lowbwt)
summary(fit)
anova(fit)[["Sum Sq"]][1]/(anova(fit)[["Sum Sq"]][1]+anova(fit)[["Sum Sq"]][2])
TukeyHSD(fit, conf.level = .90)

fit<-aov(babywt~factor(smoke_flag),data=lowbwt)
summary(fit)
anova(fit)[["Sum Sq"]][1]/(anova(fit)[["Sum Sq"]][1]+anova(fit)[["Sum Sq"]][2])
TukeyHSD(fit, conf.level = .90)



r.stres <- rstandard(r)
plot(r$fitted.values, r.stres, pch = 16)
abline(0,0, lty=2, col="red")
shapiro.test(r$residuals)
qqnorm(r$residuals)
qqline(r$residuals,col="red")
hist(r$residuals)

table(lowbwt$weight_flag)
prop.test(59,188,p=0.25,alternative = "greater")
