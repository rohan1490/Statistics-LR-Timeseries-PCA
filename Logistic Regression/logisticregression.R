data <- read.csv("D:/Study/Statistics/TABA/Logistic Regression/Aging-and-Longevity_Dataset/Dataset.csv",header = TRUE, sep = ",")
head(data)
#df <- data[ ,c(7,10:12,14,20,25,27,30,49:50,70,75)]


library(Amelia)
#missmap(df)

#summary(df)
#str(df)

df1 <- data[ ,c(7,10:12,14,20,22,29,30,101,111)]

round(cor(df1),2)

round(cor(data[ ,c(7:120)]),2)

missmap(df1)
summary(df1)
str(df1)


table(df1$q1)

#Removing response 9 from q1
sum(df1$q1 ==9)
df1 <- df1[!(df1$q1 == 9),]

#Renaming row names
row.names(df1) <- 1:nrow(df1)

#Coding 1 and 0 for response of q1
df1$q1 <- ifelse(df1$q1 == 2, 0, 1)

table(df1$q1)

table(df1$q4a)
#Removing response 7 and 9 from  q4a
df1 <- df1[!(df1$q4a == 7),]
df1 <- df1[!(df1$q4a == 9),]
table(df1$q4a)
table(df1$q1)

#Removing response 7 and 9 from  q4b
df1 <- df1[!(df1$q4b == 7),]
df1 <- df1[!(df1$q4b == 9),]
table(df1$q4b)
table(df1$q1)

#Removing response 7 and 9 from  q4c
df1 <- df1[!(df1$q4c == 7),]
df1 <- df1[!(df1$q4c == 9),]
table(df1$q4c)
table(df1$q1)

#Removing response 7 and 9 from  q4e
df1 <- df1[!(df1$q4e == 7),]
df1 <- df1[!(df1$q4e == 9),]
table(df1$q4e)
table(df1$q1)


#Removing response 9 from  q6b
table(df1$q6b)
df1 <- df1[!(df1$q6b == 9),]
table(df1$q6b)
table(df1$q1)

#Removing response 9 from  q6d
table(df1$q6d)
df1 <- df1[!(df1$q6d == 9),]
table(df1$q6d)
table(df1$q1)


#Removing response 9 from  q7a
table(df1$q7a)
df1 <- df1[!(df1$q7a == 9),]
table(df1$q7a)
table(df1$q1)

#Removing response 9 from  q7b
table(df1$q7b)
df1 <- df1[!(df1$q7b == 9),]
table(df1$q7b)
table(df1$q1)


#Removing response 99 from  agerec
table(df1$agerec==99)
df1 <- df1[!(df1$agerec == 99),]
table(df1$agerec==99)
table(df1$q1)

#Removing response 99 from  agerec
table(df1$employ)
df1 <- df1[!(df1$employ == 9),]
table(df1$employ)
table(df1$q1)



#Renaming row names
row.names(df1) <- 1:nrow(df1)



str(df1)

df1$q1 <- as.factor(df1$q1)
df1$q4a <- as.factor(df1$q4a)
df1$q4b <- as.factor(df1$q4b)
df1$q4c <- as.factor(df1$q4c)
df1$q4e <- as.factor(df1$q4e)
df1$q6b <- as.factor(df1$q6b)
df1$q6d <- as.factor(df1$q6d)
df1$q7a<- as.factor(df1$q7a)
df1$q7b<- as.factor(df1$q7b)
df1$employ<- as.factor(df1$employ)
str(df1)
table(df1$Y)
#Renaming columns
names(df1)[1] <- "Y"
names(df1)[2:11] <- paste("X", 1:10, sep="")



#Model 1
m1 <- glm(Y ~ ., data = df1, family = "binomial")
m1
summary(m1)
par(mfrow = c(2, 2))
plot(m1)
exp(coef(m1))

Anova(m1, type="II", test="Wald")



#Step of Model 1 to Model 2
m2 <- step(m1, approximation=FALSE)
m2
summary(m2)
par(mfrow = c(2,2))
plot(m2)
round(exp(coef(m2)),2)
round(coef(m2),2)
Anova(m2, type="II", test="Wald")

max(cooks.distance(m2))

round(coef(m2),2)





predicted.data <- data.frame(probability.of.y=m2$fitted.values,y=df1$Y)
predicted.data <- predicted.data[order(predicted.data$probability.of.y, decreasing = FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)
library(ggplot2)
library(cowplot)
ggplot(data=predicted.data, aes(x=rank, y=probability.of.y))+
geom_point(aes(color=y), alpha=1, shape=4, stroke=2)+
xlab("Index")+
ylab("Predicted probabilty of Y")





cooksD <- cooks.distance(m2)
#n <- nrow(df1)-7
par(mfrow = c(1, 1))
plot(cooksD, main = "Cooks Distance for Influential Obs")
abline(h = 1, lty = 1, col = "steelblue")

influential_obs <- as.numeric(names(cooksD)[(cooksD > 1)])
#outliers_removed <- df1[-influential_obs, ]

library(car)
Anova(m1, type="II", test="Wald")

library(rcompanion)
m1r2 <- nagelkerke(m1)

m1r2$Pseudo.R.squared.for.model.vs.null[1,1]
m1r2$Pseudo.R.squared.for.model.vs.null[2,1]
m1r2$Pseudo.R.squared.for.model.vs.null[3,1]


m2r2 <- nagelkerke(m2)
m2r2$Pseudo.R.squared.for.model.vs.null[1,1]
m2r2$Pseudo.R.squared.for.model.vs.null[2,1]
m2r2$Pseudo.R.squared.for.model.vs.null[3,1]

ic <- data.frame(Model = c("Model m1", "Model m2"),
                 McFadden_R2 = c(round(m1r2$Pseudo.R.squared.for.model.vs.null[1,1],3), round(m2r2$Pseudo.R.squared.for.model.vs.null[1,1],3)),
                 Cox_Snell_R2 = c(round(m1r2$Pseudo.R.squared.for.model.vs.null[2,1],3), round(m2r2$Pseudo.R.squared.for.model.vs.null[2,1],3)),
                 Nagelkerke_R2  = c(round(m1r2$Pseudo.R.squared.for.model.vs.null[3,1],3), round(m2r2$Pseudo.R.squared.for.model.vs.null[3,1],3)),
                 stringsAsFactors = FALSE)
ic


str(m1r2)
vif(m2)


library(ResourceSelection)
hl <- hoslem.test(m2$y, fitted(m2), g=10)
hl

for (i in 10:20) {
  print(hoslem.test(m2$y, fitted(m2), g=i)$p.value)
}

res <- predict(m1, df1, type="response")
res
head(res)

res[res>0.5] <- 1
res[res<=0.5] <- 0
res <- as.numeric(res)
res <- as.factor(res)
str(res)
str(df1$q1)

library(InformationValue)
library(caret)
library(e1071)
library(pROC)

confusionMatrix(res,df1$Y)

res1 <- predict(m2, df1, type="response")
res1
head(res1)

res1[res1>0.5] <- 1
res1[res1<=0.5] <- 0
res1 <- as.numeric(res1)
res1 <- as.factor(res1)
str(res1)

confusionMatrix(res1,df1$Y)

rsquared <- function(created_model) 
  {
  dev <- created_model$deviance
  null_dev <- created_model$null.deviance
  model_n <- length(created_model$fitted.values)
  R_l <- 1 - dev / null_dev
  R_cs <- 1 - exp(-(null_dev - dev) / model_n)
  R_n <- R_cs / (1 - exp(-(null_dev / model_n)))
  cat("Pseudo R-squared for logistic regression model\n\n")
  cat("Hosmer and Lemeshow R-squared\t", round(R_l, 3), "\n")
  cat("Cox and Snell R-squared\t\t", round(R_cs, 3), "\n")
  cat("Nagelkerke R-squared\t\t", round(R_n, 3), "\n")
  }

rsquared(m1)
rsquared(m2)
rsquared(m3)


library(pROC)
roc <- roc(df1$Y,m2$fitted.values)
plot(roc)


library(ROCR)
ROCRpred <- prediction(predict(m2, df1, type="response"), df1$Y)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

