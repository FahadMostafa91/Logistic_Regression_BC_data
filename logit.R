library(corrplot)
correlations<-cor(breast[,1:9])
corrplot(correlations,method="circle")

# To understand the structure of dataset
str(breast)

#https://www.datacamp.com/community/tutorials/logistic-regression-R

pairs(breast)
pairs(breast[,1:9])

breast$Classification<- factor(breast$Classification)
mylogit <- glm(breast$Classification~breast$Age+breast$BMI+breast$Glucose+
                 breast$Insulin+breast$HOMA+breast$Leptin+breast$Adiponectin+breast$Resistin+breast$MCP.1, 
               data=breast, family = "binomial")

  

confint.default(mylogit)

exp(coef(mylogit))
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

b <- predict(mylogit, data=breast, type = "response")
plot(b)

# confirmatory analysis
#---------------------------------------------------------
library(readxl)
conf <- read_excel("C:/Users/gmostafa/Desktop/Multivariate Analysis/Project/conf.xlsx")
View(conf)
pairs(conf)
pairs(conf[,1:9])
mylogit2 <- glm(conf$Classification~conf$Age+conf$BMI+conf$Glucose+conf$Resistin,
               data=conf, family = "binomial")

plot(mylogit2,data=conf)

s2 <- predict(mylogit2, data=conf, type = "response")
plot(s2)
summary(s2)
boxplot(s2)

#https://www.datacamp.com/community/tutorials/confusion-matrix-calculation-r
class_prediction <-
  ifelse(s2 > 0.50,
         "Healthy Control",
         "Cancer found"
  )
table(class_prediction)
table(class_prediction, test[["Class"]])
confusionMatrix(s2, mylogit2)
