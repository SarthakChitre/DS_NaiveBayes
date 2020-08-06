############Naive Bayes Model
library(e1071)
library(ggplot2)
library(mlbench)
library(caret)
library(psych)

train_sal=read.csv(file.choose())
str(train_sal)
test_sal=read.csv(file.choose())
train_sal$Salary=as.factor(train_sal$Salary)
test_sal$Salary=as.factor(test_sal$Salary)



model <- naiveBayes(train_sal$Salary ~., data = train_sal)
model
model_pred <- predict(model,test_sal[,-14])
mean(model_pred == test_sal$Salary) #81.93
confusionMatrix(model_pred,test_sal$Salary)


ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$age, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$hoursperweek, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=train_sal,aes(x = train_sal$age, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Black')

ggplot(data=train_sal,aes(x = train_sal$sex, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=train_sal,aes(x = train_sal$race, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Black')
