#import the data
weight <- read.csv(file.choose())
View(weight)
#exploring the data
summary(weight)

install.packages("lattice")
library(lattice)
#visualizing the data 
dotplot(weight$Weight.gained..grams.)
dotplot(weight$Calories.Consumed)

boxplot(weight$Weight.gained..grams.,col='red',horizontal = T)
boxplot(weight$Calories.Consumed,col = 'yellow')

attach(weight)
#different visualization techniques
hist(Weight.gained..grams.)
hist(Calories.Consumed)

#normality plots
qqnorm(Weight.gained..grams.)
qqline(Weight.gained..grams.)

qqnorm(Calories.Consumed)
qqline(Calories.Consumed)

#scatter plot
plot(Weight.gained..grams.,Calories.Consumed,main = 'scatterplot',col = "grey",col.lab="blue",
     col.main="red",xlab = 'Weight gained in grams.',ylab = 'Calories Consumed',pch=20)

#correlation
cor(Weight.gained..grams.,Calories.Consumed)#0.946991

#model building
weight_model <- lm(Weight.gained..grams.~Calories.Consumed,data = weight)
summary(weight_model)# p-value = 2.856e-07 <0.05 which is significant
                     # R-squared = 0.8968
                     # Co-efficients are significant 
confint(weight_model,level = 0.95) #concidering confidence interval of 0.95

pred <- predict(weight_model,interval = "predict")
pred <- as.data.frame(pred)
View(pred)

#check for correlation between fited value and original dataset
cor(pred$fit,weight$Weight.gained..grams.)#0.946991

##for better accuracy and R-squared value, we use sqrt function##
weight_model_sqrt <- lm(sqrt(Weight.gained..grams.)~Calories.Consumed,data = weight)
summary(weight_model_sqrt) # p-value = 9.56e-08 <0.05 which is significant
                           # R-squared = 0.9139
                           # Co-efficients are significant 
confint(weight_model_sqrt,level = 0.95)
pred1 <- predict(weight_model_sqrt,interval = "predict")
pred1 <- as.data.frame(pred1)
View(pred1)
cor(pred1$fit,weight$Weight.gained..grams.)#0.946991
 
##for better accuracy and R-squared value, we use log function##
weight_model_log <- lm(log(Weight.gained..grams.)~log(Calories.Consumed),data = weight)
summary(weight_model_log) # p-value = 3.168e-06 <0.05 which is significant
                        # R-squared = 0.8465
                        # Co-efficients are significant 
confint(weight_model_log,level = 0.95)
pred2 <- predict(weight_model_log,interval = "predict")
pred2 <- as.data.frame(pred2)
View(pred2)
cor(pred2$fit,weight$Weight.gained..grams.)#0.8987


##for better accuracy and R-squared value, we use log and sqrt function##
weight_model_log_sqrt <- lm(log(Weight.gained..grams.)~(Calories.Consumed*Calories.Consumed),data = weight)
summary(weight_model_log_sqrt) # p-value = 8.018e-07 <0.05 which is significant
                               # R-squared = 0.8776
                               # Co-efficients are significant 
confint(weight_model_log_sqrt,level = 0.95)
pred3 <- predict(weight_model_log_sqrt,interval = "predict")
pred3 <- as.data.frame(pred3)
View(pred3)
cor(pred3$fit,weight$Weight.gained..grams.)
