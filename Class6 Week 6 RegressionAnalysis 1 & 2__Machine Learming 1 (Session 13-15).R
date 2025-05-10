#25-04-2025 DAY 10 (Class 1)
#REGRESSION ANALYSIS (SESSION 13)
install.packages('tidyverse')
install.packages('UsingR')
install.packages('manipulate')
install.packages('broom')
install.packages('BSDA')
install.packages('ggfortify')
library(tidyverse)
library(UsingR)
library(manipulate)
library(broom)
library(BSDA)
library(ggfortify)
library(plotly)


library(UsingR)
data(Galton)
par(mfrow=c(1,2))
hist(galton$child,breaks=100, col = "lightblue", border = "pink")
hist(galton$parent,breaks=100, col = "lightblue", border = "pink")

#The empirical mean (ls)
ggplot(galton, aes(x = child)) +
  geom_histogram(binwidth = 1, colour = "lightblue", fill = "lightblue") +
  geom_vline(xintercept = mean(galton$child), colour = "blue", lwd = 5)

#Visualizing children & parents’ heights
ggplot(galton, aes(parent, child)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Regression through the origin
lm(I(child - mean(child)) ~ I(parent - mean(parent)) - 1, data = galton)


#Visualizing the best fit line
freq_galton <- as.data.frame(table(galton$child, galton$parent))
names(freq_galton) <- c("child", "parent", "freq")
plot(as.numeric(as.vector(freq_galton$parent)), 
     as.numeric(as.vector(freq_galton$child)), 
     pch = 21, col = "black", bg = "lightblue",
     cex = .05 * freq_galton$freq,
     xlab = "Parents", ylab = "Children")

lm1 <- lm(galton$child ~ galton$parent)
lines(galton$parent, lm1$fitted, COL = "red", lwd = 3)


#USING DIAMOND DATA
library(UsingR)
data(diamond)

ggplot(diamond, aes(x = carat, y = price)) +
  geom_point(color = "black", size = 1.1, shape = 21) +
  geom_smooth(method = lm, se = FALSE, col = "darkblue", linewidth = 1.1) + 
  theme_classic() + 
  theme(panel.background = element_rect(fill = "lightblue")) +
  xlab("Mass (Carats)") + 
  ylab("Price (SIN $)")

#Fitting the linear regression model
fit <- lm(price ~ carat, data = diamond)

coef(fit)

## a - is the expected price of a diamond with zero carat
round(coef(fit)[1], 2)

##b (SIN) dollar increase in price for every carat increase in mass of diamond.
round(coef(fit)[2], 2)

fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
fit2

#Predicting the price of a diamond
library(dplyr)
fit <- lm(price ~ carat, data = diamond)
fit

set.seed(234)
newX <- tibble(runif(48, 0.2, 0.5))
newX <- round(newX, 2)
newX

coef(fit)[1] + coef(fit)[2] * newX

predict(fit, carat = newX)

price_predicted = round(predict(fit, carat = newX), 2)
price_predicted

price_predicted2 <- diamond %>% 
  mutate(prediction = round(predict(fit, carat = newX), 2))
price_predicted2


#Plotting actual vs predicted
library(dplyr)
fit <- lm(price ~ carat, data = diamond)
fit

set.seed(234)
newX <- tibble(runif(48, 200, 1000))
newX <- round(newX, 2)
newX

coef(fit)[1] + coef(fit)[2] * newX
predict(fit, carat = newX)

price_predicted = round(predict(fit, carat = newX), 2)
price_predicted

price_predicted2 <- diamond %>% 
  mutate(price_predicted = round(predict(fit, carat = newX), 2)) %>%
  rename(actual_price = price)
price_predicted2


price_predicted3 <- gather(price_predicted2, variable, value, -carat)
price_predicted3
#Fitted values - predictions on Originaldataset

#Showing predictions
foma <- ggplot(price_predicted3, aes(carat, value)) +
  geom_point(aes(color = variable), size = 4, alpha = 1/2) + 
  theme_bw(base_family = "Times") +
  geom_smooth(size = 4, linetype = 3, method = "lm", se = FALSE) +
  labs(title = "LR actual versus predicted") +
  labs(x = "Carat", y = "Prices")
foma  
ggplotly(foma)


#Residuals function in LR
library(dplyr)
library(ggplot2)

fit <- lm(price ~ carat, data = diamond)
fit

coef(fit)[1] + coef(fit)[2] * newX
predict(fit, carat = newX)

price_predicted = round(predict(fit, carat = newX), 2)
price_predicted

price_predicted2 <- diamond %>% 
  mutate(price_predicted = round(predict(fit, carat = newX), 2)) %>%
  rename(actual_price = price)
price_predicted2

price_predicted3 <- gather(price_predicted2, variable, value, -carat)
price_predicted3

#Residuals
residuals(fit)
round(residuals(fit), 2)

#equivalently:
as.data.frame(price_predicted2$price - price_predicted2$prediction)


#Residuals are the gaps to the red lines
library(ggplot2)
data(diamond)
view(diamond)

y <- diamond$price; x <- diamond$carat; n <- length(y)

fit <- lm(y ~ x)
fit

e <- resid(fit)
e

yhat <- predict(fit)
yhat

ggplot(data = diamond, aes(x = carat, y = price)) +
  geom_point(shape = 21, color = "blue", size = 1.4) +
  geom_abline(lwd = 2) +
  geom_line(aes(x = carat, y = yhat), color = "red", lwd = 2) +
  xlab("Mass (carats)") +
  ylab("Price (SIN $)") 


#Summary function
fit <- lm(price ~ carat, data = diamond)

summary(fit)

# Call
# Residuals
# Coefficients
# Model Metrics


#tidy summary
library(broom)
fit <- lm(price ~ carat, data = diamond)

summary(fit)

# Call
# Residuals
# Coefficients
# Model Metrics

tidy(fit)
augment(fit)
glance(fit)

#fit %>%
#tidy() 
#this is the same as tidy(fit)


#R^2 - Coefficient Determination
fit <- lm(price ~ carat, data = diamond)

# extract the R^2 value from the regression result
fit %>% 
  glance()
#this is the same as tidy(glance)
# It's just correlation squared

diamond %>%
  summarise(
    coeff_determination = cor(price, carat) ^ 2
  )


#Residual standard error (RSE)
fit <- lm(price ~ carat, data = diamond)

# extract the R^2 value from the regression result
fit %>% 
   glance()

# Extract the RSE with broom pull ()
fit %>% 
  glance() %>% 
  pull(sigma)

# The difference between predicted price and observed price is typically about SIND 32.
fit %>% 
  tidy() %>% 
  pull(estimate)


# Calculating RSE: residuals squared
diamond %>%
  mutate(
    residuals_sq = residuals(fit) ^ 2
  ) %>%
  summarise(
    resid_sum_of_sq = sum(residuals_sq)
  )


#Calculating RSE: square root of ratio
# Residual Square Error
fit <- lm(price ~ carat, data = diamond)

diamond %>%
  mutate(
    residuals_sq = residuals(fit) ^ 2
  ) %>%
  summarise(
    resid_sum_of_sq = sum(residuals_sq),
    deg_freedom = n() - 2,
    rse = sqrt(resid_sum_of_sq / deg_freedom)
  )


# Residual Mean Square Error
fit <- lm(price ~ carat, data = diamond)

diamond %>%
  mutate(
    residuals_sq = residuals(fit) ^ 2
  ) %>%
  summarise(
    resid_sum_of_sq = sum(residuals_sq),
    n_obs = n(),
    rse = sqrt(resid_sum_of_sq /n_obs)
  )


#Non-linear data
library(ggplot2)
set.seed(234)

abi <- runif(100, -3, 3); sola <- abi + sin(abi) + rnorm(100, sd = .2)
ggplot(data = data.frame(abi, sola), aes(x = abi, y = sola)) + 
  geom_point() + 
  geom_smooth(method = "lm")

abi <- runif(100, -3, 3); sola <- abi + sin(abi) + rnorm(100, sd = .2)
ggplot(data = lm(sola ~ abi), aes(x = abi, y = resid(lm(sola ~ abi)))) +
  geom_point() +
  geom_abline(intercept = 0)


#Heteroskedasticity
#Fitting the regression

x <- runif(100, 0, 6); y <- x + rnorm(100,  mean = 0, sd = .001 * x); 

library(ggplot2)
ggplot(data = data.frame(x, y), aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm")


#No causality between explanatory variable(s) and the residuals
x <- runif(100, 0, 6); y <- x + rnorm(100,  mean = 0, sd = .001 * x); 
x

ggplot(data = lm(y ~ x)) + 
  geom_point(aes(x = x, y = resid(lm(y ~ x)))) + 
  geom_hline(aes(yintercept = 0))


#Diamond example
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)

# R in-built computation
summary(fit)$sigma

# Self-calculated
sqrt(sum(resid(fit)^2) / (n - 2))


#Outliers, leverage & influence
library(ggfortify)
library(tidyverse)
library(broom)

fit <- lm(price ~ carat, data = diamond)

autoplot(fit,
         which = 1:4,
         nrow = 4,
         ncol = 1
)


# Highly leveraged 
fit %>%
augment() %>%
dplyr::select(price, carat, leverage = .hat) %>%
arrange(desc(leverage)) %>%
head()

# Cook's distance
cooks.distance(fit)




#25-04-2025 DAY 11 (Class 2)
# REGRESSION ANALYSIS 2 (SESSION 14)
install.packages('tidyverse')
install.packages('UsingR')
install.packages('manipulate')
install.packages('broom')
install.packages('BSDA')
install.packages('ggfortify')
library(UsingR)
library(manipulate)
library(broom)
library(BSDA)
library(ggfortify)


data("mtcars")
model <- lm(mpg ~ wt + cyl, data = mtcars)
summary(model)
model$coefficients[1]


#How to Access the fit of a Multivariate Regression Model
library(ggplot2)
model <- lm(mpg ~ wt + cyl, data = mtcars)

ggplot(data.frame(model$residuals), aes(x = model$fitted.values, y = model$residuals)) +
  geom_point() + 
  geom_hline(yintercept = 0) +
  labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals")
  

#Example Churning
library(dplyr)
head(churning)

churning %>%
  dplyr::select(Churn, tenure, MonthlyCharges)

churning$churn <- ifelse(churning$churn == "No", 0, 1)

model2 <- glm(churn ~ tenure + MonthlyCharges, data = churning, family = binomial)
summary(model2)


#Linear Probability Model
churning$churn <- ifelse(churning$churn == "No", 0, 1)
lmChurn <- lm(churning$churn ~ churning$tenure)
summary(lmchurn$coef)


#Churning logistic regression
churning$churn <- ifelse(churning$churn == "No", 0, 1)
logRegChurn <- glm(churning$churn ~ churning$tenure, family = "binomial")
summary(logRegChurn)


#Churning fitted values
churning$churn <- ifelse(churning$churn == "No", 0, 1)
logRegChurn <- glm(churning$churn ~ churning$tenure, family = "binomial")

plot(churning$tenure, logRegChurn$fitted, pch = 19, col = "blue", xlab = "Tenure", ylab = "Prob of churning")


#Odds ratios and confidence intervals
churning$churn <- ifelse(churning$churn == "No", 0, 1)
logRegChurn <- glm(churning$churn ~ churning$tenure, family = "binomial")

exp(logRegChurn$coeff)
#The odds ratio for the value pf the intercept is the odds of "churning
# when tenure = 0. The odds ratio for the coefficient of tenure is the increase
# in odds above this value of the intercept when tenure increases by 1.

churning$churn <- ifelse(churning$churn == "No", 0, 1)
logRegChurn <- glm(churning$churn ~ churning$tenure, family = "binomial")
exp(confint(logRegChurn))


#ANOVA for logistic regression
churning$churn <- ifelse(churning$churn == "No", 0, 1)
logRegChurn <- glm(churning$churn ~ churning$tenure, family = "binomial")
anova(logRegChurn, test = "Chisq")




# Machine Learning (Session 15)
install.packages('tidyverse')
install.packages('UsingR')
install.packages('manipulate')
install.packages('broom')
install.packages('BSDA')
install.packages('ggfortify')
install.packages("caret")
install.packages("kernlab")
install.packages("ISLR")
install.packages("Hmisc")
install.packages("gridExtra")
library(tidyverse)
library(UsingR)
library(manipulate)
library(broom)
library(BSDA)
library(ggfortify)
library(caret)
library(kernlab)
library(ISLR)
library(Hmisc)
library(gridExtra)
library(ggplot2)


#Explore SPAM Data
library(kernlab)
data(spam)
head(spam)


#View the frequency and distribution of your in the SPAM data
library(ggplot2)
ggplot(data = spam, aes(x = your, fill = type)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("green", "red")) +
  xlab("Frequency of 'your'") +
  ggtitle("")


#Threshold for SPAM (your)
library(ggplot2)
ggplot(data = spam, aes(x = your, fill = type)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("green", "red")) +
  xlab("Frequency of 'your'") +
  geom_vline(xintercept = 0.5, color = "black")
  

#Checking accuracy - SPAM
prediction <- ifelse(spam$your > 0.5, "spam", "nonspam")
table(prediction, spam$type)/length(spam$type)


#Sample errors in prediction
#Small sample errors
library(kernlab); data(spam); set.seed(234)
small_spam_sub <- spam[sample(dim(spam)[1], size = 10),]
spamLabel <- (small_spam_sub$type == "spam") * 1 + 1
plot(small_spam_sub$capitalAve, col = spamLabel)


#Apply Rule 1 to small_spam_sub
rule1 <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x > 2.7] <- "spam"
  prediction[x < 2.40] <- "nonspam"
  prediction[x >= 2.40 & x <= 2.45] <- "spam"
  prediction[x > 2.45 & x <= 2.70] <- "nonspam"
  return(prediction)
}

library(kernlab); data(spam); set.seed(234)
small_spam_sub <- spam[sample(dim(spam)[1], size = 10),]
spamLabel <- (small_spam_sub$type == "spam") * 1 + 1
table(rule1(small_spam_sub$capitalAve), small_spam_sub$type)


#Apply Rule 2 to small_spam_sub
rule2 <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x > 2.8] <- "spam"
  prediction[x <= 2.8] <- "nonspam"
  return(prediction)
}

library(kernlab); data(spam); set.seed(234)
small_spam_sub <- spam[sample(dim(spam)[1], size = 10),]
spamLabel <- (small_spam_sub$type == "spam") * 1 + 1
table(rule2(small_spam_sub$capitalAve), small_spam_sub$type)


#Apply to bigger data
library(kernlab); data(spam); set.seed(234)
small_spam_sub <- spam[sample(dim(spam)[1], size = 10),]
spamLabel <- (small_spam_sub$type == "spam") * 1 + 1

table(rule1(spam$capitalAve), spam$type)
table(rule2(spam$capitalAve), spam$type)
mean(rule1(spam$capitalAve) == spam$type)
mean(rule2(spam$capitalAve) == spam$type)


#Access accuracy
sum(rule1(spam$capitalAve) == spam$type)
sum(rule2(spam$capitalAve) == spam$type)


#Data splitting using SPAM data
library(caret); library(kernlab); data(spam)

inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
dim(testing)


#Fitting a model
set.seed(234)
library(caret); library(kernlab); data(spam)

inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

modelFit <- train(type ~ ., data = training, method = "glm")
modelFit


##Final model
#set.seed(234)
#library(caret); library(kernlab); data(spam)
#
#inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
#training <- spam[inTrain,]
#testing <- spam[-inTrain,]
#
#modelFit <- train(type ~ ., data = training, method = "glm")
#modelFit$finalmodel


#Prediction
set.seed(234)
library(caret); library(kernlab); data(spam)

inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

modelFit <- train(type ~ ., data = training, method = "glm")

predictions <- predict(modelFit, newdata = testing)
predictions


#COnfusion Matrix
set.seed(234)
library(caret); library(kernlab); data(spam)

inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

modelFit <- train(type ~ ., data = training, method = "glm")

predictions <- predict(modelFit, newdata = testing)

confusionMatrix(predictions, testing$type)


#K-fold
set.seed(234)
folds <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = TRUE)
glimpse(folds)
sapply(folds, length)
folds[[1]][1:10]


#Return test
set.seed(234)
folds <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = FALSE)
glimpse(folds)
sapply(folds, length)
folds[[1]][1:10]


#Resampling
set.seed(234)
folds <- createResample(y = spam$type, times = 10, list = TRUE)
glimpse(folds)
sapply(folds, length)
folds[[1]][1:10]


#Setting the seed
#Seed example
set.seed(44)

inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

modelFit2 <- train(type ~ ., data = training, method = "glm")
modelFit2


set.seed(1)

inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

modelFit3 <- train(type ~ ., data = training, method = "glm")
modelFit3

#Another Example: predicting wages
library(ISLR); library(ggplot2); library(caret)
data(Wage)
glimpse(Wage)
summary(Wage)

#Get training / test sets
library(ISLR)
data(Wage)

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)


#Feature plot
library(ISLR)
data(Wage)

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

featurePlot(x = training[, c("age", "education", "jobclass")],
            y = training$wage,
            plot = "pairs")


#Qplot
library(ISLR)
data(Wage)

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

qplot(age, wage, data = training)


#Qplot with color
library(ISLR)
data(Wage)

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

qplot(age, wage, colour = jobclass, data = training)


#Add regression smooth
library(ISLR)
data(Wage)

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

qq <- qplot(age, wage, colour = jobclass, data = training) 
qq + geom_smooth(method = 'lm', formula = y ~ x)


#making factors (Hmisc package)
library(ISLR)
library(caret)
library(Hmisc)
data(Wage)
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
cutWage <- cut2(training$wage, g = 3)
cutWage
table(cutWage)


#Boxplots with cut2
library(ISLR)
library(caret)
library(Hmisc)
data(Wage)
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
cutWage <- cut2(training$wage, g = 3)

p1 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c("boxplot"))
p1


#Overlay the boxplots
library(Hmisc)
library(gridExtra)
data(Wage)
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
cutWage <- cut2(training$wage, g = 3)

p1 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c("boxplot"))
p1

p2 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c("boxplot", "jitter"))
grid.arrange(p1, p2, ncol = 2)


#Proportion tables
library(Hmisc)
data(Wage)
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
cutWage <- cut2(training$wage, g = 3)

t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1, 1)


#Density plots
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

qplot(wage, color = education, data = training, geom = "density")

