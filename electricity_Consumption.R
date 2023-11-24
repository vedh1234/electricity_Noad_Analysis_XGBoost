#install.packages("ggplot2")
#install.packages("caret")
library(ggplot2)
library(caret)
library(ggthemes)

df = read.csv("PJME_hourly.csv", header = TRUE, row.names = NULL)
head(df)
tail(df)
View(df)
#install.packages("xts")
library(xts)
library(lubridate)
# assuming df is the data frame with Datetime and PJME_MW columns
df_xts <- xts(df$PJME_MW, order.by = as.POSIXct(df$Datetime), keep.names = TRUE)
colnames(df_xts) <- "PJME_MW"
colnames(df_xts)
head(df_xts)


plot(index(df_xts),y = df_xts$PJME_MW)
ggplot(df_xts, aes(index(df_xts),y = PJME_MW)) +
  geom_point(shape = 1, size = 1, color = "blue")+
  labs(title = "PJME Energy Use in MW")+
  theme(plot.title = element_text(size = 20, face = "bold"))+
  scale_x_datetime(date_breaks = "1 day", date_labels = "%Y-%m-%d")+
  xlab("") + ylab("PJME Energy Use in MW")

ggplot(df_xts, aes(x=index(df_xts), y=PJME_MW)) +
  geom_line(color = "blue") +
  theme_bw() +
  labs(title = "PJME Energy Use in MW") +
  scale_x_datetime(date_breaks = "1000 day", date_labels = "%Y-%m-%d") +
  theme(plot.title = element_text(size = 20, face = "bold"))

df_xts['2015-01-01 01:00:00']
train <- df_xts[index(df_xts) < '2015-01-01 01:00:00', ]
head(train)
tail(train)
test <- df_xts[index(df_xts) >= '2015-01-01 01:00:00', ]
head(test)

x1<-index(train)
x1[1]

ggplot() + 
  geom_line(data=train, aes(x=index(train), y=PJME_MW, color="Training Set"), size=1) + 
  geom_line(data=test, aes(x=index(test), y=PJME_MW, color="Test Set"), size=1) + 
  geom_vline(xintercept=as.numeric(as.POSIXct("2015-01-01 01:00:00")), color="black", linetype="dashed") + 
  scale_color_manual(name = "", values = c("Training Set" = "blue", "Test Set" = "red")) + 
  labs(title="Data Train/Test Split", x="", y="PJME Energy Use in MW") + 
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

df_subset <- df_xts[as.Date("2010-01-01") < index(df_xts) & index(df_xts) < as.Date("2010-01-08"),]
df_subset
ggplot(df_subset, aes(x = index(df_subset), y = PJME_MW)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Week Of Data", x = "", y = "PJME Energy Use in MW") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%Y-%m-%d")


library(dplyr)
library(lubridate)


create_features <- function(df) {
  df$hour <- as.integer(format(index(df), "%H"))
  df$dayofweek <- as.integer(format(index(df), "%u")) - 1
  df$quarter <- as.integer(format(index(df), "%q"))
  df$month <- as.integer(format(index(df), "%m"))
  df$year <- as.integer(format(index(df), "%Y"))
  df$dayofyear <- as.integer(format(index(df), "%j"))
  df$dayofmonth <- as.integer(format(index(df), "%d"))
  df$weekofyear <- as.integer(format(index(df), "%V"))
  return(df)
}

df_xts <- create_features(df_xts)
df_xts
#install.packages("tidyverse")
library(tidyverse)

ggplot(df_xts, aes(x = factor(hour), y = PJME_MW)) +
  geom_boxplot() +
  labs(title = 'MW by Hour') +
  xlab('Hour') +
  ylab('PJME_MW')


ggplot(df_xts, aes(x=as.factor(month), y=PJME_MW)) +
  geom_boxplot(fill='skyblue') +
  scale_x_discrete(name='Month', labels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                          'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  labs(title='MW by Month') +
  theme(plot.title = element_text(size = 20, face = "bold"))



train <- create_features(train)
test <- create_features(test)

FEATURES <- c('dayofyear', 'hour', 'dayofweek', 'quarter', 'month', 'year')
TARGET <- 'PJME_MW'

X_train <- train[, FEATURES]
y_train <- train[, TARGET]

X_test <- test[, FEATURES]
y_test <- test[, TARGET]

#install.packages("xgboost")
library(xgboost)

params <- list(max_depth = 3, eta = 0.01)

xgb_model <- xgboost(data = X_train, label = y_train,
                     params = params,
                     nrounds = 1000,
                     early_stopping_rounds = 50,
                     objective = "reg:linear",
                     verbose = 100,
                     eval_metric = "rmse"
                     )
preds <- predict(xgb_model, newdata = X_test)
head(preds)



class(xgb_model)


fi<-xgboost::xgb.importance(model=xgb_model)


test$prediction <- predict(xgb_model, newdata = X_test)
test$prediction
df_xts$prediction <- test$prediction
df_xts$prediction["2014-01-01 01:00:00"]
df2 <- data.frame(df_xts)
df2 <- merge(df_xts, test['prediction'], by = 0, all.x = TRUE)
head(df2)
plot(df2$PJME_MW, type = "l", xlab = "Time", ylab = "PJME_MW", main = "Raw Data and Prediction")
points(df2$prediction, col = "red")
legend("topright", legend = c("Truth Data", "Predictions"), col = c("black", "red"), pch = c("-", ".")) 


ax <- df_xts[index(df_xts) > '2018-04-01' & index(df_xts) < '2018-04-08', 'PJME_MW'] %>%
  plot(figsize = c(15, 5), main = 'Week Of Data')
df_xts[index(df_xts) > '2018-04-01' & index(df_xts) < '2018-04-08', 'prediction'] %>%
  points(col = 'red')
legend('topleft', c('Truth Data', 'Prediction'), col = c('black', 'red'), pch = c('.', '.'))
 
score <- sqrt(mean((test$PJME_MW - test$prediction)^2))
cat(sprintf("RMSE Score on Test set: %.2f\n", score))

test$error <- abs(test$PJME_MW - test$prediction)
test$date <- as.Date(index(test))
aggregate(error ~ index(test), data=test, mean) %>%
  arrange(desc(error)) %>%
  head(10)
