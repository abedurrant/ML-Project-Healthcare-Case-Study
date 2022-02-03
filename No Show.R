library(ggplot2)


#Reading in the data that was created in Python
no_shows <- read.csv("C:/Users/abedu/Downloads/Cleaned_Data.csv")

no_shows$SMS_received = as.factor(no_shows$SMS_received)
no_shows$AppointmentDayofWeek = as.factor(no_shows$AppointmentDayofWeek)
no_shows$ScheduledDayofWeek = as.factor(no_shows$ScheduledDayofWeek)

#Plotting relationships
no_shows$No.show <- as.integer(no_shows$No.show=="Yes")
ggplot(data = no_shows, mapping = aes(x= Age,y = No.show)) + geom_point() + geom_smooth(se=FALSE) + xlab("Age of Patient") + ylab("Probability of No-show") + ggtitle("Patient's Missed Appointments by Age")
ggplot(data = no_shows, mapping = aes(x= ApptTimeAfterSchedule,y = No.show)) + geom_point() + geom_smooth(se=FALSE)+ xlab("Time Since Scheduling") + ylab("Probability of No-show") + ggtitle("Patient's No-Show vs Time Passed Since Scheduling")
ggplot(data = no_shows, mapping = aes(x= ApptTimeAfterSchedule,y = No.show)) + geom_point() + geom_smooth(se=FALSE)
ggplot(data = no_shows, mapping = aes(x= ScheduledHourofDay,y = No.show)) + geom_point() + geom_smooth(se=FALSE)
ggplot(data = no_shows, mapping = aes(x= AppointmentsMade,y = No.show)) + geom_point() + geom_smooth(se=FALSE)
ggplot(data = no_shows, mapping = aes(x= AppointmentsMissed,y = No.show)) + geom_point() + geom_smooth(se=FALSE)
ggplot(data = no_shows, mapping = aes(x= ProportionMissed,y = No.show)) + geom_point() + geom_smooth(se=FALSE)
ggplot(data = no_shows, mapping = aes(x= AppointmentDayofWeek,y = No.show)) + geom_boxplot()
ggplot(data = no_shows, mapping = aes(x= SMS_received,y = No.show)) + geom_boxplot()
ggplot(data = no_shows, mapping = aes(x= Age,y = No.show, col = SMS_received)) + geom_point() + geom_smooth(se=FALSE)
ggplot(data = no_shows, mapping = aes(x= ApptTimeAfterSchedule,y = No.show, col = SMS_received)) + geom_point() + geom_smooth() + xlab("Time Since Scheduling") + ylab("Probability of No-show") + ggtitle("Patient's No-Show vs Time Passed Since Scheduling")


#Looking at days of the week
mean(no_shows[no_shows$AppointmentDayofWeek==0,11])
mean(no_shows[no_shows$AppointmentDayofWeek==1,11])
mean(no_shows[no_shows$AppointmentDayofWeek==2,11])
mean(no_shows[no_shows$AppointmentDayofWeek==3,11])
mean(no_shows[no_shows$AppointmentDayofWeek==4,11])
mean(no_shows[no_shows$AppointmentDayofWeek==5,11])

#Text messages do seem to improve. They just aren't sent to patients with close to same day scheduling which makes sense.
morethan10 = no_shows[no_shows$ApptTimeAfterSchedule>=10,]
mean(morethan10[morethan10$SMS_received==1,11])
mean(morethan10[morethan10$SMS_received==0,11])

mean(no_shows[no_shows$SMS_received==0,11])
count(no_shows[no_shows$SMS_received==1,11])

#Fit Logistic Regression with most important from rf model
model = glm(No.show ~ Age + ApptTimeAfterSchedule + ScheduledHourofDay + AppointmentsMade + AppointmentDayofWeek+ProportionMissed+ SMS_received+ Age:SMS_received + ApptTimeAfterSchedule:SMS_received,family=binomial, data = no_shows)
summary(model)
#Age is not monotonistic which is why this model may not be the best choice. In the future buckets of categories of age could be used
#However I wanted to test one to see if it could compare well to a random forest

#Testing the best threshold
Threshold = seq(0,1,by=0.01)
false.negative.rate <- false.positive.rate <- true.positive.rate <- error.rate <- (0*Threshold-99)

for(i in 1:length(Threshold)){
  y.hat = ifelse(model$fitted.values < Threshold[i],0,1)
  error.rate[i] = mean(no_shows$No.show!=y.hat)
  TP = sum(no_shows$No.show==1 & y.hat==1)
  TN = sum(no_shows$No.show==0 & y.hat==0)
  FP = sum(no_shows$No.show==0 & y.hat==1)
  FN = sum(no_shows$No.show==1 & y.hat==0)
  false.negative.rate[i] = FN/(FN+TP) # 1-sensitivity
  false.positive.rate[i] = FP/(FP+TN) # 1-specificity
}

plot(Threshold,error.rate,ylim=c(0,1),ylab='Error Rate',type='l',lwd=2)
lines(Threshold,false.negative.rate,col=4,lty=2,lwd=2)
lines(Threshold,false.positive.rate,col=2,lty=3,lwd=2)

plot(false.positive.rate,1 - false.negative.rate
     ,xlab = "False Positive Rate (1-Specificity)",ylab = "True Positive Rate (Sensitivity)"
     ,col=4,type='l',lwd=2
)
abline(0,1,lty=3)


preds = model$fitted.values>.25
preds = as.numeric(preds)
preds = as.factor(preds)

#Import required library
library(caret)

#Creating confusion matrix
confusionMatrix(data=preds, reference = as.factor(no_shows$No.show))
