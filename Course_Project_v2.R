#######################
## Course Project
#####################
### Read Libraries
library(caret)
library(gbm)
library(AppliedPredictiveModeling)
library(randomForest)

## Read information
fitTrain <- read.csv("C:/Coursera/8_Machine_Leaning/Week4/Assignment/training.csv", stringsAsFactors= FALSE)
fitTest <- read.csv("C:/Coursera/8_Machine_Leaning/Week4/Assignment/testing.csv", stringsAsFactors= FALSE)

# Set seed 
set.seed(3433)

# split training data into training and testing data to develop the model in preparation for cross validation
inTrain <- createDataPartition(y=fitTrain$classe, p=0.7, list=FALSE)
training <- fitTrain[inTrain,]
testing <- fitTrain[-inTrain,]

### train using a random forest prediction 
modRF2 <- train(classe ~ raw_timestamp_part_1 + raw_timestamp_part_2 + cvtd_timestamp + 
           num_window +          roll_belt +         pitch_belt +           yaw_belt +      
           total_accel_belt +    gyros_belt_x +      gyros_belt_y +         gyros_belt_z +
           accel_belt_x +        accel_belt_y +      accel_belt_z +         magnet_belt_x + 
           magnet_belt_y +       magnet_belt_z +     roll_arm +             pitch_arm +     
           yaw_arm +             total_accel_arm +   gyros_arm_x +          gyros_arm_y +  
           gyros_arm_z +         accel_arm_x +       accel_arm_y +          accel_arm_z +    
           magnet_arm_x +        magnet_arm_y +      magnet_arm_z +         roll_dumbbell +   
           pitch_dumbbell+       yaw_dumbbell +      total_accel_dumbbell + gyros_dumbbell_x +
           gyros_dumbbell_y +    gyros_dumbbell_z +  accel_dumbbell_x +     accel_dumbbell_y +   
           accel_dumbbell_z +    magnet_dumbbell_x + magnet_dumbbell_y +    magnet_dumbbell_z  +  
           roll_forearm +        pitch_forearm +     yaw_forearm  +         total_accel_forearm +
           gyros_forearm_x +     gyros_forearm_y +   gyros_forearm_z +      accel_forearm_x +
           accel_forearm_y +     accel_forearm_z +   magnet_forearm_x +     magnet_forearm_y + magnet_forearm_z ,
           data=training,  method="rf", prox=TRUE, na.action(na.omit))

modRF2

#predict the classe for the testing set
# to see how cross validation 
pred1 <- predict(modRF2, testing)
# check the accuracy of the testing set
predAcc <- round(100*sum(pred1 == testing$classe)/length(testing$classe),3)
predAccs <- paste("Calculated Accuracy is: ",predAcc,"%", sep="")
print(predAccs)

# the sampling error is determined by the following equation 
saa <- round(100*(1- sum(pred1 == testing$classe)/length(testing$classe)),3)
saa1 <- paste("Sample Error is ",saa, "%", sep = "") 
print(c(saa1))

# predict the input test set
z2  <- predict(modRF2, fitTest)
z2
###  These numbers matched the quiz 20 for 20 

