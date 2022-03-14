# Import Data
#projectileData = read.csv(file.choose(new = FALSE), header = FALSE)
projectileData = read.csv('projectiles.csv', header = FALSE)
attach(projectileData)
head(projectileData, 10)

# Cleaning/Formatting Data
dataLength = nrow(projectileData)
X0=c()
X1=c()
Y0=c()
Y1=c()
RESULT_X=c()
RESULT_Y=c()
TIME_SINCE_LAUNCH=c()
i = 1
while (i <= dataLength) {
  end = 2
  while((i+end) <= dataLength && V1[i+end] != 0) {
    X0 = append(X0, V2[i])
    X1 = append(X1, V2[i+1])
    Y0 = append(Y0, V3[i])
    Y1 = append(Y1, V3[i+1])
    RESULT_X = append(RESULT_X, V2[i+end])
    RESULT_Y = append(RESULT_Y, V3[i+end])
    TIME_SINCE_LAUNCH = append(TIME_SINCE_LAUNCH, (end)*0.1)
    end = end + 1
  }
  if  ((i+2) <= dataLength && V1[i+2] == 0) {
    i = i+1
  }
  i = i+1
}

# Test Approach
## Train Set
X0_TRAIN=X0[1:8000]
X1_TRAIN=X1[1:8000]
Y0_TRAIN=Y0[1:8000]
Y1_TRAIN=Y1[1:8000]
RESULT_X_TRAIN=RESULT_X[1:8000]
RESULT_Y_TRAIN=RESULT_Y[1:8000]
TIME_SINCE_LAUNCH_TRAIN=TIME_SINCE_LAUNCH[1:8000]

## Test set
X0_TEST=X0[8001:dataLength]
X1_TEST=X1[8001:dataLength]
Y0_TEST=Y0[8001:dataLength]
Y1_TEST=Y1[8001:dataLength]
RESULT_X_TEST=RESULT_X[8001:dataLength]
RESULT_Y_TEST=RESULT_Y[8001:dataLength]
TIME_SINCE_LAUNCH_TEST=TIME_SINCE_LAUNCH[8001:dataLength]

## Generate Model With Train Set
xModel = lm(RESULT_X_TRAIN ~ X0_TRAIN + X0_TRAIN*TIME_SINCE_LAUNCH_TRAIN + X1_TRAIN*TIME_SINCE_LAUNCH_TRAIN)
yModel = lm(RESULT_Y_TRAIN ~ Y0_TRAIN + Y0_TRAIN*TIME_SINCE_LAUNCH_TRAIN + Y1_TRAIN*TIME_SINCE_LAUNCH_TRAIN + TIME_SINCE_LAUNCH_TRAIN + I(TIME_SINCE_LAUNCH_TRAIN^2))

## Test model:
newXdata <- data.frame(X0_TRAIN=X0_TEST,
                       X1_TRAIN=X1_TEST,
                       TIME_SINCE_LAUNCH_TRAIN=TIME_SINCE_LAUNCH_TEST)
newYdata <- data.frame(Y0_TRAIN=Y0_TEST,
                       Y1_TRAIN=Y1_TEST,
                       TIME_SINCE_LAUNCH_TRAIN=TIME_SINCE_LAUNCH_TEST)
newX = predict(xModel, newdata = newXdata)
newY = predict(yModel, newdata = newYdata)
squaredErrorX = (RESULT_X_TEST-newX)^2
squaredErrorY = (RESULT_Y_TEST-newY)^2
str = sprintf("MSE-X on test set: %s | MSE Y on test set %s", mean(squaredErrorX), mean(squaredErrorY))
print(str)

singlePrediction <- function(x0, x1, y0, y1, time, trueX, trueY) {
  newXdata <- data.frame(X0_TRAIN=c(x0),
                         X1_TRAIN=c(x1),
                         TIME_SINCE_LAUNCH_TRAIN=c(time))
  newYdata <- data.frame(Y0_TRAIN=c(y0),
                         Y1_TRAIN=c(y1),
                         TIME_SINCE_LAUNCH_TRAIN=c(time))
  
  #use the fitted model to predict the value for the new observation
  newX = predict(xModel, newdata = newXdata)
  newY = predict(yModel, newdata = newYdata)
  str = sprintf("Initial Conditions: X0 = %s, X1 = %s, Y0 = %s, Y1 = %s, TIME_SINCE_LAUNCH = %s", x0, x1, y0, y1, time)
  print(str)
  str = sprintf("Squared error: XError^2=%s | YError^2=%s", (trueX-newX[1])^2, (trueY-newY[1])^2)
  print(str)
}
## High angle vs Low angle
print("High Angle Test")
singlePrediction(0, 0.6, 0, 2.939, 6, 36, 2.88)
print("Low Angle Test")
singlePrediction(0, 2.9, 0, 0.768, 1.6, 46.4, 0.528)
## High power vs Low power
print("High Power Test")
singlePrediction(0, 5, 0, 5, 10.3, 515, 0.206)
print("Low Power Test")
singlePrediction(0, 0.5, 0, 0.5, 1.1, 5.5, 0.11)

# Generate Full Model With All Data
xModel = lm(RESULT_X ~ X0 + X0*TIME_SINCE_LAUNCH + X1*TIME_SINCE_LAUNCH)
yModel = lm(RESULT_Y ~ Y0 + Y0*TIME_SINCE_LAUNCH + Y1*TIME_SINCE_LAUNCH + TIME_SINCE_LAUNCH + I(TIME_SINCE_LAUNCH^2))
summary(xModel)
summary(yModel)

# This function calculates the trajectory of a projectile till it hits the ground or till 100 seconds elapse (whichever is first)
# given the x and y coordinates of the projectile (measured 0.1s apart)
calculateTrajectory <- function(x0, x1, y0, y1) {
  X = c()
  Y = c()
  for (time in seq(0, 100, by=0.1)) {

    newXdata <- data.frame(X0=c(x0),
                           X1=c(x1),
                           TIME_SINCE_LAUNCH=c(time))
    newYdata <- data.frame(Y0=c(y0),
                           Y1=c(y1),
                           TIME_SINCE_LAUNCH=c(time))
    
    #use the fitted model to predict the value for the new observation
    newX = predict(xModel, newdata = newXdata)
    newY = predict(yModel, newdata = newYdata)
    if (newY[1] < 0 && time != 0) {
      break
    }
    str = sprintf("Time: %s | Coordinates: (%s, %s)", time, newX[1], newY[1])
    print(str)
    X = append(X, newX[1])
    Y = append(Y, newY[1]) 
    plot(X,Y)
  }

}

# Evaluate Model:
#A1
calculateTrajectory(0.0 ,0.707106781187, 0.0, 0.658106781187)
calculateTrajectory(0.0 ,0.5437847, 0.0, 0.20457096)
