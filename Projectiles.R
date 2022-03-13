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

# Generate Model
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
