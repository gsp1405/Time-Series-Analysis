## Code used in the slides for lecture 2 (With some comments)

## Entering data:
data <- data.frame(t = 1:5, 
                   y = c(0.2, 1.3, 1.9, 2.3, 1.9),
                   z = c(0.4, 1.2, 2.3, 3.4, 4.3))

## Plotting
par(mar=c(3,3,1,1), mgp=c(2,0.7,0))
matplot(data$t, data[,-1], col=1:2, type="o", pch=1, lty=1, xlab="Time", ylab="Value")
legend("topleft", legend = c("y","z"), lty=1, pch=1, col=1:2)

## Manual solution
# Note that '%*%' is matrix multiplication and 'solve' is used for the inverse
x <- cbind(1, data$z, data$z^2)
(thetahat <- solve( t(x) %*% x ) %*% t(x) %*% data$y)

## Using lm
lm1 <- lm(y ~ z + I(z^2), data=data) ## Notice the I(...) to get what is calculated instead of interaction
summary(lm1)

data$residuals <- lm1$residuals
data$fit <- lm1$fitted.values

par(mfrow = c(2,1))
plot(y~z, data)
lines(fit~z, data)
legend("bottomright", legend = c("Observed","Fitted"), lty=c(NA,1), pch=c(1,NA))

plot(residuals~z,data)

## The graphs in the slides ...
require(reshape2)
require(ggplot2)

data <- matrix( c(  1 , 0.2 , 0.4 ,
      2 , 1.3 , 1.2 ,
      3 , 1.9 , 2.3 ,
      4 , 2.3 , 3.4 ,
      5 , 1.9 , 4.3),
       byrow=TRUE,ncol=3)
colnames(data) <- c("t","y","z")
data <- as.data.frame(data)

mdata <- melt(data,id.vars=c("t"))

ggplot(mdata,aes(x=t,y=value,color=variable))+geom_path()

lm1 <- lm(y~z+I(z^2),data=data)
mdata2 <- melt(cbind(data[,c("t","y")], yhat=lm1$fitted.values, residuals=lm1$residuals), id.vars=c("t"))
mdata2$plotn <- "Data and fitted values"
mdata2$plotn[mdata2$variable=="residuals"] <- "Residuals"

ggplot(mdata2,aes(x=t,y=value,color=variable))+geom_path()+
facet_wrap(~plotn,ncol=1,scales="free_y")
