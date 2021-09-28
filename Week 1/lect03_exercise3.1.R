##
## Re-doing parts of exercise 3.1 using R
##
## 2014-09-19 by Lasse Engbo Christiansen

## First defining the data
tab32 <- data.frame(t = 1:7, y = c(1, 0.5, 2, 2, 3.5, 3, 4), 
           x = c(4, 4, 3.5, 4, 2, 2.5 ,1.5 ))

## Making a simple plot
plot(y ~ x, tab32)
plot(y ~ x, tab32, pch=19, col=4)

## Question 1 - using 'lm'
lm1<-lm(y ~ x, data=tab32)
summary(lm1)

## Question 2
?predict.lm
predict(lm1,newdata=list(x=0.5))

predict(lm1,newdata=list(x=0.5),interval="pred",level=0.95,se.fit = TRUE)

## Question 3
## What if we only observed yt
plot(tab32$y, pch=19)

## This can be solved in many ways. Here we'll use a both a general linear 
## model and a linear trend model

lm3 <- lm(y ~ t, tab32)
summary(lm3)
predict(lm3, interval="pred", newdata = data.frame(t=8),
        level = 0.9)

## linear trend model:
x7<-cbind(1, -6:0)
x7
F7 <- t(x7) %*% x7
h7 <- t(x7) %*% tab32$y
theta7 <- solve(F7) %*% h7
sigma7 <- sqrt( sum((tab32$y - x7%*%theta7)^2)/(7-2) )

plot(tab32$y, xlim=c(0,8), ylim=c(0,5.9), pch=19)
lines( x7%*%theta7, col=2)

## A formal way to take the last step:
f <- function(j) return( rbind(1,j) )
hatY8 <- t(f(1)) %*% theta7
points(8, hatY8, col=2, pch=19)

Ve8 <- sigma7^2 * (1 + t(f(1)) %*% solve(F7) %*% f(1))
sqrt(Ve8)
(interval <- hatY8 + c(-1,1) * qt(0.95,5)* sqrt(Ve8) )

lines(c(8,8),interval,col=2)

