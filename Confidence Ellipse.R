#4.26
n <- 10
x1 <- c(1,2,3,3,4,5,6,8,9,11)
x2 <- c(18.95, 19.00, 17.95, 15.54, 14.00, 12.95, 8.94, 7.49, 6.00, 3.99)
X <- cbind(x1,x2)
Xbar <- colMeans(X)
S <- cov(X)
Sinv <- solve(S)

# a
d <- diag(t(t(X)-Xbar)%*%Sinv%*%(t(X)-Xbar))
d

#b
library(ellipse)
p <- 2
elps <- t(t(ellipse(S, level=0.50, npoints=1000))+Xbar)
plot(X[,1],X[,2],type="n")
index <- d < qchisq(0.5,df=p)
text(X[,1][index],X[,2][index],(1:n)[index],col="blue")
text(X[,1][!index],X[,2][!index],(1:n)[!index],col="red")
lines(elps,col="blue")



#c
names(d) <- 1:10
sort(d)
qqplot(qchisq(ppoints(500),df=p), d, main="", xlab="Theoretical Quantiles", ylab="Sample Quantiles")
qqline(d,distribution=function(x){qchisq(x,df=p)})
       
       