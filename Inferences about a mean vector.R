#4
s <-  matrix(c(2.879368421,	10.01,	-1.809052632,
               10.01,	199.7884211,	-5.64,
               -1.809052632,	-5.64,	3.627657895), nrow=3)
e <- eigen(s)

for(i in 1:3){
a[i] <- sqrt * sqrt(e$values[i])
}

b <- matrix(rep(0,9), nrow=3)
for(i in 1:3){
	b[,i] <- a[i]*e$vector[,i]
}
b


#18 b
data <- matrix(scan(file = "~/Desktop/table52.txt"), byrow=T, ncol=3)
colnames(data) <- c("SSH", "Verbal", "Science")
library(GGally)
ggpairs(as.data.frame(data))

qqnorm(data[,1], main="SSH")
qqnorm(data[,2], main="Verbal")
qqnorm(data[,3], main="Science")


s <-  matrix(c(5808.059342,	597.8352045,	222.0296712,
               597.8352045,	126.0537289,	23.38853248,
               222.0296712,	23.38853248,	23.11173483), nrow=3)
e <- eigen(s)
sqrt <- sqrt(3*86/(87*84)*2.713)
a <- rep(NA,3)
for(i in 1:3){
  a[i] <- 2 * sqrt* sqrt(e$values[i])
}
a




#20
n<- 46
X <- matrix(scan(file = "~/Desktop/table512.txt"), byrow=T, ncol=2)
colnames(x) <- c("Tail Length", "Wing length")
Xbar <- colMeans(X)
S <- cov(X)
Sinv <- solve(S)
d <- diag(t(t(X)-Xbar)%*%Sinv%*%(t(X)-Xbar))
d
library(ellipse)
p <- 2
elps <- t(t(ellipse(S, level=0.950, npoints=1000))+Xbar)
plot(X[,1],X[,2],type="n")
index <- d < qchisq(0.95,df=2)
text(X[,1][index],X[,2][index],(1:n)[index],col="blue")
text(X[,1][!index],X[,2][!index],(1:n)[!index],col="red")
lines(elps,col="blue")
points(190, 275)

t <- (p*(n-1))/(n*(n-p)) * qf(0.95, p, n-p)
plot(ellipse::ellipse(x=S, t= sqrt(t), centre = Xbar, npoints=200), type="l")








#22
data2 <- matrix(scan(file = "~/Desktop/table513.txt"), byrow=T, ncol=3)
colnames(data2) <- c("Fuel", "Repair", "Capital")
library(GGally)
ggpairs(as.data.frame(data2))
par(mfrow = c(3, 1)) 
qqnorm(data2[,1], main="Fuel")
qqnorm(data2[,2], main="Repair")
qqnorm(data2[,3], main="Capital")

##
data3 <- data2[c(-9, -21),]
colnames(data) <- c("Fuel", "Repair", "Capital")
library(GGally)
ggpairs(as.data.frame(data3))
par(mfrow = c(3, 1)) 
qqnorm(data3[,1], main="Fuel")
qqnorm(data3[,2], main="Repair")
qqnorm(data3[,3], main="Capital")


