library(MASS)
head(Boston)

#problem 1a
m_crime = median(Boston$crim)

#problem 1bi
n = dim(Boston)[1]

set.seed(12)
B = 1000
bootstrap_median = rep(0,B)
for(i in 1:B){
  index = sample(1:n,n,replace=TRUE)
  bootsample = Boston[index,]
  bootstrap_median[i] = median(bootsample$crim)
}

#problem 1bii
sqrt(sum((bootstrap_median-mean(bootstrap_median))^2)/(B-1))

#problem 1c
Y = rep(0,n)
Y[Boston$crim> m_crime] = 1

Boston$Y = Y

#problem 1d
logit = glm(Y~nox + age, data = Boston)
summary(logit)

#problem 1e
