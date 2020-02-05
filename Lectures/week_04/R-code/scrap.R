


plot(x,resid(mod))


t = rnorm(100)
tt = 1 + 2*t + rnorm(100)
plot(t,resid(lm(tt~t)))


hist(dnorm(rnorm(1000)))



(1 - pnorm(1))



set.seed(123)
N = 1000
x <- rnorm(N)            # random variable
y_star <- -1 + 2*x       # latent variable as a linear combination
pr <- pnorm(y_star)      # convert to a probability space

logit = function(x) 1/(1+exp(-1*x))

# pr <- logit(y_star)

# error <-  rnorm(N)
# error <-  rlogis(N)


# y = as.numeric(error < -1 + 2*x)


# Drop the probability of realizing a 0 or 1 in 
# a binomial distribution (bernoulli b/c only 0/1)
y <- rbinom(N, size=1, prob = pr) #<<

table(y1,y2)

# Gather as dataset
dat <- tibble(x,y_star,pr,y)
head(dat)

mod = glm(y~x,family = binomial('logit'),data=dat)
summary(mod)
plot(x,predict(mod,type="response"))
points(x,pr,col="red")







mod$coefficients

