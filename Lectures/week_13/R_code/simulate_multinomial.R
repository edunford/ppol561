require(tidyverse)

logit = function(x) 1/(1+exp(-x))

# wine, coke, water
# meeting tomorrow: 1 (yes), 0 (no)
# on diet: 1 (yes), 0 (no)

set.seed(123)

N = 500
diet = rbinom(N,1,pr=.25)
meeting = rbinom(N,1,pr=.25)


wine_ = exp(2 + -5*meeting + 0*diet)
# wine = rbinom(N,1,pr=logit(wine_))

coke_ = exp(1 + 0*meeting + -5*diet)
# coke = rbinom(N,1,pr=logit(coke_))

water_ = exp(0 + 0*meeting + 5*diet)
# water = rbinom(N,1,pr=logit(water_))


wa = 1/(1+wine_+coke_)
w = wine_/(1+wine_+coke_)
cc = coke_/(1+wine_+coke_)



choice = Hmisc::rMultinom(probs = cbind(wa,w,cc),m = 1)
choice = relevel(as.factor(choice),ref = "wa")

glm(as.numeric(choice=="cc")~meeting+diet,family=binomial('logit'))

D = tibble(choice,meeting,diet)

m_mod = nnet::multinom(choice~meeting+diet,data=D)
summary(m_mod)



table(D$choice,D$meeting)
table(D$choice,D$diet)
table(D$meeting,D$diet)

est <- 
  obsval::obsval(choice~meeting+diet,
                 data = D,
                 baseline.category = "wa",
                 reg.model = "mlogit",
                 effect.var = "diet",
                 effect.vals = c(0,1))
summary(est$model)


est$preds[,,2] %>% 
  as_tibble %>% 
  gather(categories,probabilities) %>% 
  ggplot(aes(x=categories,y=probabilities)) + 
  geom_boxplot()



m_w <- glm(as.numeric(choice=="w")~meeting+diet,family=binomial('logit'))
m_c <- glm(as.numeric(choice=="cc")~meeting+diet,family=binomial('logit'))




