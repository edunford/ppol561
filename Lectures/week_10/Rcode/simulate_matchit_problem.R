require(tidyverse)
require(MatchIt)


# Simulated Data --------------

# Let's manufacture the problem

set.seed(123)
N = 1000
x1 <- rnorm(N) 
x2 <- rnorm(N)

# Probability of being in the treatment group is a function of existing
# covariates
z = -1 + x1 + x2 
pr_tr <- pnorm(z)
tr <- rbinom(N,1,pr_tr)
sum(tr)/length(tr)
error <-  rnorm(N)
y = tr + x1*x2 + x1^2 + x1^3 + error
D = tibble(y,x1,x2,tr) # Gather as a data frame


# raw regresion. 
summary(lm(y~ tr,data=D))
summary(lm(y~ tr + x1 + x2,data=D))

# We do just fine if we know the "true" data generating process
summary(lm(y~ tr + x1*x2 + I(x1^2) + I(x1^3),data=D)) 



# Propensity Score matching ---------------------

m_out = matchit(tr ~ x1 + x2, 
                method="nearest",
                data=D,
                discard = "both")
summary(m_out)

plot(m_out,type="hist")

D_m = match.data(m_out)


summary(lm(y ~ tr , data=D_m))
summary(lm(y ~ tr + x1 + x2, data=D_m))

mean(match.data(m_out,"treat")$y)-mean(match.data(m_out,"control")$y)


# Full Estimation -----------

m_out2 = matchit(tr ~ x1 + x2, 
                method="full",
                data=D,
                # discard = "both",
                ratio = 1)
summary(m_out2)

D_m2 = match.data(m_out2)
plot(m_out2,type="hist")

summary(lm(y ~ tr ,weights = D_m2$weights, data=D_m2))
summary(lm(y ~ tr + x1 + x2, weights = D_m2$weights, data=D_m2))



# Coarsened Exact Matching  -----------------------

m_out3 = matchit(tr ~ x1 + x2, 
                 method="exact",
                 data=D,
                 # discard = "both",
                 ratio = 1)

# Issue when we only have continuous categories... values rarely fall in the
# same bin exactly

D %>% 
  ggplot(aes(x1,x2)) +
  geom_point()


# We can cut the observations into bins...
D_c <-
  D %>% 
  mutate(x1_cut = cut_interval(x1,25),
         x2_cut = cut_interval(x2,25)) 


D_c %>% 
  group_by(x1_cut,x2_cut) %>% 
  summarize(treat = sum(tr),
            control = sum(tr==0)) %>% 
  arrange(x1_cut,x2_cut)


# And then match exactly along those bins
m_out3 = matchit(tr ~ x1_cut + x2_cut, 
                 method="exact",
                 data=D_c,
                 # discard = "both",
                 ratio = 1)
summary(m_out3)
D_m3 = match.data(m_out3)

summary(lm(y ~ tr ,weights = D_m3$weights, data=D_m3))
summary(lm(y ~ tr + x1 + x2, weights = D_m3$weights, data=D_m3))

# Performs pretty well on these data

# Can we visualize this? Yep... and it's insightful
D_m3 %>% 
  as_tibble() %>% 
  ggplot(aes(x1,x2,color=factor(tr),size=weights)) +
  geom_point(show.legend = T) +
  geom_point(data=D,aes(x1,x2),color="grey80",inherit.aes = F,alpha=.3) +
  theme_minimal()


# We can also implement coarsened exact matching via MatchIt

m_out3b = matchit(tr ~ x1 + x2, 
                 method="cem",
                 data=D,
                 ratio = 1)
summary(m_out3b)
D_m3b = match.data(m_out3b)

summary(lm(y ~ tr ,weights = D_m3b$weights, data=D_m3b))
summary(lm(y ~ tr + x1 + x2, weights = D_m3b$weights, data=D_m3b))



# Mahalanobis Distance Matching -----------------

m_out4 = matchit(tr ~ x1 + x2, 
                 data=D,
                 method="nearest", 
                 distance="mahalanobis", 
                 discard = "both",
                 caliper=0.15,
                 replace=F)
summary(m_out4)
plot(m_out2,type="hist")

D_m4 = match.data(m_out4)

summary(lm(y ~ tr , data=D_m4))
summary(lm(y ~ tr + x1 + x2,data=D_m4))


# Inverse propensity weighting ------------------

m_out5 = matchit(tr ~ x1 + x2, 
                method="nearest",
                data=D,
                discard = "none")
w = m_out5$weights
summary(lm(y ~ tr ,weights = w, data=D))
summary(lm(y ~ tr + x1 + x2,weights = w,data=D))


# Genetic Matching -------------------

m_out6 = matchit(tr ~ x1 + x2, 
                 method="cem",
                 data=D)



# Complicating Matters -----------------------------


# What happens when the propensity is non-linear?

# Let's manufacture the problem

set.seed(123)
N = 1000
x1 <- rnorm(N) 
x2 <- rnorm(N)

# Probability of being in the treatment group is a function of existing
# covariates
z = -1 + x1*x2 + (x1^2)*(x2^2)
pr_tr <- pnorm(z)
tr <- rbinom(N,1,pr_tr)
sum(tr)/length(tr)
error <-  rnorm(N)
y = tr + x1*x2 + x1^2 + x1^3 + error
D = tibble(y,x1,x2,tr) # Gather as a data frame


# raw regresion. 
summary(lm(y~ tr,data=D))
summary(lm(y~ tr + x1 + x2,data=D))

# We do just fine if we know the "true" data generating process
summary(lm(y~ tr + x1*x2 + I(x1^2) + I(x1^3),data=D)) 


dd <- 
D %>% 
  mutate(x1_cut = cut_interval(x1,25),
         x2_cut = cut_interval(x2,25)) 
  
dd_m <-
  matchit(tr ~ x1_cut + x2_cut, 
          data=dd,
          method="exact",ratio=1)  %>% 
  match.data  
  
lm(y~ tr,weights = dd_m$weights,data=dd_m) %>%summary(.)
lm(y~ tr + x1 + x2,weights = dd_m$weights,data=dd_m) %>% summary(.)















