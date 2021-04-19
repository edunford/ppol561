# Dropped slide material ....


# This slidee shows a symattric distribution w/ a cut off...
---
Let's assume a normal and symmetric distribution.

```{r,echo=F,fig.align="center",fig.width=10,fig.height=6}
tibble(x=seq(-3,3,.1),pr=dnorm(seq(-3,3,.1))) %>% 
  mutate(pr2 = ifelse(x>=0,pr,0),
         x2 = ifelse(x>=0,x,0)) %>% 
ggplot(aes(x,pr)) +
  geom_area(alpha=.5,fill="grey60") +
  geom_area(aes(y=pr2,x2),alpha=.5,fill="steelblue") +
  geom_vline(xintercept = 0,color="darkred",size=1.5) +
  geom_label(x=0,y=.45,label=latex2exp::TeX("$\\tau$"),size=10) +
  geom_label(x=2,y=.4,label="y = 1",size=7,color="steelblue") +
  geom_label(x=-2,y=.4,label="y = 0",size=7,color="grey20") +
  ylim(0,.45) +
  labs(x = latex2exp::TeX("$\\beta_0 + \\beta_1 x_i$"),
       y="Density") +
  theme_bw() +
  theme(axis.title = tt,
        axis.text = tt ) 
```

---





---

## Simulation 

```{r,highlight=T,cache=F}
set.seed(123)
N = 100
x <- rnorm(N)            # random variable
y_star <- -1 + .5*x       # latent variable as a linear combination

# Assume the error
error <-  rnorm(N) #<< 

# 1 if y_star > error, 0 otherwise
y = as.numeric(error < y_star) #<< 


# Gather as dataset
dat1 <- tibble(x,y_star,error,y)
head(dat1)
```


---

```{r,fig.align="center",fig.width=10,fig.height=4,cache=F}
pairs(dat1,col="steelblue")
```

--

```{r,cache=F}
mod = glm(y~x,data=dat1,family=binomial(link = "probit"))
broom::tidy(mod)
```
