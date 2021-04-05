set.seed(123)
N = 100
x <- rnorm(N)
tr = rbinom(N,1,.3)
y <- -1*tr + -1*x + rnorm(N,sd=2)
match = rep(1,N)


N2 = 25
x2 <- runif(N2,-5,-2)
y2 <- runif(N2,-10,-0)
tr2 = rep(0,N2)
match2 = rep(0,N2)

N3 = 25
x3 <- runif(N3,3,5)
y3 <- runif(N3,-10,-0)
tr3 = rep(0,N3)
match3 = rep(0,N3)

D = 
  tibble(y = c(y,y2,y3),
         x = c(x,x2,x3),
         tr = c(tr,tr2,tr3),
         match = c(match,match2,match3)) %>% 
  mutate(type = ifelse(tr==1,"T","C"))


# Process the models 
gen_dat <- 
  function(mod,D){
    bind_rows(
      tibble(type = 'T',
             x = D$x,
             yhat = predict(mod,D %>% mutate(tr=1))),
      tibble(type = 'C',
             x = D$x,
             yhat = predict(mod,D %>% mutate(tr=0)))
    )  
  }

m1 = lm(y~tr+x,D)
m2 = lm(y~tr+x+I(x^2),D)


  
D %>% 
  ggplot(aes(x,y,color=type)) +
  geom_text(aes(label=type),
            size=5,alpha=.8,
            show.legend = F) +
  geom_line(data=gen_dat(m1,D),aes(x,yhat,color=type),
            size=1.5,
            inherit.aes = F,show.legend = F) +
  geom_line(data=gen_dat(m2,D),aes(x,yhat,color=type),
            size=1,
            inherit.aes = F,show.legend = F) +
  theme_fivethirtyeight() +
  scale_color_manual(values=c("orangered","steelblue"))


# Matching example
D %>% 
  ggplot(aes(x,y,color=type)) +
  geom_text(aes(label=type),
            size=5,alpha=.8,
            show.legend = F) +
  gghighlight::gghighlight(match==1) +
  geom_smooth(method="lm",se=F) +
  theme_fivethirtyeight() +
  scale_color_manual(values=c("orangered","steelblue")) 
