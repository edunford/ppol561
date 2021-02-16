#' Week 4 Lecture Code


# Simulating a Single Variable --------------------------------------------

      N = 1000
      x <- rnorm(N)
      hist(x,col="grey30",border = "white")

# Bivariate Simulation ----------------------------------------------------
    
      # Basic bivariate model
      N  = 100
      x <- rnorm(N)
      y = x 
      plot(x,y,pch=16,col="grey30")
      abline(a=0,b=1,lty=2)
      
      # Crazy functional form
      N  = 1000
      x <- rnorm(N)
      y = 1 + sin(x)
      plot(x,y,pch=16,col="grey30",ymin=c(-3,3))
      
      
      # With error
      N  = 100
      x <- rnorm(N)
      error <- rnorm(N)
      y = x + error
      plot(x,y,pch=16,col="grey30",ymin=c(-3,3))
      abline(a=0,b=1,lty=2,col="darkred",lwd=2)
      
      # Run model
      m = lm(y~x)
      
      
      # crazy functional form
      N  = 1000
      x <- rnorm(N)
      error <- rnorm(N)
      y = sin(x) + error
      plot(x,y,pch=16,col="grey30",ymin=c(-3,3))
      abline(a=0,b=1,lty=2,col="darkred",lwd=2)
      
      m = lm(y~ x)
      summary(m)
      
      m = lm(y~ sin(x))
      summary(m)
      


# Monte Carlo Simulation --------------------------------------------------

    n_sims = 1000
    true_beta = 1
    beta_container = rep(NA,n_sims)
    for ( i in 1:n_sims){
      N  = 100
      x <- rnorm(N)
      error <- rnorm(N)
      y = 1 + true_beta*x + error
      m = lm(y~x)
      beta_container[i] = coef(m)[2]
    }
    hist(beta_container,col = scales::alpha("steelblue",alpha=.5),border = "white",
         xlim = c(-3,3),main = "Simulation of Beta 1")
    abline(v=0,lty=2,col="darkred")
    abline(v=true_beta,lty=2,col="steelblue")


# Precision of Estimates --------------------------------------------------

    n_sims = 1000
    var_x = 1
    var_e = 1
    true_beta = 1
    beta_container = rep(NA,n_sims)
    for ( i in 1:n_sims){
      N  = 100
      x <- rnorm(N,mean = 0 ,sd = var_x)
      error <- rnorm(N,mean = 0,sd = var_e)
      y = 1 + true_beta*x + error
      m = lm(y~x)
      beta_container[i] = coef(m)[2]
    }
    hist(beta_container,col = scales::alpha("steelblue",alpha=.5),border = "white",
         xlim = c(-3,3),main = "Simulation of Beta 1")
    abline(v=0,lty=2,col="darkred")
    abline(v=true_beta,lty=2,col="steelblue")

    
    # Multicollinearity
    n_sims = 1000
    true_beta = .5
    beta_container = rep(NA,n_sims)
    for ( i in 1:n_sims){
      N  = 100
      corr = .1
      sigma =  matrix(c(1,corr,corr,1),ncol=2,nrow=2)
      X <- MASS::mvrnorm(N,mu = c(0,0),Sigma = sigma)
      x1 <- X[,1]
      x2 <- X[,2]
      error <- rnorm(N)
      y = 1 + true_beta*x1 + -.25*x2 + error
      m = lm(y~x1 + x2)
      beta_container[i] = coef(m)[2]
    }
    hist(beta_container,col = scales::alpha("steelblue",alpha=.5),border = "white",
         xlim = c(-3,3),main = "Simulation of Beta 1")
    abline(v=0,lty=2,col="darkred")
    abline(v=true_beta,lty=2,col="steelblue")
    
    
    
# Omitted Variable Bias ---------------------------------------------------
    
    # Simulate omitted variable bias similar to the dag
    n_sims = 1000
    true_beta = 1
    confound_beta = 1
    beta_container = rep(NA,n_sims)
    for ( i in 1:n_sims){
      N  = 100
      z <- rnorm(N,mean = 0 ,sd = 1)
      x <- -z + rnorm(N,mean = 0 ,sd = 1)
      error <- rnorm(N,mean = 0,sd = var_e)
      y = 1 + true_beta*x + -z + error
      m = lm(y~x)
      beta_container[i] = coef(m)[2]
    }
    hist(beta_container,col = scales::alpha("steelblue",alpha=.5),border = "white",
         xlim = c(-3,3),main = "Simulation of Beta 1")
    abline(v=0,lty=2,col="darkred")
    abline(v=true_beta,lty=2,col="steelblue")
    


# Collider Bias  ----------------------------------------------------------
      
    # (1) Simulate collider bias
    N <- 1000
    u <- rnorm(N)
    x <- u + rnorm(N)
    y <- x + u + rnorm(N)
    m <- lm(y ~ x)
    summary(m)
    m2 <- lm(y ~ x + u)
    summary(m2)
    


    # (2) Controlling on a colliders descendants
    set.seed(2222)
    N <- 1000
    a <- rnorm(N)
    b <- rnorm(N)
    m <- a + b + rnorm(N)
    z <- m + rnorm(N)
    x <- a + rnorm(N)
    y <- b + rnorm(N)
    
    lm(y ~ x) %>% broom::tidy()
    lm(y ~ x + z) %>% broom::tidy()


    

# Backdoor Adjustment -----------------------------------------------------

    # Simulate the confounded model 
    v <- rnorm(N)
    h <- rnorm(N)
    z <- h + rnorm(N)
    g <- v + rnorm(N)
    q <- v + h + rnorm(N)
    x <- g + q + rnorm(N)
    y <- -2*x + q + rnorm(N)
    
    lm(y ~ x) %>% broom::tidy() # Simple model
    
    
    # Show how controlling on the right variables returns the correct results
    lm(y ~ x + g + q) %>% broom::tidy() 
    lm(y ~ x + v + q) %>% broom::tidy() 
    lm(y ~ x + h + q) %>% broom::tidy() 
    lm(y ~ x + z + q) %>% broom::tidy() 
    
    # Hit on that we cannot interpret the controls
  
    

# Breakout ----------------------------------------------------------------

    # Simulation
    N = 500
    z <- rnorm(N)
    g <- rnorm(N)
    s <- g + z + rnorm(N)
    x <- g + s + rnorm(N)
    y <- x + s + z + rnorm(N)
    
    # Two correction options
    lm(y ~ x + s + g)
    lm(y ~ x + s + z)
    
    

# Frontdoor Adjustment ----------------------------------------------------

    # Simulate the frontdoor condition
    set.seed(1234)
    N = 1000
    u <- rnorm(N)
    x <- u + rnorm(N)
    z <- .2*x + .6*rnorm(N)
    y <- .5*z + .1*u + .4*rnorm(N)
    
    # True Indirect Effect: .2 * .5 = .1    
    
    # Computing the indirect effect. 
    lm(y ~ x) %>% broom::tidy()
    x_on_z = lm(z ~ x)
    z_on_y = lm(y ~ z )
    x_on_z$coefficients[2]*z_on_y$coefficients[2]
    
    

# Measurement Error -------------------------------------------------------

    set.seed(1111)
    N = 1000
    x <- rnorm(N)
    noise <- rnorm(N,mean=0,sd=1)
    x_obs <-  x + noise
    y <- x + rnorm(N)
    m <- lm(y~ x_obs)
    summary(m)
    
    # What happens as the measurement error increases? 
    # Attenuation
    
    # Solutions: (1) get a better measure, (2) over imputation
    

# Missingness -------------------------------------------------------

    # MCAR
    set.seed(1234)
    N = 1000
    x <- rnorm(N)
    s <- rnorm(N)
    y <- x + rnorm(N)
    D = data.frame(y,x,s)
    D[D$s > 1,c('y','x')] = NA
    m <- lm(y ~ x,data=D)
    summary(m)
    
    
    # MAR
    set.seed(1234)
    N = 1000
    s <- rnorm(N)
    x <- s + rnorm(N)
    y <- x + s + rnorm(N)
    D = data.frame(y,x,s)
    D[D$s > 1,c('y','x')] = NA
    
    # MNAR
    m <- lm(y ~ x,data=D)
    summary(m)
    
    # MAR
    m <- lm(y ~ x + s,data=D)
    summary(m)
    
    
    # Imputation as a solution?
    amelia_fit <- Amelia::amelia(D, m=5, parallel = "multicore")
    m <- lm(y ~ x,data=amelia_fit$imputations[1])
    summary(m)
    

# Other biases  -----------------------------------------------------------

    
    # Other forms of bias that we'll delve into this semester 
    
    # selection bias
    # aggregation bias
    
    

# Heteroskedasticity  -----------------------------------------------------

    
    
    N = 1000
    x <- rnorm(N)
    error <- rnorm(N,sd = abs(x))
    y = x + error
    plot(x,y)
    plot(x,error)
    
    
    
    # MC Sim
    n_sims = 1000
    beta_container = rep(NA,n_sims)
    for ( i in 1:n_sims){
      N = 150
      x <- rnorm(N)
      error <- rnorm(N,sd = abs(x))
      y = x + error
      m = lm(y~x)
      beta_container[i] = coef(m)[2]
    }
    hist(beta_container,col = scales::alpha("steelblue",alpha=.5),
         border = "white",
         xlim = c(-3,3),main = "Simulation of Beta 1")
    abline(v=0,lty=2,col="darkred")
    abline(v=true_beta,lty=2,col="steelblue")
    