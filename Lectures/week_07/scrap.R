

library(tidyverse)
library(dagitty)
library(ggdag)

my_theme <- ggraph::theme_graph() + 
  theme(strip.text = element_text(size=18),
        legend.position = "top")
theme_set(my_theme)
theme_set(theme_gray())

# Simple DAG
coords <- data.frame(matrix(c("x",0,0,
                              "y",2,0), nrow=2, ncol=3, byrow=T))
colnames(coords) <- c("name","x","y")

# Initialize the DAG.
dag <- dagify(y ~ x,
              exposure = "x",
              outcome = "y",
              coords=coords)

tidy_dagitty(dag)

# Plot the DAG
ggdag(dag,text_size = 8) + theme_dag()

tidy_dagitty(dag) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  ggdag::geom_dag_edges(edge_linetype=2) +
  geom_dag_point() +
  geom_dag_text() +
  theme_dag()



# Unobservable Variable -----------------------

coords <- 
  c("x",0,0,
    "z",0,1,
    "u",1.5,1,
    "y",2,0) %>% 
  {data.frame(matrix(., nrow=length(.), ncol=3, byrow=T))} %>% 
  {colnames(.) <- c("name","x","y");.}


# Initialize the DAG.
dag <- dagify(x ~ z,
              y ~ z,
              z ~ u,
              y ~ x + u,
              exposure = "x",
              outcome = "y",
              coords=coords)


dag %>% 
  tidy_dagitty() %>% 
  mutate(observed = ifelse(name=="u",2,1)) %>% 
  mutate(observed_text = ifelse(name=="u","black","white")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(aes(edge_linetype=observed)) +
  geom_dag_point(aes(color=factor(observed)),show.legend = F) +
  scale_color_manual(values=c('1'="black",'2'="grey70")) +
  geom_dag_text() +
  theme_dag()

# Unobservable Variable -----------------------

coords <- 
  c("x",0,0,
    "z",0,1,
    "y",2,0) %>% 
  {data.frame(matrix(., nrow=length(.), ncol=3, byrow=T))} %>% 
  {colnames(.) <- c("name","x","y");.}


# Initialize the DAG.
dag <- dagify(x ~ z,
              y ~ z,
              z ~ y,
              y ~ x,
              exposure = "x",
              outcome = "y",
              coords=coords)

dag %>% ggdag()
dag %>% 
  tidy_dagitty() %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(aes(edge_linetype=observed)) +
  geom_dag_point() +
  geom_dag_text() +
  theme_dag()


# Reverse Causality -------------------------------------------------------

coords <- 
  c("y_t1",0,0,
    "x_t1",0,1,
    "x_t2",1,1,
    "y_t2",1,0,
    "x_t3",2,1,
    "y_t3",2,0) %>% 
  {data.frame(matrix(., nrow=length(.), ncol=3, byrow=T))} %>% 
  {colnames(.) <- c("name","x","y");.}

dag <- dagify(y_t1 ~ x_t1,
              x_t2 ~ y_t1,
              y_t2 ~ x_t2,
              x_t3 ~ y_t2,
              y_t3 ~ x_t3,
              exposure = "x_t2",
              outcome = "y_t2",
              coords=coords)
dag %>% 
  tidy_dagitty() %>% 
  arrange(name) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point() +
  geom_dag_edges() +
  geom_dag_text(parse = TRUE, label = c(expression(X[t-1]), 
                                        expression(X[t]),
                                        expression(X[t+1]),
                                        expression(Y[t-1]),
                                        expression(Y[t]),
                                        expression(Y[t+1]))) +
  theme_dag()
  


# More Complex DAG --------------------------------------------------------

# Layout
coords <- data.frame(matrix(c("X",0,0,
                              "Y",2,0,
                              "Q",1,1,
                              "G",0,1,
                              "V",0,2,
                              "H",2,2,
                              "Z",2,1), nrow=8, ncol=3, byrow=T))
colnames(coords) <- c("name","x","y")


# Initialize the DAG.
dag <- dagify(Y ~ Z, 
              Y ~ Q,
              Z ~ H,
              G ~ V,
              Q ~ H, Q ~ V,
              X ~ G, X ~ Q,
              Y ~ X,
              exposure = "X",
              outcome = "Y",
              coords=coords)

# Plot the DAG
ggdag(dag)


# Paths of x to y
ggdag_paths(dag,shadow = T)

ggdag_adjustment_set(dag) +
  labs(color="",shape="") +
  scale_color_manual(values=c("steelblue","black"))

ggdag_adjustment_set(d)

coords <- data.frame(matrix(c("x",0,0,
                              "z",1,0,
                              "y",2,0), nrow=3, ncol=3, byrow=T))
colnames(coords) <- c("name","x","y")

# Initialize the DAG.
dag <- dagify(z ~ x,
              y ~ z,
              exposure = "x",
              outcome = "y",
              coords=coords)
ggdag(dag,text_size = 8)



# -------------------------------------------------------------------------


# anscestor concept
dag <- dagify(X ~ Z + U,
              Z ~ P,
              Y ~ Z,
              Y ~ X,
              exposure = "X",
              outcome = "Y") 
d = dag %>% tidy_dagitty(seed=123) 

ggdag(d)


ggdag_parents(d,"Y") +
  scale_color_manual(values=c("steelblue","black")) +
  labs(color="") +
  theme(legend.position = "top")


ggdag_ancestors(d,"Y") +
  scale_color_manual(values=c("steelblue","black")) +
  labs(color="") +
  theme(legend.position = "top")






ggdag_paths(d,from="P",to="Y",shadow = T) +
  theme(legend.position = "none",
        strip.text = element_blank()) 

dag %>%  
  ggdag_adjust("z")
ggdag_butterfly_bias() 


m_bias() %>% ggdag()




m_bias() %>% 
  tidy_dagitty() %>% 
  as_tibble(.) %>% 
  mutate_if(is.factor,as.character) %>% 
  bind_rows(.,tribble(
    ~name,~x,~y,~direction,~to,~xend,~yend,~circular,
    'z',1,1,'->',"m",1,.5,F
  )) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point() +
  geom_dag_edges() +
  geom_dag_text() +
  theme_dag()



# Layout
coords <- data.frame(matrix(c("a",0,1,
                              "b",2,1,
                              "m",1,.5,
                              "x",0,0,
                              "y",2,0,
                              "z",1,0), nrow=6, ncol=3, byrow=T))
colnames(coords) <- c("name","x","y")


# Initialize the DAG.
dag <- dagify(x~ a, 
              m ~ a + b,
              z ~ m,
              y ~ b,
              exposure = "x",
              outcome = "y",
              coords=coords)

dag %>% ggdag_adjust('z')



# Dscendant collider sim --------------------------------------------------

N <- 1000
a <- rnorm(N)
b <- rnorm(N)
m <- a + b + rnorm(N)
z <- m + rnorm(N)
x <- a + rnorm(N)
y <- b + rnorm(N)

lm(y ~ x) %>% broom::tidy()

lm(y ~ x + z) %>% broom::tidy()



# Backdoor Criteria -------------------------------------------------------


v <- rnorm(N)
h <- rnorm(N)
z <- h + rnorm(N)
g <- v + rnorm(N)
q <- v + h + rnorm(N)
x <- g + q + rnorm(N)
y <- -2*x + q + rnorm(N)


lm(y ~ x) %>% broom::tidy()

lm(y ~ x + g + q) %>% broom::tidy()


dag %>% ggdag_adjust("J")


set.seed(1234)
N = 1000
u <- rnorm(N)
x <- u + rnorm(N)
z <- .2*x + .6*rnorm(N)
y <- .5*z + .1*u + .4*rnorm(N)

.2*.5


x_on_z = lm(z ~ x)
z_on_y = lm(y ~ z )

x_on_z$coefficients[2]*z_on_y$coefficients[2]




set.seed(1234)
N = 1000
u <- rnorm(N)
x <- u + rnorm(N)
z <- .2*x + .3*u + .3*rnorm(N)
y <- .5*z + .1*u + .4*rnorm(N)

.2*.5


x_on_z = lm(z ~ x)
z_on_y = lm(y ~ z )

x_on_z$coefficients[2]*z_on_y$coefficients[2]



# -------------------------------------------------------------------------

wage_coords <- 
  c("background",0,0,
    "p_educ",0,1,
    "college",1,0,
    "family_income",1,1,
    "wage",2,.5) %>% 
  {data.frame(matrix(., nrow=length(.), ncol=3, byrow=T))} %>% 
  {colnames(.) <- c("name","x","y");.}

wage_dag <- dagify(family_income ~ p_educ, 
                   college ~ p_educ + family_income + background,
                   p_educ ~ background,
                   wage ~ college + family_income,
                   exposure = "college",
                   outcome = "wage",
                   coords = wage_coords)

wage_dag %>% 
  tidy_dagitty(seed = 333,circular = FALSE) %>%
  mutate(observed = ifelse(name=="background",2,1)) %>% 
  mutate(observed_text = ifelse(name=="background","black","grey70")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(aes(edge_linetype=observed),curvature = 1) +
  geom_dag_point(aes(color=factor(observed)),show.legend = F,size=3) +
  scale_color_manual(values=c('1'="black",'2'="grey70")) +
  geom_dag_text(color="black",size=7,nudge_x = 0,nudge_y = .2) +
  theme_dag()

  


# Example Two-------------------------------------------------------------------------

# conflict_coords <- 
#   c("agricultural_resources",10,5,
#     "precipitation",5,5,
#     "income",10,10,
#     "pop",10,5
#     # "conflict",7,3,
#     # "development",3,3,
#     # "nature",3,0,
#     # "past_conflict",0,0
#     ) %>% 
#   {data.frame(matrix(., nrow=length(.), ncol=3, byrow=T))} %>% 
#   {colnames(.) <- c("name","x","y");.}

conflict_dag <- dagify(agricultural_resources ~ precipitation + pop + development, 
                       conflict ~ agricultural_resources + income,
                       income ~ past_conflict + agricultural_resources,
                       precipitation ~ climate,
                       pop ~ climate + past_conflict,
                       development ~ past_conflict,
                   exposure = "precipitation",
                   outcome = "conflict")



conflict_dag %>% 
  tidy_dagitty(seed = 334,layout = "nicely") %>%
  mutate(observed = ifelse(name=="climate",2,1)) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(aes(edge_linetype=observed),curvature = 1) +
  geom_dag_point(aes(color=factor(observed)),show.legend = F,size=3) +
  scale_color_manual(values=c('1'="black",'2'="grey70")) +
  geom_dag_text(color="black",size=7,nudge_x = 0,nudge_y = .2) +
  theme_dag()



conflict_coords = conflict_dag %>% 
  tidy_dagitty(seed = 334,layout = "nicely") %>% 
  select(name,x,y) %>% 
  as.data.frame() %>% 
  unique()


conflict_dag2 <- dagify(agricultural_resources ~ precipitation, 
                       conflict ~ agricultural_resources + income,
                       income ~ past_conflict + agricultural_resources,
                       precipitation ~ climate,
                       pop ~ climate + past_conflict,
                       development ~ past_conflict,
                       exposure = "precipitation",
                       outcome = "conflict",
                       coords=conflict_coords)
conflict_dag2 %>% 
  tidy_dagitty() %>%
  mutate(observed = ifelse(name=="climate",2,1)) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(aes(edge_linetype=observed),curvature = 1) +
  geom_dag_point(aes(color=factor(observed)),show.legend = F,size=3) +
  scale_color_manual(values=c('1'="black",'2'="grey70")) +
  geom_dag_text(color="black",size=7,nudge_x = 0,nudge_y = .2) +
  theme_dag()




# PC Alg ------------------------------------------------------------------

dag <- dagify(X ~ Z + U,
              Z ~ P,
              Y ~ Z + U,
              exposure = "X",
              outcome = "Y") 
tidy_dagitty(dag,seed=123) %>% ggdag() +theme_dag()

coord = dag %>% tidy_dagitty(seed=123) %>% select(name,x,y) %>% distinct() %>% as.data.frame

# One
dagify(X ~~ Z + U + P +Y ,
           Z ~~ U + X + Y+P,
           Y ~~ X + U + Z+P,
           U ~~ Y + X + Z+P,
           P ~~ Y + X + Z+ U,
           exposure = "X",
           outcome = "Y",
           coords=coord) %>% 
  tidy_dagitty() %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(arrow_bidirected = arrow(angle = 0),curvature = 0) +
  geom_dag_point(show.legend = F) + 
  geom_dag_text(size=7)



# 2
dagify(X ~~ Z + U ,
           Z ~~ X + Y,
           Y ~~ U + Z,
           U ~~ Y + X,
           Z ~~ P,
           exposure = "X",
           outcome = "Y",
           coords=coord) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(arrow_bidirected = arrow(angle = 0),curvature = 0) +
  geom_dag_point(show.legend = F) + 
  geom_dag_text(size=7)


# 3
dagify(X ~~ Z + U ,
            Z ~~ X + Y,
            Y ~~ U + Z,
            U ~~ Y + X,
            Z ~~ P,
            exposure = "X",
            outcome = "Y",
            coords=coord) %>% 
tidy_dagitty() %>% 
  mutate(color = ifelse(name %in% c("U","X","Z"),"on","off")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(arrow_bidirected = arrow(angle = 0),curvature = 0) +
  geom_dag_point(aes(color=color),show.legend = F) + 
  geom_dag_text(size=7) +
  scale_color_manual(values=c("black","steelblue"))


# 4
dagify(Z ~~ X + Y,
       Y ~~ U + Z,
       U ~~ Y ,
       X ~ U + Z,
       Z ~~ P,
       exposure = "X",
       outcome = "Y",
       coords=coord) %>% 
  tidy_dagitty() %>% 
  mutate(color = ifelse(name %in% c("U","Y","Z"),"on","off")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(arrow_bidirected = arrow(angle = 0),curvature = 0) +
  geom_dag_point(aes(color=color),show.legend = F) + 
  geom_dag_text(size=7) +
  scale_color_manual(values=c("black","steelblue"))



# 5
dagify(Z ~~ X + Y,
       Y ~~ U + Z,
       Y ~ U + Z,
       X ~ U + Z,
       Z ~~ P,
       exposure = "X",
       outcome = "Y",
       coords=coord) %>% 
  tidy_dagitty() %>% 
  mutate(color = ifelse(name %in% c("P","X","Z"),"on","off")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(arrow_bidirected = arrow(angle = 0),curvature = 0) +
  geom_dag_point(aes(color=color),show.legend = F) + 
  geom_dag_text(size=7) +
  scale_color_manual(values=c("black","steelblue"))

# 6
dagify(Z ~~ X + Y,
       Y ~~ U + Z,
       Y ~ U + Z,
       X ~ U + Z,
       Z ~~ P,
       exposure = "X",
       outcome = "Y",
       coords=coord) %>% 
  tidy_dagitty() %>% 
  mutate(color = ifelse(name %in% c("P","Y","Z"),"on","off")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(arrow_bidirected = arrow(angle = 0),curvature = 0) +
  geom_dag_point(aes(color=color),show.legend = F) + 
  geom_dag_text(size=7) +
  scale_color_manual(values=c("black","steelblue"))

# 7
dagify(Y ~ U + Z,
       X ~ U + Z,
       Z ~~ P,
       exposure = "X",
       outcome = "Y",
       coords=coord) %>% 
  tidy_dagitty() %>% 
  # mutate(color = ifelse(name %in% c("P","Y","Z"),"on","off")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(curvature = 0) +
  geom_dag_point(show.legend = F) + 
  geom_dag_text(size=7) +
  scale_color_manual(values=c("black","steelblue"))


# PCALG -------------------------------------------------------------------

set.seed(123)
N = 1000
p = rnorm(N)
z = p + rnorm(N)
u = rnorm(N)
x = z + u + rnorm(N)
y = z + u + rnorm(N)
d = data.frame(y,x,z,u,p)

ci_stat <- list(C = cor(d), n = nrow(d))

skeleton <- skeleton(ci_stat, indepTest = gaussCItest, 
                     labels = colnames(d), alpha = 0.05)
Rgraphviz::plot(skeleton)

pc_gauss <- pc(ci_stat, 
               indepTest = gaussCItest, 
               labels = colnames(d), alpha = 0.05)
dev.off()
Rgraphviz::plot(pc_gauss)


cand <-
  tibble(v1 = colnames(d)) %>%
  crossing(tibble(v2 = colnames(d))) %>%
  filter(v1!=v2)

ci_corr1 = function(v1,v2){
  t_stat = broom::tidy(lm(v1 ~ v2))$statistic[2]
  ifelse(abs(t_stat) >= 1.96,1,0)
}
ci_corr2 = function(v1,v2,v3){
  t_stat = broom::tidy(lm(v1 ~ v2 + v3))$statistic[2]
  ifelse(abs(t_stat) >= 1.96,1,0)
}

cand$not_ci = NA
for( i in 1:nrow(cand)){
  v1 <- d[,cand$v1[i]]
  v2 <- d[,cand$v2[i]]
  cand$not_ci[i] <- ci_corr1(v1,v2)
}

cand2 <- cand %>% filter(not_ci==1) %>% arrange(v1)


# cand<- 
#   cand %>%
#   mutate(data = list(d)) %>% 
#   mutate(data = pmap(list(data,v1,v2), function(.d,.x,.y) select(.d,.x,.y) )) %>% 
#   mutate(ci = map_dbl(data, ~ci_corr(.x))) %>% 
#   filter(ci==1) %>% 
#   arrange(v1) 



set.seed(123)
cand2 %>% 
  tidygraph::as_tbl_graph() %>%
  igraph::get.adjacency()
  ggraph::ggraph() +
  ggraph::geom_edge_link() + 
  ggraph::geom_node_point(size=15)  +
  ggraph::geom_node_text(aes(label=name),size=7,color="white")
  
cand2


node= "p"

middle_nodes <- unique(cand2$v1)
for ( node in middle_nodes){
  
}
tmp <- cand2[cand2$v1 == node,]
options <- tmp$v2
for(opt1 in options){
  for (opt2 in options){
    if(opt1 != opt2){
      v1 <- d[,opt1]
      v2 <- d[,opt2]
      v3 <- d[,node]
      ci1 <- ci_corr1(v1,v2)
      ci2 <- ci_corr2(v1,v2,v3)
      if(ci1 == ci2){
        
      }
    }
  }
}


cand2 %>% 
  group_by(v1) %>% 
  nest()



cor.test(cand$data[[2]][,1],cand$data[[2]][,2])


apply(candidates,1,comp_corr)




# -------------------------------------------------------------------------

conflict_dag <- dagify(a ~ pr + po + d, 
                       c ~ a + i,
                       i ~ pc + a + d,
                       pr ~ cl,
                       po ~ cl + pc,
                       d ~ pc,
                       exposure = "pr",
                       outcome = "c",
                       latent="cl") 
tidy_dagitty(conflict_dag)

conflict_dag %>% 
  ggdag_dseparated(from = "pr", to = "c", controlling_for = "a",
                   text = FALSE, use_labels = "label")

ggdag_adjustment_set(conflict_dag)

impliedConditionalIndependencies(conflict_dag)

