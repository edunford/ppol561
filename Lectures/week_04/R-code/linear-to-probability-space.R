
# Linear transition animation

require(tidyverse)
require(gganimate)

set.seed(123)
N = 100
x <- rnorm(N)  

y_star <- 2*x 
pr <- pnorm(y_star)


p <- 
  tibble(y_star,pr,x) %>% 
  gather(key,val,-x) %>%
  mutate(time = ifelse(key=="y_star",1,2),
         label = ifelse(key=="y_star","XB","F(XB)")) %>% 
  ggplot(aes(x,val)) +
  geom_point(size=4,alpha=.5,color="steelblue") +
  scale_color_identity() +
  theme_bw() + 
  labs(y="Scale",x=latex2exp::TeX("$X\\beta$")) +
  transition_states(time,
                    state_length = c(3,3),
                    wrap=T)+ 
  theme(axis.title = element_text(family="serif",size=16)) +
  enter_fade() +
  exit_fade() +
  ease_aes('sine-in-out') +
  view_follow()

# Render
anim = animate(p,nframes = 75)

# Save Animation
save_animation(anim,file = "Lectures/week_04/Figures/lin-to-pr-space.gif")
