


a1 <- 
  tibble(y = 4,
         x = 3,
         candidate= c("i"),
         size=c(50,50)) 


a1 %>% 
  ggplot(aes(x,y,color=candidate,size=size)) +
  geom_point() +
  geom_text(color="white",aes(label=candidate),size=5) +
  scale_color_manual(values = c("grey30")) +
  scale_size_identity() +
  ylim(0,5) + xlim(0,6) +
  theme_void() +
  theme(legend.position = "none")




a2 <- 
  tibble(y=c(2,4,2),
         x = c(1,3,5),
         candidate= c("Grad School","You","No Grad School"),
         size=c(50,50,50),
         alpha=c(1,1,1),
         color=c("steelblue","grey30","darkred")) 

a2 %>% 
  ggplot(aes(x,y,size=size,color=color,alpha=alpha)) +
  geom_segment(x=3,xend=1,y=4,yend=2,size=.5,alpha=.2,color="grey40") +
  geom_segment(x=3,xend=5,y=4,yend=2,size=.5,alpha=.2,color="grey40") +
  geom_point() +
  scale_color_identity() +
  scale_alpha_identity() +
  scale_size_identity() +
  geom_text(color=c("white","white","white"),aes(label=candidate),size=5) +
  ylim(0,5) + xlim(0,6) +
  theme_void() +
  theme(legend.position = "none")



a3 <- 
  tibble(y=c(2,5,2,-5),
         x = c(1,3,5,3),
         candidate= c("Grad School","You","No Grad School","$$$"),
         size=rep(50,4),
         alpha=c(.5,1,.5,1),
         color=c("steelblue","grey30","darkred","steelblue")) 

a3 %>% 
  ggplot(aes(x,y,size=size,color=color,alpha=alpha)) +
  geom_segment(x=3,xend=1,y=5,yend=2,size=.5,alpha=.2,color="grey40") +
  geom_segment(x=3,xend=5,y=5,yend=2,size=.5,alpha=.2,color="grey40") +
  geom_point() +
  scale_color_identity() +
  scale_alpha_identity() +
  scale_size_identity() +
  geom_text(color="white",aes(label=candidate),size=5) +
  ylim(-10,10) + xlim(0,6) +
  theme_void() +
  theme(legend.position = "none")




bind_rows(
  tibble(y=5,x=3,candidate = "You",size=50,alpha=1,color="grey30"),
  tibble(y=2,x=1,candidate = "Grad School",size=50,alpha=1,color="steelblue"),
  tibble(y=2,x=5,candidate = "No Grad School",size=50,alpha=1,color="darkred"),
  tibble(y=0,x=3,candidate = "$$$",size=50,alpha=1,color="black"),
) %>% 
  
  ggplot(aes(x,y,size=size,color=color,alpha=alpha)) +
  geom_segment(x=3,xend=1,y=5,yend=2,size=.5,alpha=.2,color="grey40") +
  geom_segment(x=3,xend=5,y=5,yend=2,size=.5,alpha=.2,color="grey40") +
  geom_segment(x=1,xend=3,y=2,yend=0,size=.5,alpha=.2,color="grey40") +
  geom_segment(x=5,xend=3,y=2,yend=0,size=.5,alpha=.2,color="grey40") +
  geom_point() +
  scale_color_identity() +
  scale_alpha_identity() +
  scale_size_identity() +
  geom_text(color="white",aes(label=candidate),size=5) +
  ylim(-10,10) + xlim(0,6) +
  theme_void() +
  theme(legend.position = "none")
