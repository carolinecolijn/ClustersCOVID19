
## Plots of the estimated incubation period distributions for Singapore and Tianjin
# (gamma distributed)

library(ggplot2)
library(viridis)

p1 <- ggplot(data = data.frame(x = c(0, 20)), aes(x)) +
  stat_function(fun = dgamma, n = 1001, size=0.8, args = list(shape=3.05, scale = 1.95), aes(colour = "Singapore, all")) + ylab("") +
  stat_function(fun = dgamma, n = 1001, size=0.8,args = list(shape=3.22, scale = 1.818  ), aes(colour = "Singapore, early")) +
  stat_function(fun = dgamma, n = 1001, size=0.8,args = list(shape=2.96, scale = 2.034 ), aes(colour = "Singapore, late")) +
  scale_y_continuous() +
  stat_function(fun = dgamma, n = 1001, size=0.8,args = list(shape=4.74, scale = 1.83), aes(colour = "Tianjin, all")) + 
  theme_classic() +
  labs(x = "Days")  +
  stat_function(fun = dgamma, n = 1001, size=0.8,args = list(shape=6.01, scale = 1.140 ), aes(colour = "Tianjin, early")) +
  stat_function(fun = dgamma, n = 1001, size=0.8,args = list(shape=17.78, scale = 0.695 ), aes(colour = "Tianjin, late")) +
  scale_colour_manual("", values = c(viridis(256)[110], viridis(256)[40], viridis(256)[170], inferno(256)[200], inferno(256)[150], inferno(256)[230]))+
  guides(color = guide_legend(override.aes = list(size = 2)))
p1

#ggsave(filename = "final_figures/Fig3_dist_inc.pdf")




