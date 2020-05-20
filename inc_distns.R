
## Plots of the estimated incubation period distributions for Singapore and Tianjin

library(ggplot2)
library(viridis)

p1 <- ggplot(data = data.frame(x = c(0, 20)), aes(x)) +
  stat_function(fun = dweibull, n = 1001, size=0.8, args = list(shape=1.83, scale = 6.91), aes(colour = "Singapore, all")) + ylab("") +
  stat_function(fun = dweibull, n = 1001, size=0.8,args = list(shape=2.05, scale = 6.587  ), aes(colour = "Singapore, early")) +
  stat_function(fun = dweibull, n = 1001, size=0.8,args = list(shape=1.75, scale = 6.989 ), aes(colour = "Singapore, late")) +
  scale_y_continuous() +
  stat_function(fun = dweibull, n = 1001, size=0.8,args = list(shape=2.41, scale = 10.01), aes(colour = "Tianjin, all")) + 
  theme_classic() +
  labs(x = "Days")  +
  stat_function(fun = dweibull, n = 1001, size=0.8,args = list(shape=2.88, scale = 7.643 ), aes(colour = "Tianjin, early")) +
  stat_function(fun = dweibull, n = 1001, size=0.8,args = list(shape=4.34, scale = 13.661 ), aes(colour = "Tianjin, late")) +
  scale_colour_manual("", values = c(viridis(256)[110], viridis(256)[40], viridis(256)[170], inferno(256)[200], inferno(256)[150], inferno(256)[230]))+
  guides(color = guide_legend(override.aes = list(size = 2)))
p1



require(gridExtra)

p1 <- ggplot(data = data.frame(x = c(0, 20)), aes(x)) +
  stat_function(fun = dweibull, n = 101, args = list(shape=1.88, scale = 7.97), aes(colour = "Singapore")) + ylab("") +
  stat_function(fun = dweibull, n = 101, args = list(shape=2.35, scale = 6.38  ), aes(colour = "Singapore, early")) +
  stat_function(fun = dweibull, n = 101, args = list(shape=1.93, scale = 8.79 ), aes(colour = "Singapore, late")) +
  scale_y_continuous() +
  theme_classic() +
  labs(x = "Days")  +
  scale_colour_manual("", values = c(inferno(256)[110], inferno(256)[80], inferno(256)[140]))

p2 <- ggplot(data = data.frame(x = c(0, 20)), aes(x)) +
  ylab("") +
  scale_y_continuous() +
  stat_function(fun = dweibull, n = 101, args = list(shape=2.25, scale = 10.15), aes(colour = "Tianjin")) + theme_classic() +
  labs(x = "Days")  +
  stat_function(fun = dweibull, n = 101, args = list(shape=2.73, scale = 7.84 ), aes(colour = "Tianjin, early")) +
  stat_function(fun = dweibull, n = 101, args = list(shape=3.95, scale = 13.636 ), aes(colour = "Tianjin, late")) +
  scale_colour_manual("", values = c( inferno(256)[200], inferno(256)[170], inferno(256)[230]))

grid.arrange(p1, p2, ncol=2)



