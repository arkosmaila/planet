library(ggplot2)
library(tidyr)

mydf=data.frame(flat=rnorm(100000,0.5, 1),
                informative=rnorm(100000,0.5, 0.01),
                intermediate=rnorm(100000,0.5, 0.1)) %>%
  pivot_longer(cols=c(flat, informative, intermediate))
               
ggplot(mydf, aes(x=value, fill=name, color=name)) + 
  geom_density(alpha=0.5)+xlim(0,1)+theme_bw()+labs(x="", color="", fill="")+
  facet_wrap(. ~ name, ncol=1)
ggsave(file.path("C:/Users/chamcl/Desktop/Teaching/MaModInference/priors.png"),
       width = 3, height=7)
