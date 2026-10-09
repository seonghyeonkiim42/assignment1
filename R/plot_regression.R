##Problem 1-(b) ---
library(ggplot2)

sim_data=read.csv("data/sim_data.csv")
head(sim_data)

p_reg=ggplot(sim_data, aes(x=x, y=y)) +
  geom_point(alpha=0.6)+
  geom_smooth(method="loess", formula=y~x, span=0.3)+
  labs(title="Scatter plot and LOESS fit", x="x", y="y")+
  theme_minimal()
print(p_reg)

##Problem 1-(c)----
ggsave("plots/regression_fit.png", plot=p_reg, width=7, height=4.5, dpi=300)
