#Problem 1-----------------------------
##Problem 1-(a) ----
set.seed(1)
n=200
x=seq(0, 1, length.out=n)
y=sin(2*pi*x)+rnorm(n, sd=0.15)

sim_data=data.frame(x=x, y=y)
head(sim_data)

write.csv(sim_data, "data/sim_data.csv", row.names=FALSE)
file.exists("data/sim_data.csv")
