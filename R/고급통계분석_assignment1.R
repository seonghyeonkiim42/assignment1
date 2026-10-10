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

#Problem 2-----------------------------
##Problem 2-(a)----
bubble_sort=function(vec, decreasing=FALSE){
  n=length(vec)
  if (n<2)
    return(vec)
  
  for (i in 1:(n-1)){
    swapped=FALSE
    for (j in 1:(n-i)){
      if (decreasing) {
        need_swap=vec[j] < vec[j+1]
      } else {
        need_swap=vec[j] > vec[j+1]
      }
      if (need_swap){
        temp=vec[j]
        vec[j]=vec[j+1]
        vec[j+1]=temp
        swapped=TRUE
      }
    }
    if (!swapped)
      break
  }
  return(vec)
}

set.seed(1)
x=runif(10)
print(x)

bubble_sort(x)
bubble_sort(x, decreasing=TRUE)

##Problem 2-(b)----
quick_sort=function(vec, decreasing=FALSE){
  n=length(vec)
  if (n<2)
    return(vec)
  
  pivot=vec[1]
  rest=vec[-1]
  
  if (decreasing) {
    left=rest[rest>pivot]
    right=rest[rest<=pivot]
  }
  else {
    left=rest[rest<pivot]
    right=rest[rest>=pivot]
  }
  return(c(quick_sort(left, decreasing),
           pivot,
           quick_sort(right, decreasing)))
}

set.seed(1)
x=runif(10)
print(x)

quick_sort(x) #오름차순(ASC)
quick_sort(x, decreasing=TRUE) #DESC

#Problem 3-----------------------------
#Problem 3-(a)
library(ggplot2)

num_deriv=function(f, x, h=1e-6, method="central"){
  if (method=="forward") {
    d=(f(x+h)-f(x)) / h
  }
  else if (method == "backward"){
    d=(f(x)-f(x-h))/h
  }
  else if (method =="central") {
    d=(f(x)-f(x-h))/(2*h)
  }
  else {
    stop("method는 forward, backward, central 중 하나여야 한다.")
  }
  return(d)
}

f=function(x) cos(x)-x
f_prime=function(x) - sin(x) -1

x_grid=seq(0, 2*pi, length.out=100)

d_true=f_prime(x_grid)
d_forward=num_deriv(f, x_grid, method="forward")
d_backward=num_deriv(f, x_grid, method="backward")
d_central=num_deriv(f, x_grid, method="central")

deriv_df=data.frame(
  x=rep(x_grid, 4),
  value=c(d_true, d_forward, d_backward, d_central),
  method=rep(c("analytic", "forward", "backward", "central"), each=100)
)

ggplot(deriv_df, aes(x=x, y=value, color=method)) +
  geom_line() + labs(title="Numerical vs. analytic derivative of f(x)=cos(x)-x",
                     x="x", y="f'(x)") + theme_minimal()
