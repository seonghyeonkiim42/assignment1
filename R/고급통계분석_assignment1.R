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
    d=(f(x+h)-f(x-h))/(2*h)
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

##Problem 3-(b)----
newton_raphson=function(f, fprime=NULL, x0, maxiter=100, h=1e-6, epsilon=1e-10){
  if (is.null(fprime)) {
    fprime=function(x) num_deriv(f, x, h=h, method="central")
  }
  x_old=x0
  path=x0
  converged=FALSE
  
  for(iter in 1:maxiter)
  {
    slope=fprime(x_old)
    if (slope==0)
      stop("기울기가 0이라 더 이상 진행 불가")
    
    x_new=x_old-f(x_old)/slope
    path=c(path, x_new)
    
    if(abs(x_new-x_old)<epsilon) {
      converged=TRUE
      break
    }
    x_old=x_new
  }
  if(!converged) warning("maxiter 안에 수렴하지 못 함")
  return (list(root=x_new, iter=iter, converged=converged, path=path))
}

##Problem 3-(c)----
f=function(x) cos(x)-x
f_prime=function(x) - sin(x) -1

res_num = newton_raphson(f, x0=0.5)
res_ana=newton_raphson(f, fprime=f_prime, x0=0.5)

print(res_num$root, digits=15)
print(res_ana$root, digits=15)
res_num$iter
res_ana$iter

print(res_num$path, digits=15)
print(res_ana$path, digits=15)

#Problem 4-----------------------------
##Problem 4-(a)----
left_rectangle=function(f, a, b, n){
  h=(b-a)/n
  x=a+(0:(n-1))*h
  return(h*sum(f(x)))
}

##Probelm 4-(b)
trapezoid=function(f, a, b, n){
  h=(b-a)/n
  fx=f(a+(0:n)*h)
  return(h/2*(fx[1]+2*sum(fx[2:n]+fx[n+1])))
}

##Problem 4-(c)
simpson=function(f, a, b, n){
  if (n %%2 !=0)
    stop("Simpson method은 n이 짝수이어야 함")
  
  h=(b-a)/n
  fx=f(a+(0:n)*h)
  
  i=1:(n-1)
  odd_i=i[i%%2==1]
  even_i = i[i%%2==0]
  
  return(h/3*(fx[1]+4*sum(fx[odd_i+1])+2*sum(fx[even_i+1])+fx[n+1]))
}

##Problem 4-(d)
n=100
res_left=left_rectangle(sin, 0, pi, n)
res_trap=trapezoid(sin, 0, pi, n)
res_simp=simpson(sin, 0, pi, n)

print(c(lef=res_left, trap=res_trap, simpson=res_simp), digits=15)

##Problem 4-(e)
n_vec=c(10,30,60, 100, 150, 200)
true_val=2

err_left=sapply(n_vec, function(n) abs(left_rectangle(sin, 0, pi, n)-true_val))
err_trap=sapply(n_vec, function(n) abs(trapezoid(sin, 0, pi, n)-true_val))
err_simp=sapply(n_vec, function(n) abs(simpson(sin, 0, pi, n))-true_val)

err_table=data.frame(n=n_vec, left=err_left, trap=err_trap, simpson=err_simp)
print(err_table, digits=5)

err_df=data.frame(
  n=rep(n_vec, 3),
  error=c(err_left, err_trap, err_simp),
  method=rep(c("Left rectangle", "Trapezoid", "Simpson"), each=6)
)

ggplot(err_df, aes(x=n, y=error, color=method, linetype = method)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Absolute error of numerical integration of sin(x) on [0, pi]",
       x = "n (log scale)", y = "|approx - 2| (log scale)") +
  theme_minimal()
