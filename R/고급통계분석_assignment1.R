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
