rm(list=ls(all=TRUE))
graphics.off()


lambda = 3
t = seq(0,2, by = .01)
delta_vec = seq(1,0, by = -.05); length(delta_vec)
# i <- 3
delta <-  22# delta_vec[i]

power <- t/delta
F_t = 1 - (1 + (-lambda*t / (t / delta)))^(t/delta)
F_t



F_t2 = 1 - exp(-lambda*t)
F_t2

plot(t, F_t2)


for(i in 1:length(delta_vec)){
  delta <- delta_vec[i]
  F_t = 1 - (1+ (-lambda*t / t / delta))^(t/delta)
plot(t, F_t)
  }





x_break <- 3
n <- 100000
l <- runif(n = n, min = 1, max = 10)
sum(((l - x_break) / x_break) < 0)
sum((l - x_break) < 0)
sum(((l - x_break) / x_break) > 0)







#######################################
#######################################



f_ab <- function(t, a, b) a*b/(a-b)*exp(-b*t)*(1-exp(-(a-b)*t))

dt <- 0; tmax <- 1000; stept = 1
t = seq(from=dt, to=tmax, by = stept)
a = 1/100; b= 1/50
# a = 1/10; b= 1/5
func <- f_ab(t, a, b)
tmp <- cbind(t, func)
tmp[tmp[,2] == max(func),]
abline(v = 69)
plot(func, type = "l")
store <- cumsum(func)
plot(store, type = "l")


#######################################
#######################################
p=.05
# n=50
n = seq(from=10, to=1000, by=1)
k=6

n*p
func = choose(n = n-1, k = k-1) * p^k * (1-p)^(n-k)
func
plot(func, type = "l")
store <- cumsum(func)
plot(store, type = "l")


# n = seq(from=10, to=100, by=1)
tmp <- c()




#######################################
#######################################
library(stats)
?rgamma


plot(density(rgamma(n = 10000, shape = 1, rate = 1/50)),col="red")
lines(density(rgamma(n = 10000, shape = 2, rate = 1/50)),col="green")
lines(density(rgamma(n = 10000, shape = 3, rate = 1/50)),col="blue")
1/(1/50)
mean(rgamma(n = 10000, shape = 1, rate = 1/50))
1/(1/50)^2
var(rgamma(n = 10000, shape = 1, rate = 1/50))


3/(1/50)
mean(rgamma(n = 10000, shape = 3, rate = 1/50))



# lamba = rate
# k = shape
func_erlang <- Vectorize(function(k = 1, lambda = 1/50, t = tvec){
result <- lambda * exp(-lambda*t)*((lambda*t)^(k-1)) / factorial(k-1)
return(result)
})

func_erlang2 <- function(k = 1, lambda = 1/50, t = tvec){
  result <- lambda * exp(-lambda*t)*((lambda*t)^(k-1)) / factorial(k-1)
  return(result)
}

sum(func_erlang(k = 1, lambda = lambda, t = seq(from=1, to=1000, by = 1)) ==
      func_erlang2(k = 1, lambda = lambda, t = seq(from=1, to=1000, by = 1)))


tvec <- seq(from=1, to=500, by = 1)
lambda <- 1/50


plot(tvec, func_erlang(k = 1, lambda = lambda, t = tvec),
     type = "l", col = "red")
vari <- data.frame(col = c("green", "blue", "yellow", "brown", "purple"),
                   k = c(2,3,4,5,6))
for(i in 1:nrow(vari)){
  lines(tvec, func_erlang(k = vari$k[i], lambda = lambda, t = tvec),
       type = "l", col = vari$col[i])
}



#######################################
#######################################
func_poisson2 <- Vectorize(function(k = 1, lambdat = 3/2){
  result <- (exp(-lambdat)*(lambdat^k) / factorial(k))^2
  return(result)
})

kvec <- seq(from=0, to=100, by = 1)
sum(func_poisson2(k = kvec))
plot(kvec, func_poisson2(k = kvec),
     type = "l", col = "red")
# func_poisson2(k = 0:100)
# diff(func_poisson2(k = 1:10), lag=1)



sum(rexp(n = 100, rate = 1))
sum(rexp(n = 100, rate = 2))
sum(rexp(n = 100, rate = 3))


t = seq(from=1, to=10, by =1)
lambda = .3
# 100*(1-exp(-lambda*t))
sum(100*(1-exp(-lambda*t))); sum(1-exp(-lambda*t))
plot(t, 1-exp(-lambda*t))





#######################################
#######################################
# rbernoulli(n = 1, p = .95)
# rbinom(n = 100, size = 1, prob = .5)

player <- 30
casino <- 100
prob_losing <- .49
vec_player <- c()
vec_player[1] <- player
i=2
while(player > 0 && player < casino){
  if(purrr::rbernoulli(n = 1, p = prob_losing)){
    player = player - 1
    vec_player[i] <- player
  } else{
    player = player + 1
    vec_player[i] <- player
  }
  i=i+1
}
plot(vec_player)

# (1-(.4/.6)^30) / (1-(.4/.6)^100)

player <- 30
casino <- 100
prob_losing <- .49
prob_winning <- 1-prob_losing
(1-(prob_losing/prob_winning)^player) / (1-(prob_losing/prob_winning)^casino)


###################################
vec_outcome <- c()

for(l in 1:2000){
player <- 30
casino <- 100
prob_losing <- .49
vec_player <- c()
vec_player[1] <- player
i=2
while(player > 0 && player < casino){
  if(purrr::rbernoulli(n = 1, p = prob_losing)){
    player = player - 1
    vec_player[i] <- player
  } else{
    player = player + 1
    vec_player[i] <- player
  }
  i=i+1
}
if(vec_player[length(vec_player)] == casino){
  # print("win")
  vec_outcome[l] <- "win"
} else if(vec_player[length(vec_player)] == 0){
  # print("lose")
  vec_outcome[l] <- "lose"
}
}

table(vec_outcome) / 2000
