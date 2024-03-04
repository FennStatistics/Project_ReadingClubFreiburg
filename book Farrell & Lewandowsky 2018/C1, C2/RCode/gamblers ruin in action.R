#######################################
#######################################

### single process + plot
player <- 10
casino <- player*1000
player/casino*100


prob_losing <- 19/37
prob_losing # loosing
1-prob_losing # winning
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



### formula long term
player <- 10
casino <- player * 1000
player/casino*100
prob_losing <- 19/37
prob_winning <- 1-prob_losing
(1-(prob_losing/prob_winning)^player) / (1-(prob_losing/prob_winning)^casino)




### simulation study X tries
vec_outcome <- c()
repetitions <- 100
for(l in 1:repetitions){
  player <- 20
  casino <- 100
  prob_losing <- .51
  vec_player <- c()
  vec_player[1] <- player
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
    print("win")
    vec_outcome[l] <- "win"
  } else if(vec_player[length(vec_player)] == 0){
    print("lose")
    vec_outcome[l] <- "lose"
  }
}

table(vec_outcome) / repetitions


### formula long term
player <- 20
casino <- 100
prob_losing <- .4999
prob_winning <- 1-prob_losing
(1-(prob_losing/prob_winning)^player) / (1-(prob_losing/prob_winning)^casino)
