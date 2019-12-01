LENGTH = 3

Agent_state_history <- NULL
Agent_print_flag <- TRUE
Agent_symbol <- NULL
Agent_V <- NA
board = matrix(rep(0,9),ncol = 3,nrow = 3)
env_X = -1
env_O = 1
env_winner = NULL
env_ended = FALSE
env_num_states = 3^(LENGTH * LENGTH)
Human_set_symbol<- NULL
Agent_random_learning <- 0.1
Agent_learning_rate <- 0.5



Agent_setV <- function(V){
  Agent_V <- V
  return(Agent_V)
}

Agent_set_symbol <- function(sym){
  Agent_symbol <- sym
  return(Agent_symbol)
}

Agent_print <- function(flag){
  Agent_print_flag <- print_flag
}

Agent_reset_history <- function(Agent_state_history){
  Agent_state_history <- vector()
}

Agent_Action <- function(board_loc,Agent_symbol,Agent_V){
  r = runif(1)
  best_state = NA
  next_move=NA
  if(r < Agent_random_learning)
  {
    if(Agent_print_flag == TRUE){
      print("random")
    }
    possible_moves = NULL
    for(i in seq(1,LENGTH,1)){
      for(j in seq(1,LENGTH,1)){
        if(board_loc[i,j]==0){
          possible_moves=append(possible_moves,list(c(i,j)),length(possible_moves))
        }
      }
    }
    index = sample(1:length(possible_moves),1)
    next_move = possible_moves[index]
  }
  else{
    value = matrix(,nrow = LENGTH,ncol = LENGTH)
    next_move = NA
    best_value = -1
    for(i in seq(1,LENGTH,1)){
      for(j in seq(1,LENGTH,1)){
        if(board_loc[i,j]==0){
          board_loc[i,j]=Agent_symbol
          state = Env_get_state(board_loc)
          board_loc[i,j]=0
          value[i,j]=Agent_V[state]
          if(Agent_V[state] > best_value){
            best_value = Agent_V[state]
            best_state = state
            next_move=list(c(i,j))
          }
        }
      }
    }
    if(Agent_print_flag == TRUE){
      print("greedy method")
     # print("------------------")
      if(board_loc[i,j]==0){
        print(value)
      }
      else{
       # print(board_loc)
      }
     # print("------------------")
    }
  }
    board_loc[next_move[[1]][1],next_move[[1]][2]]=Agent_symbol
    
    #print(list(board_loc,Agent_V))
    return(board_loc)
 
}

Agent_update_state_history <- function(s){
  Agent_state_history=append(Agent_state_history,s,length(Agent_state_history))
  return(Agent_state_history)
  }


Agent_update <- function(Agent_symbol,Agent_V,Agent_state_history,board_loc){
  reward <- Env_reward(Agent_symbol,board_loc)
  target = reward
  #print("state")
  #print(Agent_state_history)
  for(prev in rev(Agent_state_history)){
    #print("prev")
    #print(prev)
    value = Agent_V[prev]+Agent_learning_rate*(target - Agent_V[prev])
    Agent_V[prev] = value
    target = value
  }
  Agent_reset_history(Agent_state_history)
  return(Agent_V)
}

Env_reward <- function(symbol,board_loc){
  if(!Env_game_over(recalculate=TRUE,board_loc)[[2]]){
    return(0)
  }
  if(!is.na(Env_game_over(recalculate=TRUE,board_loc)[[1]])& Env_game_over(recalculate=TRUE,board_loc)[[1]] == symbol){
    return(1)
  }
  else
    return(0)
}

Env_get_state <- function(board_loc){
  k=0
  h=0
  for(i in seq(1,LENGTH,1)){
    for(j in seq(1,LENGTH,1)){
      if(board_loc[i,j]==0){
        v=0
      }else if(board_loc[i,j]== env_X){
        v=1
      }else if(board_loc[i,j]== env_O){
        v=2
      }
      h = h+(3^k)*v
      k = k+1
    }
  }
  return(h)
}

Env_game_over <- function(recalculate = FALSE,board_loc){
  if(!recalculate & env_ended)
    return(env_ended)
  for(i in seq(1, LENGTH , 1 )){
    for(player in c(env_X, env_O)){
      if(sum(board_loc[i,])== (player * LENGTH)){
        env_winner = player
        env_ended = TRUE
        return(list(env_winner,TRUE))
      }
    }
  }
  for(j in seq(1, LENGTH , 1 )){
    for(player in c(env_X, env_O)){
      if(sum(board_loc[,j])== (player * LENGTH)){
        env_winner = player
        env_ended = TRUE
        return(list(env_winner,TRUE))
      }
    }
  }
  for(player in c(env_X,env_O)){
    if(sum(diag(board_loc))==(player *LENGTH)){
      env_winner = player
      env_ended = TRUE
      return(list(env_winner,TRUE))
    }
  }
  for(player in c(env_X,env_O)){
    if(sum(diag(apply(board_loc,2,rev)))==(player *LENGTH)){
      env_winner = player
      env_ended = TRUE
      return(list(env_winner,TRUE))
    }
  }
  if(all(board_loc!=0) == TRUE){
    env_winner= NA
    env_ended = TRUE
    return(list(env_winner,TRUE))
  }
  env_winner = NA
  return(list(env_winner,FALSE))
}

Env_is_draw <- function(){
  if(env_ended & is.null(env_winner)){
    return(1)
  }else 
    return(0)
} 

env_draw_board <- function(board_loc){
  board_loc[which(board_loc==1)]= "B"
  board_loc[which(board_loc==-1)]="X"
  board_loc[which(board_loc==0)]=" "
  print("-------------")
  print(board_loc)
  print("-------------")
}

Human_set_symbol <- function(hum_symbol){
  human_set_symbol <- hum_symbol
  return(human_set_symbol)
}

Human_action <- function(Human_set_symbol,board_loc){
  
  while (TRUE) {
    move = readline("Enter coordinates i,j for your next move (i,j=1..3): ")
    move = strsplit(move,",")
   # print(move)
    i = as.numeric(move[[1]][1])
    j = as.numeric(move[[1]][2])
   # print(i,j)
    if(board_loc[i,j]==0){
      board_loc[i,j]= Human_set_symbol
      break
    }
  }
  return(board_loc)
}

get_state_hash_and_winner<- function(){
  ind=0
  state=NULL
  winner=NULL
  ended=NULL
  result=NULL
  matrix_all<-expand.grid(c(-1:1),c(-1:1),c(-1:1),c(-1:1),c(-1:1),c(-1:1),c(-1:1),c(-1:1),c(-1:1))
  for(i in seq(1,nrow(matrix_all),1)){
    board<-matrix(unlist(c(matrix_all[i,])),nrow=3)
    state[i]<-Env_get_state(board)
    winner[i]<-Env_game_over(recalculate=TRUE,board)
    ended[i]<-Env_game_over(recalculate=TRUE,board)[[2]]
    result<-append(result,list(c(state[i],winner[i],ended[i])),length(result))
  }
  return(result)
}

InitialV_X<- function(state_winner_triples){
  V=rep(0,env_num_states)
  for(triples in state_winner_triples){
    if(!is.na(triples[[3]])& triples[[3]] ){
      if(!is.na(triples[[2]])& triples[[2]]==env_X){
        v=1
      }else{
        v=0
      }
    }else{
      v=0.5
    }
    V[triples[[1]]]=v
  }
  return(V)
}

InitialV_O<- function(state_winner_triples){
  V=rep(0,env_num_states)
  for(triples in state_winner_triples){
    if(!is.na(triples[[3]])&triples[[3]] ){
      if(!is.na(triples[[2]])&triples[[2]]==env_O){
        v=1
      }else{
        v=0
      }
    }else{
      v=0.5
    }
    V[triples[[1]]]=v
  }
  return(V)
}

play_game<- function(p1_sym,p2_sym,p1_v,p2_v=NULL,board_loc,draw=FALSE,Bot=TRUE){
  current_player = p1_sym
  p1_state_history=NULL
  p2_state_history=NULL
  while(!Env_game_over(recalculate=TRUE,board_loc)[[2]]){
    if(current_player==p1_sym){
      board_loc<- Agent_Action(board_loc,p1_sym,p1_v)
      board_loc<-as.matrix(board_loc,nrow=3)
      #print(class(board_loc))
      #print(board_loc)
      current_player=p2_sym
     # print(current_player)
    }
    else{
      if(Bot==TRUE){
      board_loc<- Agent_Action(board_loc,p2_sym,p2_v)
      }
      else{
      
      board_loc<-Human_action(p2_sym,board_loc)
      }
      board_loc<-as.matrix(board_loc,nrow=3)
      current_player=p1_sym
    }
    state = Env_get_state(board_loc)
    p1_state_history<-append(p1_state_history,state,length(p1_state_history))
    #print("play_History")
    #print(p1_state_history)
    p2_state_history<-append(p2_state_history,state,length(p2_state_history))
    
    if (draw) {
      env_draw_board(board_loc)
      
    }
  }

  p1_v<-Agent_update(p1_sym,p1_v,p1_state_history,board_loc)
  p2_v<-Agent_update(p2_sym,p2_v,p2_state_history,board_loc)
  return(list(p1_v,p2_v))
}


state_winner_triples = get_state_hash_and_winner()
Vx = InitialV_X( state_winner_triples)
Vo = InitialV_O( state_winner_triples)
P1_V<-Agent_setV(Vx)
P1_sym<-Agent_set_symbol(1)

P2_V<-Agent_setV(Vo)
P2_sym<-Agent_set_symbol(-1)
try = 10000
for(t in seq(1,try,1)){
  if(t %% 200 == 0){
    print(t)
  }
  P1_V<- play_game(P1_sym, P2_sym, P1_V,P2_V,board,draw = FALSE)[[1]]
  P2_V<- play_game(P1_sym, P2_sym, P1_V,P2_V,board,draw = FALSE)[[2]]
}
Hum_Sym<-Human_set_symbol(-1)
while(TRUE){
  P1_V<- play_game(p1_sym =  P1_sym, p2_sym = Hum_Sym,p1_v =  P1_V,board_loc = board,Bot=FALSE,draw = TRUE)[[1]]
}
