# the birthday problem
#======================
roll_dice<-function(k){
  all_r<-sample(c(1:6),k,replace=T)
  return(sum(all_r))
}
roll_dice(2)

roll_sim<-function(n){
  counter<-0
  roll<-replicate(n,roll_dice(2))
  sum(roll==12)
}
m<-10000
prob<-roll_sim(m)/m
1/36

# built-in function
pbirthday(10)
sapply(1:10,pbirthday)
plot(sapply(1:10,pbirthday))

pbirthday(100)
sapply(1:100,pbirthday)
plot(sapply(1:100,pbirthday))

n<-10
match<-0
m<-1000
for(i in 1:m){
  bday<-sample(365,n,replace=T)
  if(length(unique(bday))<n){
    match<-match+1
  }
}
prob_m<-match/m
prob_m
pbirthday(10)

#monthy hall
doors<-c(1:3)
## choose door 1 and prize in door 2
reveal<-doors[-c(1,2)] # because always reveal the one with no price
reveal

# suppouse that door 1 is chosen and it contains the prize
# so the presentator selects the other door at random
sample(doors[-1],size=1,replace=F)

set.seed(1)
doors <- c(1,2,3)
prize <- sample(x =doors, size = 1)
initial_choice <- 1
if(prize==initial_choice){
  print("The initial choice was correct!")
}
print(prize)

count<-0
sele_f<-function(){
  prize <- sample(x =doors, size = 1)
  initial_choice <-  sample(x =doors, size = 1)
  if(prize==initial_choice){
    count<-count+1
  }
  return(count)
}
m<-1000
sum(replicate(m,sele_f()))/m
