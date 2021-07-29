# Euro-2020-Predictive-Model-based-on-FIFA-Ranking-System

we built a Predictive Model based on FIFA Ranking and making the assumption that the points follow a normal distribution. If we look closer at FIFA’s Ranking Model we will see that it is based on the ELO System where the expected result of the game can be extracted from the following formula as attached in the .docx file in this repository.



Let’s run 10000 simulations to estimate the probability of the Euro 2020 Winner.

# get the UEFA Ranking


ranking<-list(bel=1783,
fra=1757,
eng=1687,
por=1666,
esp=1648,
ita=1642,
den=1632,
ger=1609,
sui=1606,
cro=1606,
net=1598,
wal=1570,
swe=1570,
aus=1523,
ukr=1515,
cze=1459)

win_prob<-function(a,b) {
  
  w_prob<-1/(1+10^(-(a-b)/600))
  outcome<-sample(c(TRUE,FALSE), 1, prob=c(w_prob, 1-w_prob))
  return(outcome)
}

sim_champion<-c()

for (i in 1:10000) {

#######################
#### Final 16
#######################

# game bel vs por
teams_game1<-c("bel","por")
game1<-win_prob(ranking[[teams_game1[1]]], ranking[[teams_game1[2]]])
if (game1) {
  qualified_game1<-teams_game1[1] 
  } else {
  qualified_game1<-teams_game1[2]}

qualified_game1


# game ita vs aus
teams_game2<-c("ita","aus")
game2<-win_prob(ranking[[teams_game2[1]]], ranking[[teams_game2[2]]])
if (game2) {
  qualified_game2<-teams_game2[1] 
} else {
  qualified_game2<-teams_game2[2]}

qualified_game2


# game fra vs sui
teams_game3<-c("fra","sui")
game3<-win_prob(ranking[[teams_game3[1]]], ranking[[teams_game3[2]]])
if (game3) {
  qualified_game3<-teams_game3[1] 
} else {
  qualified_game3<-teams_game3[2]}

qualified_game3


# game cro vs esp
teams_game4<-c("cro","esp")
game4<-win_prob(ranking[[teams_game4[1]]], ranking[[teams_game4[2]]])
if (game4) {
  qualified_game4<-teams_game4[1] 
} else {
  qualified_game4<-teams_game4[2]}

qualified_game4



# game swe vs ukr
teams_game5<-c("swe","ukr")
game5<-win_prob(ranking[[teams_game5[1]]], ranking[[teams_game5[2]]])
if (game5) {
  qualified_game5<-teams_game5[1] 
} else {
  qualified_game5<-teams_game5[2]}

qualified_game5


# game eng vs ger
teams_game6<-c("eng","ger")
game6<-win_prob(ranking[[teams_game6[1]]], ranking[[teams_game6[2]]])
if (game6) {
  qualified_game6<-teams_game6[1] 
} else {
  qualified_game6<-teams_game6[2]}

qualified_game6


# game net vs cze
teams_game7<-c("net","cze")
game7<-win_prob(ranking[[teams_game7[1]]], ranking[[teams_game7[2]]])
if (game7) {
  qualified_game7<-teams_game7[1] 
} else {
  qualified_game7<-teams_game7[2]}

qualified_game7


# game wal vs den
teams_game8<-c("wal","den")
game8<-win_prob(ranking[[teams_game8[1]]], ranking[[teams_game8[2]]])
if (game8) {
  qualified_game8<-teams_game8[1] 
} else {
  qualified_game8<-teams_game8[2]}

qualified_game8



#######################
#### Final 8
#######################

teams_f8_1<-c(qualified_game1,qualified_game2)
game_f8_1<-win_prob(ranking[[teams_f8_1[1]]], ranking[[teams_f8_1[2]]])


if (game_f8_1) {
  qualified_f8_1<-teams_f8_1[1] 
} else {
  qualified_f8_1<-teams_f8_1[2]}

qualified_f8_1


teams_f8_2<-c(qualified_game3,qualified_game4)
game_f8_2<-win_prob(ranking[[teams_f8_2[1]]], ranking[[teams_f8_2[2]]])

if (game_f8_2) {
  qualified_f8_2<-teams_f8_2[1] 
} else {
  qualified_f8_2<-teams_f8_2[2]}

qualified_f8_2


teams_f8_3<-c(qualified_game5,qualified_game6)
game_f8_3<-win_prob(ranking[[teams_f8_3[1]]], ranking[[teams_f8_3[2]]])

if (game_f8_3) {
  qualified_f8_3<-teams_f8_3[1] 
} else {
  qualified_f8_3<-teams_f8_3[2]}

qualified_f8_3


teams_f8_4<-c(qualified_game7,qualified_game8)
game_f8_4<-win_prob(ranking[[teams_f8_4[1]]], ranking[[teams_f8_4[2]]])

if (game_f8_4) {
  qualified_f8_4<-teams_f8_4[1] 
} else {
  qualified_f8_4<-teams_f8_4[2]}

qualified_f8_4


#######################
#### Final 4
#######################

teams_f4_1<-c(qualified_f8_1,qualified_f8_2)
game_f4_1<-win_prob(ranking[[teams_f4_1[1]]], ranking[[teams_f4_1[2]]])

if (game_f4_1) {
  qualified_f4_1<-teams_f4_1[1] 
} else {
  qualified_f4_1<-teams_f4_1[2]}

qualified_f4_1


teams_f4_2<-c(qualified_f8_3,qualified_f8_4)
game_f4_2<-win_prob(ranking[[teams_f4_2[1]]], ranking[[teams_f4_2[2]]])


if (game_f4_2) {
  qualified_f4_2<-teams_f4_2[1] 
} else {
  qualified_f4_2<-teams_f4_2[2]}

qualified_f4_2


#######################
#### Final 
#######################

teams_f<-c(qualified_f4_1,qualified_f4_2)
game_f<-win_prob(ranking[[teams_f[1]]], ranking[[teams_f[2]]])

if (game_f) {
  champion<-teams_f[1] 
} else {
  champion<-teams_f[2]}

sim_champion<-c(sim_champion,champion)
}


prop.table(table(sim_champion))*100

Outcome:

sim_champion
  aus   bel   cro   cze   den   eng   esp   fra   ger   ita   net   por   sui   swe   ukr   wal 
 1.68 16.22  3.74  1.33  7.64 10.17  6.53 14.70  4.75  6.83  5.99  6.21  3.38  4.37  2.28  4.18 

As we can see, Belgium has 16.22% to win the Euro 2020 and it is the favorite according to this approach.
