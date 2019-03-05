# baseball_book_Ch03

library(Lahman)
a <- subset(Batting, yearID ==2014)
b <- subset(Batting, yearID ==2015)
c <- merge(a, b, by='playerID')
c
## 두 시즌 모두 10타석 넘는 선수의 상관관계 뽑아보기
d<-c[c$AB.x>10&c$AB.y>10, ]
d

cor(d$HR.x, d$HR.y)
cor(d$H.x/d$AB.x, d$H.y/d$AB.y)

## AVG = H/AB (타율 = 유효한 히트 수 / 사구, 볼넷 제외 유효 타석 수 )

## 출루율 OBP = (안타 + 몸에 맞는 볼 + 포볼) / (타석 + 몸에 맞는 볼 + 포볼 + 희생타)

## 장타율 SLG = (단타 +2*2루타 + 3*3루타 +4*홈런)/타석 

# 공격 지표들을 이용한 상관관계
library(Lahman)
library(plyr)
a<-subset(Batting, yearID>2014)
a$teamID <- as.numeric(as.factor(a$teamID))
b<-function(a){return(data.frame(
  team=ifelse(mean(a$teamID)==a$teamID, 0, 1),
  a$playerID, a$lgID, a$SF, a$SH, a$H,
  a$yearID, a$teamID, a$RBI, a$AB))}
d<-ddply(a,.(playerID),b)


a$teamID
mean(a$teamID)
tail(Batting)

d$lag_team <-as.numeric(sapply(1:nrow(d), function(x){d$a.teamID[x-1]}))
d$lag_RBI <- as.numeric(sapply(1:nrow(d), function(x){d$a.RBI[x-1]}))
d$lag_AB <- as.numeric(sapply(1:nrow(d), function(x){d$a.AB[x-1]}))
d$lag_SF <- as.numeric(sapply(1:nrow(d), function(x){d$a.SF[x-1]}))
d$lag_SH <- as.numeric(sapply(1:nrow(d), function(x){d$a.SH[x-1]}))
d$lag_H <- as.numeric(sapply(1:nrow(d), function(x){d$a.H[x-1]}))
d$lag_playerID <- as.character(sapply(1:nrow(d), function(x){d$playerID[x-1]}))

d$lag_avg<-d$lag_H/d$lag_AB
d$sac<-d$lag_SF+d$lag_SH
d<-subset(d, a.AB>400&lag_AB>400)
d$change_rbi<-d$a.RBI/d$lag_RBI
d<-subset(d, !((lag_team=='NA')|(a.teamID==lag_team)))
d$lg_col<-iflese(d$a.lgID=='NL', 'gray', 'black')
d$lg_shape<-ifelse(d$a.lgID=='NL', 2, 15)
