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
d$lg_col<-ifelse(d$a.lgID=="NL", 'gray', 'black')
d$lg_shape<-ifelse(d$a.lgID=='NL', 2, 15)

# 190306

##
cor(d$lag_avg, d$change_rbi)
length(d$lag_avg)
length(d$change_rbi)
d$lg_col
d$lg_shape
d$a.lgID

### 전년도 타율과 타점 변화율 상관 관계
plot(d$lag_avg, d$change_rbi, main = 'Predictor of RBI', xlab=
       'Batting Average of Prior year', ylab = 'Change inRBI', las=1,
     cex.axis=0.8, pch=19, col=d$lg_col)

#d$lag_avg
#d$change_rbi

text(x=0.3, y=1.6, label='r=-0.49')
abline(lm(change_rbi~lag_avg, d))
legend(x=0.29, y=1.4, c('National', 'American'), col=c('gray', 'black'),
       pch=c(19,19))

##
cor(d$sac, d$change_rbi)

### 전년도 희생공격과 타점 변화율 상관관계
plot(d$sac, d$change_rbi, main='Predictor of RBI', font.main=3, xlab=
       'Sacrifice Files & Hits', ylab='Change in RBI', las=1,
     cex.axis=0.8, pch=d$lg_shape)
text(x=4, y=1.6, label='r=0.50')
abline(lm(change_rbi~sac, d), lty=2, lwd=3)


##
#install.packages('tableHTML')
library(tableHTML)
e<-with(d, data.frame(change_rbi, sac, lag_avg))
e
colnames(e)<-c('c_RBI', 'Sacrifice', 'AVG')
cor(e)

tableHTML(round(cor(e), 3))

## 데이터에서 룰을 찾다 : 연관성 분석
library(Lahman)
a<-subset(Batting, yearID>2010, select=c(playerID, teamID))
a$teamID<-factor(a$teamID)
a$teamID<-as.character(a$teamID)
str(a)

###
library(data.table)
move<-dcast(setDT(a)[, idx := 1:.N, by = playerID],
            playerID~idx, value.var=c('teamID'))
move
move[is.na(move)]<-''
move
move[,1]<-NULL
move
write.csv(move, file='move.csv')

rm(list=ls())

library(arules)
move<-read.transactions('move.csv', sep=',')
move
summary(move)

# 연관성 분석 테이블에 나타나는 팀 빈도수
itemFrequencyPlot(move, support=0.01, cex.names=0.6)

pattern<-apriori(move, list(support=0.0015, confidence=0.50, minlen=2))
summary(inspect(pattern))

#support = count(NYA, PHI, PIT) / support(NYA, PHI)

#lift = confidence(NYA, PHI --> PIT) / support(NYA, PHI, PIT)

# 지도에 산포도를 그리다 : R과 구글의 만남

#remove.packages('ggplot2')
#remove.packages('ggmap')
#install.packages('ggplot2')
#install.packages('ggmap')
library(ggplot2)
library(ggmap)

b<-get_map('Kansas City', zoom=4, maptype='roadmap')
?register_google
