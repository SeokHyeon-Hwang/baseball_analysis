# 책 <메이저리그 야구 통계학>, 김재민 지음
# chapter2. 메이저리그 데이터 마이닝

# 간단한 데이터 내 손으로 직접 만들기
a<-c(0.280, 0.257, 0.312, 0.266, 0.295)
a

b<-c('Eric', 'John', 'Steven', 'Keith', 'Kim')
b

c<-c(Eric, John, Steven, Keith, Kim)
c

d<-c(a,b)
d
mode(d)

# 데이터 불러오기
#batting <- read.csv(file.choose(), header=True)
#batting <- read.csv('C:/User/Desktop/파일이름.csv')
#install.packages('Lahman')
library(Lahman)

data(Batting)
View(Batting)
# Master dataset
#View(Master)
#Pitching 데이터
a<-subset(Pitching, playerID=='kershcdl01')
#a<-Pitching[Pitching$playerID=='kershcl01',]
a

b<-subset(Pitching, playerID=='투수 ID 투입')
#b<-Pitching[Pitching$playerID=='투수 ID 투입']
b

c<-subset(Batting, playerID=='타자 ID 투입')
#c<-Batting[Batting$playerID=='투수 ID 투입']
c

a <- Pitching[Pitching$playerID=='kershcl01',]
a

# 순서가 같은 테이블 합치기: cbind(), rbind()
a<-c('A', 'B', 'C', 'D', 'E')
b<-c(0.280, 0.257, 0.312, 0.266, 0.295)
c<-cbind(a, b)
c

colnames(c) <- c('player', 'avg')
c

age<-c(26, 23, 31, 27, 24)
d<- cbind(c, age)
d

a<-c('A', 'B', 'C', 'D', 'E')
b<-c(0.280, 0.257, 0.312, 0.266, 0.295)
f<-rbind(a,b)
f

# 순서가 다른 테이블 합치기 : merge()
d<-matrix(c('C', 'D', 'E', 'B', 'A', 26, 23, 31, 27, 24), ncol=2)
d

colnames(d) <- c('player', 'age')
d

c

e<-merge(c, d, by='player')
e

# 양적 변수를 명목 변수로 바꾸기
a<-c('A', 'B', 'C', 'D', 'E')
b<-c(0.280, 0.257, 0.312, 0.266, 0.295)
c<-cbind(a,b)
colnames(c) <- c('player', 'avg')
c

d<-matrix(c('C', 'D', 'E', 'B', 'A', 26, 23, 31, 27, 25), ncol=2)
colnames(d) <- c('player', 'age')
d
e<-merge(c, d, by='player')
e

str(e)

#e$age <- as.numeric(e$age)
# 이렇게 하면 나이가 클래스로 구분되어 원하는 결과가 안나온다.
#e
#str(e)


e$age <- as.character(e$age)
# 이렇게 바꿔야 원하는 결과를 얻을 수 있다. numeric으로 바꾸지 말자1
e
g<-ifelse(e$age>25, 1, 0)
g
str(e)

i <- ifelse(e$age>25, 'blue', 'black')
i

h <- cbind(e, g)
h

# 괄호 사용법
## 소괄호
a<-matrix(c('Blue Jays', 'Giants', 'Dodgers', 0.268, 0.267, 0.250, 232, 136, 187, 891, 696, 667), ncol=4)
colnames(a)<-c('Team', 'AVG', 'HR', 'R')
a

b<-matrix(c('Clubs', 'Dodger', 'Pirates', 3.36, 3.44, 3.21, 134, 145, 110, 407, 395, 453), ncol=4)
colnames(b)<-c('Team', 'ERA', 'HRA', 'BBA')
b
c<-merge(a, b, by='Team')
c
d<-cbind(a, b)
d

## 중괄호 : 함수 만들 때 사용
fnc<-function(H, AB){H/AB}
fnc(10, 35)
fnc(17, 55)
fnc(14, 57)
## 대괄호
a<-matrix(c('Blue Jays', 'Giants', 'Dodgers', 0.269, 0.267, 0.250, 232, 136, 187, 891, 696, 667, 3.80, 3.72, 3.44, 173, 155, 145, 397, 431, 395), ncol=7)
colnames(a)<-c('Team', 'AVG', 'HR', 'R', 'ERA', 'HRA', 'BBA')
a
mode(a)
str(a)
#b<-a[a$Team=='Giants',]
#상위 코드 실행 안됨. data.frame으로 변환후 해보면 된다. 

b<-a[2,]
a<-data.frame(a)
str(a)
mode(a)
b<-a[a$Team=='Giants',]
b

str(Pitching)
c<-Pitching[Pitching$playerID=='kershcl01',]
c
dim(c)

d<-na.omit(c)
d
dim(d)
# 결측치 없다!

# 조건문 사용하기
library(Lahman)
View(Master)

a<-subset(Batting, playerID == 'altuvjo01'|playerID =='zobribe01')
a
dim(a)

b<-subset(a, yearID>2011&yearID<2017)
b
dim(b)

c<-subset(b, !(yearID==2014|yearID==2015))
c
dim(c)

d<-subset(c, select=c('playerID', 'HR', 'X3B'))
d
dim(d)

# 계속 사용할 테이블 고정하기
#attach(테이블명)
attach(Batting)

#attach(Team_2015)
#detach(Team_2015)