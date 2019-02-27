
# CH 01
## 01-01
name <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I')
HR <-c(9, 4, 13, 8, 1, 22,24, 11, 21)
AVG <- c(0.311, 0.283, 0.27, 0.268, 0.264, 0.258, 0.256, 0.255)
pit <- cbind(name, HR, AVG)
pit
mean(AVG)
median(AVG)

summary(HR)

var(HR)

sqrt(var(HR))

## 01-02
#install.packages('RMySQL')
#install.packages('dbConnect')
library(RMySQL)
library(dbConnect)
a = dbConnect(MySQL(), user='root',
              password='qwer1234', dbname='shop', host = 'localhost')
b <- "select*from list;"
c<-dbGetQuery(a,b)
c

## 01-03
#install.packages('Lahman')
library(Lahman)
dat <- subset(Teams, yearID==2016, select=c(R, H))
cor(dat$H, dat$R)
plot(dat$H, dat$R)


## 01-04
library(shiny)
ui = shinyUI(fluidPage(
  titlePanel(h2("Correlation between Hit and Run Scored", align="center")),
  sidebarLayout(
    sidebarPanel(
      selectInput("var1", "select X variable", choices=c("H"=1, "R"=2, "ERA"=3)),
      selectInput("var2", "select Y variable", choices=c("H"=1, "R"=2, "ERA"=3))
    ),
    mainPanel(h4("The correlation coefficient between the two variables is"),
              textOutput("correlation"), br(),
              h4("Scatterplot between the two variables"),
              plotOutput("plot"))
  )
))

library(Lahman)
dat<-subset(Teams, yearID==2016, select=c(H, R, ERA))
server = shinyServer(
  function(input, output)({
    x <- reactive({dat[, as.numeric(input$var1)]})
    y <- reactive({dat[, as.numeric(input$var2)]})
    output$correlation <- renderPrint({cor(x(), y())})
    output$plot <- renderPlot(({plot(x(), y())}))
  })
)

shinyApp(ui=ui, server=server)

install.packages('rsconnect')

rsconnect::setAccountInfo(name='datahsh', token='1C07483A51A9D34B20CA086056C08762', secret='0nWafhEqxAYAYQW4K/Jjp15ky2tiU8tOaYlex+vA')

rsconnect::deployApp('C:/Users/ktm/0226_0408_study/baseball01')

## https://ibuyworld.shinyapps.io/correlation/