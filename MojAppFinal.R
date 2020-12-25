library(shiny)
library(RCurl)
library(jsonlite)
library(digest)
library(anytime)
library(dplyr)
#library(signal)
#devtools::install_github("Yang-Tang/shinyjqui")
library(rhandsontable)
library(shinyjqui)
require(XML)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(V8)
library(shinycssloaders)
library(ggplot2)
library(knitr)
library(Matrix)
library(QRM)
library(dplyr)
source("slike.r")
source("BootStrap.R")
source("Eval.R")
#memory.limit(4090)

#load("data.RDATA",envir = globalenv())

history <- read.csv("data/hist.csv",stringsAsFactors = F)
history$TIME <- as.Date(history$TIME)
history[,2:ncol(history)] <- apply(history[,2:ncol(history)],2,as.numeric)
# tec <- xlsx::read.xlsx("tec.xlsx",1,header = T,encoding = "UTF-8")
# tec <- tec[tec$Label %in% colnames(history),1:3]
tec <- read.csv("tec.csv",stringsAsFactors = F)

LastTry <- as.Date(read.csv("data/LT.csv")[1,1])

# setwd("rmd")
# rmdfiles <- list.files(pattern = ".Rmd")
# for(i in rmdfiles){
#   knit(i)
# }
# setwd("..")

names1 <- colnames(history)[2:27]
DF <- matrix(NA,ncol=12,nrow=length(names1))
DF[,1] <- names1 
DF <- data.frame(DF,stringsAsFactors = F)
rowDF <- c("Currency","Type","Face Value","Strike","Premium","Roll. Vol","R.V. alpha","Smooth. factor","Time in days","Quantile","Copula","Simulations")
colnames(DF) <- rowDF




# UI ----------------------------------------------------------------------



ui <- function(req){
  dashboardPage(
    dashboardHeader(title = "Currency Options"
    ),
    dashboardSidebar(
      sidebarMenu(id = "www",
                  menuItem("Login", tabName = "login", icon = icon("user")),
                  menuItem("Theory & Help", tabName = "theory", icon = icon("fas fa-graduation-cap")),
                  menuItem("Data", tabName = "refresh", icon = icon("wrench")),
                  menuItem("Parameters", tabName = "params", icon = icon("codepen")),
                  menuItem("Simulations", tabName = "results", icon = icon("bar-chart-o")),
                  menuItem("Export", tabName = "export", icon = icon("truck")),
                  menuItem("Users", tabName = "source", icon = icon("user-secret")),
                  menuItem("Logout", tabName = "out", icon = icon("sign-out-alt"))
      ),collapsed = F,
      tags$head(
        tags$script(
          HTML(#code for hiding sidebar tabs 
            "Shiny.addCustomMessageHandler('manipulateMenuItem1', function(message)
          {
          var aNodeList = document.getElementsByTagName('a');
          
          for (var i = 0; i < aNodeList.length; i++) 
          {
          if(aNodeList[i].getAttribute('data-toggle') == message.toggle && aNodeList[i].getAttribute('role') == message.role) 
          {
          if(message.action == 'hide')
          {
          aNodeList[i].setAttribute('style', 'display: none;');
          } 
          else 
          {
          aNodeList[i].setAttribute('style', 'display: block;');
          };
          };
          }
          });"
          )
        )
      )
    ),
    dashboardBody(
      useShinyjs(),
      shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
      tags$head(tags$style(HTML('
                              @import url("//fonts.googleapis.com/css?family=Lobster|Cabin:400,700");
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #003333;
                              color: #E6E6FA;
                              font-family: Lobster, cursive;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #003333;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #003333;
                              }        
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #003333;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #17C36D;
                              }
                              
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color:  #003333;
                              color: #FFF0F5;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #17C36D;
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color:#17C36D;
                              }
                              
                              '))
      ),
      
      tabItems(
        tabItem(tabName = "login",
                navbarPage(icon("user"),
                           tabPanel("Informations",
                                    column(12,align="center",
                                           br(),
                                           h1("Welcome!",style="color:maroon"),
                                           br(),
                                           h3("Author: Jakob Jelenčič"),
                                           br(),
                                           h3("Email: jakob.jelencic1@gmail.com"),
                                           br()
                                    )
                           )
                )
        ),
        tabItem(tabName = "theory",
                navbarPage(icon("graduation-cap"),
                           tabPanel("Bootstrap and Copulas",
                                    h3("Detailed sources:",style="color:maroon"),
                                    actionLink("link1","Method's source:",onclick ="window.open('https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:32017R0653&from=EN', '_blank')"),
                                    br(),
                                    actionLink("link2","Copula's theory:",onclick ="window.open('https://en.wikipedia.org/wiki/Copula_(probability_theory)', '_blank')"),
                                    br(),
                                    # actionLink("link3","Comparission of Gaussian and Student copula (in Slovene):"),
                                    # br(),
                                    actionLink("link4","What are currency options?",onclick ="window.open('https://www.investopedia.com/terms/c/currencyoption.asp', '_blank')"),
                                    br(),
                                    withMathJax(includeMarkdown("rmd/theory.md")),
                                    br()
                           ),
                           tabPanel("Detailed example and Help",
                                    withMathJax(includeMarkdown("rmd/example.md"))
                           )
                )
        ),
        tabItem(tabName = "refresh",
                navbarPage(icon("wrench"),
                           tabPanel("Refresh",
                                    column(12, align="center",
                                           br(),
                                           hr(),
                                           h2("Data range (daily data):",style="color:maroon"),
                                           h3("First day:"),
                                           h3(as.Date("2007-1-1")),
                                           br(),
                                           h3("Last day:"),
                                           h3(textOutput("lastDay")),
                                           br(),
                                           hr(),
                                           br(),
                                           actionButton("ref","Collect new data",icon=icon("refresh")),
                                           br(),
                                           helpText("On button press you will collect new data from Bank of Slovenia's official site. It may take 
                                                  couple of minutes (ETA: 1-2 min). \n "),
                                           br(),
                                           br()
                                           
                                           
                                    )
                           ),
                           tabPanel("Current data",
                                    h2("Part 1:",style="color:maroon"),
                                    br(),
                                    DT::dataTableOutput("head"),
                                    br(),
                                    h2("Part 2:",style="color:maroon"),
                                    br(),
                                    DT::dataTableOutput("tail")
                           ),
                           tabPanel("Visualization and legend",
                                    column(12,align="center",
                                           h3("Pick currency:",style="color:maroon"),
                                           selectizeInput("plot1","",choices = names1,selected = "USD"),
                                           br(),
                                           plotOutput("plot"),
                                           DT::dataTableOutput("legend"),
                                           br()
                                           
                                    )
                           )
                )
        ),
        tabItem(tabName = "params",
                navbarPage(icon("codepen"),
                           tabPanel("Settings:",id="ss",
                                    column(12,align="center",
                                           h3("Choose your Currencies:",style="color:maroon"),
                                           selectInput("currency","",multiple=T,choices=colnames(history)[2:ncol(history)],selected=c()),#"USD","GBP","JPY")),
                                           helpText("For each of selected currencies you have to fill out the form that will appear bellow. 
                                                  Afterwards please confirm your choices with button click bellow."),
                                           br()
                                    ),
                                    
                                    
                                    # Parametri ---------------------------------------------------------------
                                    
                                    
                                    column(4,align="center",id="USD",
                                           h3("USD:"),
                                           selectInput("tipUSD","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceUSD","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KUSD","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preUSD","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVUSD","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaUSD","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STUSD","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="JPY",
                                           h3("JPY:"),
                                           selectInput("tipJPY","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceJPY","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KJPY","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preJPY","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVJPY","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                higher volatilty of TS also click yes."),
                                           sliderInput("RValphaJPY","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STJPY","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="CZK",
                                           h3("CZK:"),
                                           selectInput("tipCZK","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceCZK","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KCZK","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preCZK","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVCZK","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaCZK","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STCZK","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="DKK",
                                           h3("DKK:"),
                                           selectInput("tipDKK","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceDKK","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KDKK","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preDKK","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVDKK","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaDKK","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STDKK","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="GBP",
                                           h3("GBP:"),
                                           selectInput("tipGBP","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceGBP","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KGBP","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preGBP","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVGBP","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaGBP","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STGBP","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="HUF",
                                           h3("HUF:"),
                                           selectInput("tipHUF","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceHUF","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KHUF","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preHUF","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVHUF","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaHUF","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STHUF","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="PLN",
                                           h3("PLN:"),
                                           selectInput("tipPLN","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FacePLN","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KPLN","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("prePLN","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVPLN","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaPLN","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STPLN","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="RON",
                                           h3("RON:"),
                                           selectInput("tipRON","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceRON","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KRON","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preRON","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVRON","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaRON","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STRON","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="SEK",
                                           h3("SEK:"),
                                           selectInput("tipSEK","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceSEK","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KSEK","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preSEK","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVSEK","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaSEK","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STSEK","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="CHF",
                                           h3("CHF:"),
                                           selectInput("tipCHF","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceCHF","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KCHF","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preCHF","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVCHF","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaCHF","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STCHF","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="NOK",
                                           h3("NOK:"),
                                           selectInput("tipNOK","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceNOK","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KNOK","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preNOK","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVNOK","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaNOK","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STNOK","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="HRK",
                                           h3("HRK:"),
                                           selectInput("tipHRK","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceHRK","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KHRK","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preHRK","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVHRK","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaHRK","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STHRK","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="RUB",
                                           h3("RUB:"),
                                           selectInput("tipRUB","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceRUB","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KRUB","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preRUB","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVRUB","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaRUB","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STRUB","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="TRY",
                                           h3("TRY:"),
                                           selectInput("tipTRY","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceTRY","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KTRY","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preTRY","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVTRY","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaTRY","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STTRY","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="AUD",
                                           h3("AUD:"),
                                           selectInput("tipAUD","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceAUD","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KAUD","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preAUD","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVAUD","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaAUD","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STAUD","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="CAD",
                                           h3("CAD:"),
                                           selectInput("tipCAD","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceCAD","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KCAD","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preCAD","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVCAD","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaCAD","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STCAD","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="CNY",
                                           h3("CNY:"),
                                           selectInput("tipCNY","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceCNY","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KCNY","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preCNY","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVCNY","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaCNY","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STCNY","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="HKD",
                                           h3("HKD:"),
                                           selectInput("tipHKD","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceHKD","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KHKD","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preHKD","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVHKD","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaHKD","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STHKD","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="IDR",
                                           h3("IDR:"),
                                           selectInput("tipIDR","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceIDR","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KIDR","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preIDR","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVIDR","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaIDR","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STIDR","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="KRW",
                                           h3("KRW:"),
                                           selectInput("tipKRW","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceKRW","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KKRW","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preKRW","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVKRW","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaKRW","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STKRW","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")),
                                    column(4,align="center",id="MYR",
                                           h3("MYR:"),
                                           selectInput("tipMYR","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceMYR","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KMYR","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preMYR","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVMYR","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaMYR","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STMYR","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="NZD",
                                           h3("NZD:"),
                                           selectInput("tipNZD","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceNZD","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KNZD","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preNZD","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVNZD","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaNZD","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STNZD","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="PHP",
                                           h3("PHP:"),
                                           selectInput("tipPHP","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FacePHP","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KPHP","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("prePHP","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVPHP","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaPHP","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STPHP","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="SGD",
                                           h3("SGD:"),
                                           selectInput("tipSGD","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceSGD","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KSGD","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preSGD","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVSGD","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaSGD","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STSGD","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="THB",
                                           h3("THB:"),
                                           selectInput("tipTHB","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceTHB","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KTHB","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preTHB","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVTHB","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaTHB","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STTHB","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    ),
                                    column(4,align="center",id="ZAR",
                                           h3("ZAR:"),
                                           selectInput("tipZAR","Option type:", c("Call","Put"),selected="Call"),
                                           numericInput("FaceZAR","Face value:",10000, min=0, max=1000000000, step=1000),
                                           numericInput("KZAR","Strike value:",10100, min=0, max=1000000000, step=100),
                                           numericInput("preZAR","Premium:",100, min=0, max=1000000000, step=10),
                                           br(),
                                           h4("Additional settings:"),
                                           selectInput("RVZAR","Would you like to apply rolling vollatilty?", c("Yes","No"),selected="No"),
                                           helpText("If your calculations are for the purpose of stress testing, click yes. If you expect 
                                                  higher volatilty of TS also click yes."),
                                           sliderInput("RValphaZAR","What alpha should be used for rolling vollatilty?",min = 0.51,max=0.99,step = 0.01,value = 0.8),
                                           helpText("Default is 0.8"),
                                           sliderInput("STZAR","Are recent events more important than thoose from long ago?",min = 0.975,max=1,step = 0.001,value = 1),
                                           helpText("Here you can control the smoothing factor for exponentionally diminishing importance of events from long ago. For uniform 
                                                  probability leave this to 1. If you want to be extremly focsued on recents events put it on 0.975.")
                                    )
                                    
                                    
                                    # Konec paramtrov ---------------------------------------------------------
                                    ,
                                    br(),
                                    column(12,align="center",
                                           h2("Parameters below apply for whole portfolio:",style="color:maroon"),
                                           br(),
                                           h3("Time period in days:",style="color:maroon"),
                                           numericInput("t","",256, min=1, max=30000, step=1),
                                           helpText("1 year equals 256 work days. 256 means that you are predicting 
                                                  value of your portfolio on 256th bussines day from today."),
                                           h3("Choose the final quantile:",style="color:maroon"),
                                           sliderInput("Alpha","",min=0.001,max=0.999,step=0.001,value=0.500),
                                           helpText("Quantile determines your view on the future. If you expect extremly bad development, choose something around 0.01, for 
                                                  average prediction 0.5 and for optimistic scenario something higher than 0.8. For optimistic scenario I do not recommend rolling volatilty."),
                                           br(),
                                           h3("Which copula will you use?",style="color:maroon"),
                                           selectInput("kopula","",choices = c("Gaussian","Student"),selected = "Student"),
                                           helpText("If you are predicting extreme events, use Student's copula (3 degrees of freedom). If you chose just one currency then 
                                                  copula is irrelevant."),
                                           br(),
                                           h3("Number of simulations:",style="color:maroon"),
                                           sliderInput("number","",min=25000,value=100000,max=200000,step=1000),
                                           helpText("Minimum is 25 000 simulations. I recommened 100 000 or higher. Take into the account 
                                                  server's hardware limitations."),
                                           br(),
                                           h3("Confirm the choice of parameters:",style="color:maroon"),
                                           br(),
                                           actionButton("goo","Please confirm your choice",icon=icon("fas fa-arrow-circle-up")),
                                           br(),
                                           helpText("You will be redirected to computation page."),
                                           br())
                                    
                           )
                )
        ),
        
        # Results -----------------------------------------------------------------
        
        
        tabItem(tabName = "results",
                navbarPage(icon("bar-chart-o"),
                           tabPanel("Params",
                                    column(12,align="center",
                                           h3("Start computation:",style="color:maroon"),
                                           actionButton("check",label=h3(icon("fas fa-arrow-circle-up")),width = "75px"),
                                           br(),
                                           h3("Selected parameters:",style="color:maroon"),
                                           dataTableOutput("pdf")
                                    )
                           ),
                           tabPanel("Result",
                                    column(12,align="center",id="pre",
                                           h3("Please select your parameters.")),
                                    
                                    column(12,align="center",id="FINALCOL",
                                           h3("Result:",style="color:maroon"),
                                           br(),
                                           withSpinner(
                                             DT::dataTableOutput("final"),7,"black"),
                                           br(),
                                           h3("Change quantile:",style="color:maroon"),
                                           sliderInput("kvant2","",min=0.0001,max=1,step=0.001,value=0.5),
                                           br(),
                                           h3("Options Portfolio Density:",style="color:maroon"),
                                           withSpinner(
                                             plotOutput("finalValue"),7,"black"),
                                           br(),
                                           h3("Futures Portfolio Density:",style="color:maroon"),
                                           withSpinner(
                                             plotOutput("fut"),7,"black")
                                    )
                           ),
                           tabPanel("Raw simulations",
                                    column(12,align="center",id="SIM11",
                                           h3("Simulation matrix:",style="color:maroon"),
                                           selectizeInput("filter","Displayed:",choices=c("Wait"="USD"),selected="USD",multiple=T),
                                           withSpinner(
                                             DT::dataTableOutput("SIM"),7,"black")
                                    )
                           ),
                           tabPanel("Evaluated simulations",
                                    column(12,align="center",id="SIM211",
                                           h3("Portfolio values:",style="color:maroon"),
                                           withSpinner(
                                             DT::dataTableOutput("SIM2"),7,"black")
                                    )
                           )
                )
        ),
        
        # export ------------------------------------------------------------------
        
        
        tabItem(tabName = "export",
                navbarPage(icon("truck"),
                           tabPanel("Download Report",
                                    column(12,align="center",
                                           br(),
                                           h2("Download HTML:",style="color:maroon"),
                                           br(),
                                           h3("Included:"),
                                           br(),
                                           h4("- Parameters data frame."),
                                           br(),
                                           h4("- History trajectories of chosen currencies."),
                                           br(),
                                           h4("- Empirical densities of portfolio returns (Options and Futures)."),
                                           br(),
                                           h4("- Data frame with results."),
                                           br(),
                                           downloadButton("report",label="Download",width="100px")
                                    )
                                    
                           )
                )
        ),
        tabItem(tabName = "source",
                navbarPage(icon("user-secret"),
                           tabPanel("Users",
                                    column(12,align="center",
                                           br(),
                                           actionButton("edit","Edit",icon("edit")),
                                           actionButton("save","Save",icon("save")),
                                           br(),
                                           h4(textOutput("info")),
                                           br()
                                    ),
                                    column(12,align="left",id="hot1",
                                           #selectInput("hotSelect","Filter:",choices = cho,multiple = F,selected=cho[1:10]),
                                           withSpinner(
                                             rHandsontableOutput("hot"),7,"black"),
                                           br()),
                                    column(12,align="center",id="hot2",
                                           dataTableOutput("dff"),
                                           br()),
                                    fluidPage(
                                      div(style = "display: none;",
                                          textInput("remote_addr", "remote_addr",
                                                    if (!is.null(req[["HTTP_X_FORWARDED_FOR"]]))
                                                      req[["HTTP_X_FORWARDED_FOR"]]
                                                    else
                                                      req[["REMOTE_ADDR"]]
                                          )
                                      )
                                    )
                           )
                )
        ),
        
        # logout ------------------------------------------------------------------
        
        
        tabItem(tabName = "out",
                navbarPage(icon("sign-out-alt"),
                           tabPanel("LogOut",
                                    column(12,align="center",
                                           br(),
                                           h4("Logged in as:",style="color:maroon"),
                                           h4(textOutput("kdoseeem")),
                                           br(),
                                           actionButton("logout","LogOut",icon=icon("sign-out-alt"))
                                    )
                           )
                )
        )
        
        
      )
      
    )
    
    
  )
}


# server ------------------------------------------------------------------


server <- function(input, output,session){ 
  
  
  # login -------------------------------------------------------------------
  
  values <- reactiveValues(authenticated = FALSE)
  
  # Return the UI for a modal dialog with data selection input. If 'failed'
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    shinyjs::toggle(selector = "ul li:eq(1)", anim = TRUE)
    shinyjs::toggle(selector = "ul li:eq(2)", anim = T)
    shinyjs::toggle(selector = "ul li:eq(3)", anim = T)
    shinyjs::toggle(selector = "ul li:eq(4)", anim = T)
    shinyjs::toggle(selector = "ul li:eq(5)", anim = T)
    shinyjs::toggle(selector = "ul li:eq(6)", anim = T)
    shinyjs::toggle(selector = "ul li:eq(7)", anim = T)
    #shinyjs::toggle(selector = "ul li:eq(8)", anim = T)
    modalDialog(
      column(12,align="center",id="logs",
             br(),
             textInput("username", "Username:"),
             br(),
             passwordInput("password", "Password:"),
             br(),
             tags$img(src = base64enc::dataURI(file = "www/sample_3.jpg", mime = "image/jpeg"))),
      footer = tagList(
        # modalButton("Cancel"),
        column(12,align="center",
               actionButton("ok", "OK",icon=icon("fas fa-check-circle")))
        
      ),
      fade = T,
      size = "m"
    )
  }
  
  # Show modal when button is clicked.
  # This `observe` is suspended only whith right user credential
  
  obs1 <- observe({
    IP <- read.csv("data/ip.csv",stringsAsFactors = F,encoding="UTF-8")
    hide("pdf")
    hide("FINALCOL")
    hide("SIM11")
    hide("SIM211")
    disable("check")
    disable("report")
    ip <- isolate(input$remote_addr)[1]
    ip <- as.character(ip)
    ip <- strsplit(ip,",")[[1]][1]
    if(is.null(ip)){ip <- "nope"}
    if(ip %in% IP$IP){
      kdo11 <- which(ip == IP$IP)
      tip <- IP$TYPE[kdo11]
      if(IP$TYPE[kdo11]=="normal"){
        shinyjs::toggle(selector = "ul li:eq(6)", anim = T)
        #shinyjs::toggle(selector = "ul li:eq(7)", anim = T)
      }
      #print(IP)
      kdo$sem <- IP$USERNAME[kdo11]
      obs1$suspend()
      obs2$suspend()
      obs3$suspend()
    }else{
      showModal(dataModal())
      jqui_effect('#logs',effect="bounce",duration = 1)
      #jqui_effect('#logs',effect="bounce",duration = 1)
    }
    #print(input$remote_addr)
  })
  
  # When OK button is pressed, attempt to authenticate. If successful,
  # remove the modal.
  IPtable <- reactiveValues(IP=read.csv("data/ip.csv",stringsAsFactors = F,encoding="UTF-8"))
  
  obs2 <- observeEvent(input$ok,{
    IP <- read.csv("data/ip.csv",stringsAsFactors = F,encoding="UTF-8")
    #req(input$ok)
    #jqui_effect('#logs',effect="shake")
    isolate({
      Username <- input$username
      Password <- input$password
    })
    #jqui_effect('password',effect="shake")
    Id.username <- which(IP$USERNAME == Username)
    Id.password <- which(IP$PASSWORD == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        
        ## updejt IP
        ip <- isolate(input$remote_addr)[1]
        ip <- as.character(ip)
        ip <- strsplit(ip,",")[[1]][1]
        
        if(is.null(ip)){ip <- "unknown"}
        if(ip==""){ip <- "unknown"}
        
        cleanup <- which(IP$IP==ip)
        if(length(cleanup)>0){
          IP$IP[cleanup] <- "unknown"
        }
        IP$IP[Id.username] <- ip
        IP$LAST_LOGIN[Id.username] <- as.character(Sys.time())
        IPtable$IP <- IP
        kdo$sem <- IP$USERNAME[Id.username]
        write.csv(IP,"data/ip.csv",row.names = F,fileEncoding = "UTF-8")
        
        
        values$authenticated <- TRUE
        values1$time <-  as.numeric(Sys.time())
        obs1$suspend()
        shinyjs::toggle(selector = "ul li:eq(1)", anim = TRUE)
        shinyjs::toggle(selector = "ul li:eq(2)", anim = T)
        shinyjs::toggle(selector = "ul li:eq(3)", anim = T)
        shinyjs::toggle(selector = "ul li:eq(4)", anim = T)
        shinyjs::toggle(selector = "ul li:eq(5)", anim = T)
        shinyjs::toggle(selector = "ul li:eq(7)", anim = T)
        if(IP$TYPE[Id.password]=="admin"){
          shinyjs::toggle(selector = "ul li:eq(6)", anim = TRUE)
          #shinyjs::toggle(selector = "ul li:eq(7)", anim = TRUE)
        }
        
        #removeModal()
        showModal({
          modalDialog(
            fluidRow(column(12,align="center",
                            br(),
                            h2("Welcome!",style="color:maroon"),
                            br(),
                            h4("Please read the example for better understanding. Thank you."),
                            br(),
                            tags$img(src = base64enc::dataURI(file = "www/sample_3.jpg", mime = "image/jpeg")),
                            br())),
            easyClose = F,
            footer=column(12,align="center",
                          actionButton("go1","Close",icon=icon("user-secret")))
          )
        })
        
      } else {
        values$authenticated <- FALSE
        jqui_effect('#logs',effect="shake")
        updateTextInput(session,"username",value="")
        updateTextInput(session,"password",value = "")
      }
    }else{
      jqui_effect('#logs',effect="shake")
      updateTextInput(session,"username",value="")
      updateTextInput(session,"password",value = "")
    }
  })
  
  values1 <- reactiveValues(time= 0)
  autoInvalidate <- reactiveTimer(1000)
  
  obs3 <- observe({
    autoInvalidate()
    if(values$authenticated &  as.numeric(Sys.time()) - values1$time > 4){
      updateTabsetPanel(session, "www",
                        selected ="theory")
      removeModal()
      obs3$suspend()
    }
  })
  
  observeEvent(input$go1,{
    #newtab <- switch(input$tabs, "log" = "intro","two" = "one")
    updateTabsetPanel(session, "www",
                      selected ="theory")
    removeModal()
    obs3$suspend()
  })
  
  
  # theory ------------------------------------------------------------------
  observeEvent(input$link1,{
    #browseURL("https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:32017R0653&from=EN ")
  })
  
  observeEvent(input$link2,{
    #browseURL("https://en.wikipedia.org/wiki/Copula_(probability_theory)")
  })
  
  # observeEvent(input$link3,{
  #   browseURL("kopule.html")
  # })
  
  observeEvent(input$link4,{
    #browseURL("https://www.investopedia.com/terms/c/currencyoption.asp")
  })
  
  # parameters --------------------------------------------------------------
  for(z in 2:ncol(history)){
    zi <- colnames(history)[z]
    hide(id=paste0(zi))
  }
  
  observeEvent(input$currency,{
    #print(input$currency)
    for(z in input$currency){
      shinyjs::show(z,anim = T,time = 1)
    }
  })
  
  observe({
    for(z in setdiff(names1,input$currency)){
      shinyjs::hide(id=z,anim = T,time = 1)
    }
  })
  
  param <- reactiveValues(DF=DF)
  
  output$pdf <- renderDataTable({
    param$DF
  }
  )
  
  observeEvent(input$goo,{
    
    if(!is.null(input$currency)){
      ind <- names1 %in% input$currency
      kje <- which(ind)
      param$DF <- DF[ind,]
      colnames(param$DF) <- rowDF
      blabla <- c("tip","Face","K","pre","RV","RValpha","ST")
      param$DF[,9] <- c(input$t)
      param$DF[,10] <- c(input$Alpha)
      param$DF[,11] <- c(input$kopula)
      param$DF[,12] <- c(input$number)
      for(j in 1:length(kje)){
        kaj <- input$currency[j]
        for(zw in 2:(length(blabla)+1)){
          param$DF[j,zw] <- eval(parse(text=paste0("input$",blabla[zw-1],kaj)))
        }
      }
      
      shinyjs::show("pdf")
      shinyjs::enable("check")
      updateTabsetPanel(session, "www",
                        selected ="results")
      updateSliderInput(session,"kvant2",value=input$Alpha)
      # shinyjs::show("FINALCOL")
      # shinyjs::show("SIM")
      # shinyjs::show("SIM2")
      # shinyjs::hide("pre")
      
      showModal({
        modalDialog(
          title=h2(icona_narobe),
          fluidRow(column(12,align="center",
                          br(),
                          h4("You have been redirected to the results page."),
                          br(),
                          h4("Please double check parameters at first tab and start the computation."),
                          br(),
                          h4("After that you can export the results in HTML output."),
                          br(),
                          tags$img(src = base64enc::dataURI(file = "www/sample_3.jpg", mime = "image/jpeg")),
                          br())),
          easyClose = T,
          size = "m",
          footer=column(12,align="center",
                        modalButton("Close",icon=icon("fas fa-check-circle")))
        )
      })
    }else{
      showModal({
        modalDialog(
          title=h2(icona_narobe),
          fluidRow(column(12,align="center",
                          br(),
                          h4("Please choose at least one currency."),
                          br(),
                          tags$img(src = base64enc::dataURI(file = "www/sample_3.jpg", mime = "image/jpeg")),
                          br())),
          easyClose = T,
          size = "m",
          footer=column(12,align="center",
                        modalButton("Close",icon=icon("fas fa-check-circle")))
        )
      })
    }
    #trigger$i <- T
    ##go calculation, naredi df parametrov in preusmeri na result.
  })
  
  trigger <- reactiveValues(i=F)
  
  observeEvent(input$check,{
    
    trigger$i <- T
    
    showModal({
      modalDialog(
        title=h2(icona_narobe),
        fluidRow(column(12,align="center",
                        br(),
                        h4("Simulation may take up to 3 minutes."),
                        br(),
                        h4("Time is increasing with number 
                           of simulations, time period and number of currencies."),
                        br(),
                        h4("You can view the results at the result tab."),
                        br(),
                        tags$img(src = base64enc::dataURI(file = "www/sample_3.jpg", mime = "image/jpeg")),
                        br())),
        easyClose = T,
        size = "m",
        footer=column(12,align="center",
                      modalButton("Close",icon=icon("fas fa-check-circle")))
      )
    })
    
  })
  
  observe({
    if(trigger$i){
      try({
        
        err <- T
        
        
        updateSliderInput(session,"kvant2",value=input$Alpha)
        updateSelectizeInput(session,"filter",choices=input$currency,selected=input$currency[1:min(10,length(input$currency))],options=list(multiple = T))
        shinyjs::show("FINALCOL")
        # shinyjs::show("SIM11")
        # shinyjs::show("SIM211")
        shinyjs::hide("pre")
        
        ind <- names1 %in% input$currency
        kje <- which(ind)
        data <- as.matrix(hist$hist[,kje+1])
        beta <- c(param$DF[,7]) %>% as.numeric()
        Percent <- c(param$DF[,8]) %>% as.numeric()
        STRES <- c(param$DF[,6])=="Yes"
        St.Simulacij <- param$DF[1,12] %>% as.numeric()
        St.Dni.Vnaprej <- param$DF[1,9] %>% as.numeric()
        Student <- param$DF[1,11]=="Student"
        
        #print(head(data))
        
        sim$sim <- BootStrap(
          data=data,
          beta=beta,
          Percent = Percent,
          STRES = STRES,
          St.Simulacij = St.Simulacij,
          St.Dni.Vnaprej = St.Dni.Vnaprej,
          Student = Student,
          norm = T
        )$Results
        #print(class(sim$sim))
        #print(dim(sim$sim))
        colnames(sim$sim) <- colnames(hist$hist)[kje+1]
        
        Face <- param$DF[,3] %>% as.numeric()
        Type <- param$DF[,2]
        Premium <- param$DF[,5] %>% as.numeric()
        Strike <- param$DF[,4] %>% as.numeric()
        
        tmp <- Eval(simulation = sim$sim,
                    Face = Face,
                    Strike = Strike,
                    Premium = Premium,
                    Type =  Type   )
        
        #print(dim(sim$sim))
        MojResult$final <- tmp$O
        Fair$Value <- tmp$Fu
        removeModal()
        
        showModal({
          modalDialog(
            title=h2(icona_narobe),
            fluidRow(column(12,align="center",
                            br(),
                            h2("Done!"),
                            br(),
                            h4("You can view the results now."),
                            br(),
                            tags$img(src = base64enc::dataURI(file = "www/sample_3.jpg", mime = "image/jpeg")),
                            br())),
            easyClose = T,
            size = "m",
            footer=column(12,align="center",
                          modalButton("Close",icon=icon("fas fa-check-circle")))
          )
        })
        shinyjs::show("SIM11")
        shinyjs::show("SIM211")
        enable("report")
        err <- F
        trigger$i <- F
      },silent = F)
      if(err){
        trigger$i <- F
        shinyjs::hide("FINALCOL")
        shinyjs::hide("SIM11")
        shinyjs::hide("SIM211")
        shinyjs::show("pre")
        disable("check")
        
        showModal({
          modalDialog(
            title=h2(icona_narobe),
            fluidRow(column(12,align="center",
                            br(),
                            h4("Error!"),
                            br(),
                            h4("You probably ran out of memory. Please reduce the number of simulations or days ahead."),
                            br(),
                            tags$img(src = base64enc::dataURI(file = "www/sample_3.jpg", mime = "image/jpeg")),
                            br())),
            easyClose = T,
            size = "m",
            footer=column(12,align="center",
                          modalButton("Close",icon=icon("fas fa-check-circle")))
          )
        })
        
      }
    }
  })
  
  sim <- reactiveValues(sim=history)
  Fair <- reactiveValues(Value=c(1,2,3))
  # Calculation -------------------------------------------------------------
  MojResult <- reactiveValues(final=c(1,2,3))
  
  output$final <- DT::renderDataTable({
    ## value today
    xx <- c("Invested Money:",
            "Paid premium:",
            "Total Cost (for options):",
            "Expected portfolio value for options (E[X]):",
            "Predicted portfolio payoff for options:",
            "Relative change in % (options only):",
            "Expected portfolio value for futures (E[X]):",
            "Predicted portfolio value for futures (Premium is set to 0 for futures):")
    yy <- c(sum(as.numeric(param$DF[,3])),
            sum(as.numeric(param$DF[,5])),
            sum(as.numeric(param$DF[,3])) + sum(as.numeric(param$DF[,5])),
            mean(MojResult$final),
            as.numeric(quantile(MojResult$final,probs=input$kvant2)),
            0,
            mean(Fair$Value),
            as.numeric(quantile(Fair$Value,probs=input$kvant2))
    )
    ahm <- data.frame(X=xx,
                      Y=yy,
                      stringsAsFactors = F,
                      row.names = NULL)
    colnames(ahm) <- c("Variables:","Values:")
    ahm[6,2] <- (ahm[5,2] / ahm[3,2])*100
    ahm
  },
  options = list(
    rownames=FALSE)
  )
  
  observeEvent(input$kvant2,{
    param$DF[,10] <- input$kvant2
  })
  
  output$finalValue <- renderPlot({
    ha <- density(MojResult$final)
    l1 <- as.numeric(min(input$kvant2*length(ha$y),1))
    vup <- as.numeric(quantile(MojResult$final,probs=input$kvant2))
    
    g <- ggplot(data.frame(x=ha$x,y=ha$y), aes(x,y, fill="col")) +
      geom_line(aes(x=x,y=y),lwd=1.2,col="black") +
      xlab("Density of simulated portfolio returns (options):")
    
    d <- ggplot_build(g)$data[[1]]
    
    g <- g + geom_area(data = subset(d, x < vup), aes(x=x, y=y), fill="blue",alpha=0.35)
    g <- g + geom_point(data = subset(d, x < vup),
                        aes(x=tail(x,1),y=tail(y,1)),
                        color="darkred",lwd=2)
    g <- g + geom_segment(data=subset(d, x < vup),
                          aes(x=tail(x,1),xend=tail(x,1),y=0,yend=tail(y,1)),col="darkred",lwd=1)
    g <- g + geom_point(data = subset(d, x < vup),
                        aes(x=tail(x,1),y=0),
                        color="darkred",lwd=2) +
      theme(legend.position="none")
    g
    
  })
  
  output$fut <- renderPlot({
    ha <- density(Fair$Value)
    l1 <- as.numeric(min(input$kvant2*length(ha$y),1))
    vup <- as.numeric(quantile(Fair$Value,probs=input$kvant2))
    
    g <- ggplot(data.frame(x=ha$x,y=ha$y), aes(x,y, fill="col")) +
      geom_line(aes(x=x,y=y),lwd=1.2,col="black") +
      xlab("Density of simulated portfolio returns (futures):")
    
    d <- ggplot_build(g)$data[[1]]
    
    g <- g + geom_area(data = subset(d, x < vup), aes(x=x, y=y), fill="blue",alpha=0.35)
    g <- g + geom_point(data = subset(d, x < vup),
                        aes(x=tail(x,1),y=tail(y,1)),
                        color="darkred",lwd=2)
    g <- g + geom_segment(data=subset(d, x < vup),
                          aes(x=tail(x,1),xend=tail(x,1),y=0,yend=tail(y,1)),col="darkred",lwd=1)
    g <- g + geom_point(data = subset(d, x < vup),
                        aes(x=tail(x,1),y=0),
                        color="darkred",lwd=2)+
      theme(legend.position="none")
    g
    
  })
  
  # tables and plot ------------------------------------------------------------------
  
  # output$mojUs <- DT::renderDataTable({
  #   IPtable$IP
  # }, 
  # escape = FALSE,
  # options = list(
  #   lengthMenu = list(c(10, 100, -1), c("10",'100', 'All')),
  #   preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
  #   drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } ')),
  # rownames=T
  # )
  
  output$SIM <- DT::renderDataTable({
    a <-as.matrix(sim$sim[,input$filter])
    colnames(a) <- input$filter
    a
    
  }, 
  escape = FALSE,
  options = list(
    lengthMenu = list(c(10, 100, -1), c("10",'100', 'All')),
    preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } ')),
  rownames=T
  )
  
  output$SIM2 <- DT::renderDataTable({
    xx <- data.frame(`Portfolio Values`=MojResult$final)
    colnames(xx) <- "Portfolio Values"
    xx
  }, 
  escape = FALSE,
  options = list(
    lengthMenu = list(c(10, 100, -1), c("10",'100', 'All')),
    preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } ')),
  rownames=T
  )
  
  
  
  hist <- reactiveValues(hist=history)
  
  output$head <- DT::renderDataTable({
    hist$hist[,1:14]
    #print(input$currency)
  })  
  
  output$tail <- DT::renderDataTable({
    hist$hist[,c(1,15:27)]
  }, 
  escape = FALSE,
  options = list(
    lengthMenu = list(c(10, 15,20, -1), c("10",'15','20', 'All')),
    preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } ')),
  rownames=FALSE
  )
  
  output$plot <- renderPlot({
    x <- which(input$plot1 == names1) + 1
    y <- hist$hist[,1]
    x <- hist$hist[,x]
    ggplot(data=data.frame(Currency=x,Time=as.Date(y))) +
      #geom_line(aes(x=Time,y=sgolayfilt(Currency,p=3,n=101)),lwd=1,col="red")+
      geom_smooth(aes(x=Time,y=Currency),lwd=1,col="green")  +
      geom_line(aes(x=Time,y=Currency),lwd=1)  +
      ggtitle(paste0(input$plot1))
  })
  
  output$legend <- DT::renderDataTable({
    tec
  }, 
  escape = FALSE,
  options = list(
    lengthMenu = list(c(10, 15,20, -1), c("10",'15','20', 'All')),
    preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } ')),
  rownames=FALSE
  )
  
  
  LD <- reactiveValues(LD=tail(history$TIME,1))
  
  output$lastDay <- renderText(
    as.character(LD$LD)
  )
  
  
  # refresh -----------------------------------------------------------------
  lastTRY <- reactiveValues(i=LastTry)
  
  observeEvent(input$ref,{
    up <- T
    if(lastTRY$i >= Sys.Date()){
      up <- F
      showModal({
        modalDialog(
          title=h2(icona_narobe),
          fluidRow(column(12,align="center",
                          br(),
                          h4("You are up to date!"),
                          br(),
                          tags$img(src = base64enc::dataURI(file = "www/sample_3.jpg", mime = "image/jpeg")))),
          br(),
          
          easyClose = T,
          footer=column(12,align="center",modalButton("Close")),
          fade = T
        )
      })
    }
    
    
    if(up){
      showModal({
        modalDialog(
          title=h2(icona_narobe),
          fluidRow(column(12,align="center",
                          br(),
                          h4("Loading data. Please wait."),
                          br(),
                          h4("Estimated Time Left: 90 sec."),
                          br(),
                          tags$img(src = base64enc::dataURI(file = "www/sample_3.jpg", mime = "image/jpeg")))),
          br(),
          
          easyClose = F,
          footer=NULL
        )
      })
      
      
      
      download.file("https://www.bsi.si/_data/tecajnice/dtecbs-l.xml","tmp.xml")
      
      showModal({
        modalDialog(
          title=h2(icona_narobe),
          fluidRow(column(12,align="center",
                          br(),
                          h4("Reading data. Please wait."),
                          br(),
                          h4("Estimated Time Left: 90 sec."),
                          br(),
                          tags$img(src = base64enc::dataURI(file = "www/sample_3.jpg", mime = "image/jpeg")))),
          br(),
          
          easyClose = F,
          footer=NULL,fade = F
        )
      })
      
      data <- xmlParse("tmp.xml")
      xml_data <- xmlToList(data)
      withProgress(message = 'Processing:', value = 0, {
        x <- unlist(xml_data[[1]])
        
        df <- as.data.frame(x)
        df <- as.data.frame(t(df),stringsAsFactors = F)
        df <- df[,1:102]
        df <- df[,c(T,T,F)]
        ime <- df[,c(F,T)]
        valuta <- df[,c(T,F)]
        
        im <- c()
        for(i in 1:ncol(ime)){
          im <- c(im,as.character(ime[1,i]))
        }
        
        colnames(valuta) <- im
        
        df <- valuta
        rm(ime,valuta,im)
        df <- cbind(TIME=as.Date(tail(x,1)),df)
        df <- df[,1:29]
        df <- df[, colnames(df)!="ISK"]
        df <- df[, colnames(df)!="BGN"]
        w <- length(xml_data)
        
        showModal({
          modalDialog(
            title=h2(icona_narobe),
            fluidRow(column(12,align="center",
                            br(),
                            h4("Processing data. Please wait."),
                            br(),
                            h4("Estimated Time Left: 45 sec."),
                            br(),
                            tags$img(src = base64enc::dataURI(file = "www/sample_3.jpg", mime = "image/jpeg")))),
            br(),
            
            easyClose = F,
            footer=NULL,fade = F
          )
        })
        
        for(i in 2:(length(xml_data)-1)){
          df[i,] <- NA
          x <- unlist(xml_data[[i]])
          df$TIME[i] <- tail(x,1) %>% as.Date()
          for(j in 2:ncol(df)){
            if(colnames(df)[j] %in% x){
              ind <- which(x==colnames(df)[j]) %>% as.numeric()
              df[i,j] <- x[ind - 1] %>% as.numeric()
            }
          }
          incProgress(1/w, message =  "Processing:")
          #print(i/w)
        }
        
        #history <- df
        
        kat <-  !(df$TIME %in% history$TIME)
        
        
        
        history <- rbind(history,df[kat,])
        #write.csv(history,"data/hist.csv",row.names = F)
        
        history[,2:ncol(history)] <- apply(history[,2:ncol(history)],2,as.numeric)
        
        lastTRY$i <- Sys.Date()
        
        write.csv(history,"data/hist.csv",row.names = F)
        write.csv(lastTRY$i,"data/LT.csv",row.names = F)
        
        #assign("history",history,globalenv())
        #assign("LastTry",LastTry,globalenv())
        #naniar::gg_miss_var(history[,2:ncol(history)],show_pct = T)
        #rm(list=setdiff(ls(),c("history")))
        #save.image("data.RDATA")
        hist$hist <- history
        removeModal()
        
        LD$LD <- tail(history$TIME,1)
        
        showModal({
          modalDialog(
            title=h2(icona_ok),
            fluidRow(column(12,align="center",
                            br(),
                            h2("Done!"),
                            br(),
                            tags$img(src = base64enc::dataURI(file = "www/sample_3.jpg", mime = "image/jpeg")))),
            br(),
            
            easyClose = T,
            footer=column(12,align="center",modalButton("Close")),
            fade = F
          )
        })
        
      })
    }
  })
  
  # download ----------------------------------------------------------------
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      "report.html"
    },
    content = function(file) {
      showModal(
        modalDialog(
          title= h1(icona_narobe),
          br(),
          column(12,align="center",h3("Knitting report, please wait.")),
          withSpinner("",type=7,color="black"),
          easyClose=FALSE,
          
          footer=NULL
        )
      )
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path("report.Rmd")
      #file.copy("report1.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(par=param$DF,Options=MojResult$final,Futures=Fair$Value,history=hist$hist)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,output_format = "html_document",
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      removeModal()
    }
    
  )  
  
  kdo <- reactiveValues(sem="none")
  output$kdoseeem <- renderText({
    kdo$sem
  })
  
  observeEvent(input$logout,{
    #print(kdo$sem)
    IP <- read.csv("data/ip.csv",stringsAsFactors = F,encoding="UTF-8")
    ind <- which(IP$USERNAME == kdo$sem)
    IP$IP[ind] <- "unknown"
    IPtable$IP <- IP
    write.csv(IP,"data/ip.csv",row.names = F,fileEncoding = "UTF-8")
    shinyjs::js$refresh()
  })
  
  
  # edit users --------------------------------------------------------------
  tttt <- ""
  disable("save")
  hide("hot1")
  VALUES <- reactiveValues()

  #DF1 <- reactiveValues(DF1=read.csv("data/ip.csv",stringsAsFactors = F))

  ## Handsontable
  obsss <- observe({
    VALUES[["previous"]] <- isolate(IPtable$IP)
    #print(DF1$DF1)
    try({
      IPtable$IP = hot_to_r(input$hot) 
    },silent=T)
  },suspended = T)

  edit <- reactiveValues(edit=T)

  output$hot <- renderRHandsontable({
    #colnames(DF1$DF1) <-"EDIT"
    TMP <- isolate(IPtable$IP)
    # if(!is.null(input$hotSelect)){
    #TMP <- TMP[TMP$USERNAME == input$hotSelect || TMP$TYPE==input$hotSelect || TMP$LAST_LOGIN==input$hotSelect,]
    # }
    if (!is.null(TMP)){
      rhandsontable(TMP, readOnly = edit$edit,rowHeaders = F,selectCallback = T,stretchH = "all")
    }
  })

  ## Save
  observeEvent(input$save, {
    edit$edit <- T
    text$text <- tttt
    disable("save")
    enable("edit")
    toggle("hot2",anim = F)
    toggle("hot1",anim=F)
    obsss$suspend()
    finalDF <- isolate(IPtable$IP)
    write.csv(finalDF,"data/ip.csv",row.names = F)
    showModal({
      modalDialog(
        title=h2(icona_ok),
        fluidRow(column(12,align="center",
                        br(),
                        h4("The changes were saved!"),
                        br(),
                        tags$img(src = base64enc::dataURI(file = "www/sample_3.jpg", mime = "image/jpeg")),
                        br())),
        easyClose = T,
        size = "m",
        footer=column(12,align="center",
                      modalButton("Close",icon=icon("fas fa-check-circle")))
      )
    })
    #DF1$DF1 <- hot_to_r(finalDF)
  })

  observeEvent(input$edit,{
    edit$edit <- F
    text$text <- "You have enabled editing. You can add new users with right click. Use CTRL + F to filter."
    enable("save")
    disable("edit")
    toggle("hot1",anim = F)
    toggle("hot2",anim=F)
    obsss$resume()
    showModal({
      modalDialog(
        title=h2(icona_narobe),
        fluidRow(column(12,align="center",
                        br(),
                        h4("You have enable editing."),
                        br(),
                        tags$img(src = base64enc::dataURI(file = "www/sample_3.jpg", mime = "image/jpeg")),
                        br())),
        easyClose = T,
        size = "m",
        footer=column(12,align="center",
                      modalButton("Close",icon=icon("fas fa-check-circle")))
      )
    })
  })

  output$dff <- renderDataTable({
    IPtable$IP
  }
  # ,
  # options = list(
  #   lengthMenu = list(c(10, -1), c("10", 'ALL')),
  #   #preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }')
  #   drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } ')
  #)
  )


  text <- reactiveValues(text=tttt)

  output$info <- renderText({
    text$text
  })
  
  
}



shinyApp(ui,server,options = list(port=4000,host="192.168.1.11"))
