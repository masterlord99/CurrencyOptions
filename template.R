# https://machinelearningmastery.com/how-to-develop-convolutional-neural-networks-for-multi-step-time-series-forecasting/
# https://machinelearningmastery.com/how-to-develop-lstm-models-for-multi-step-time-series-forecasting-of-household-power-consumption/



library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(V8)
library(shinycssloaders)
source("slike.r")

# inclRmd <- function(path) {
#   paste(readLines(path, warn = FALSE), collapse = '\n') %>%
#     knitr::knit2html(text = ., fragment.only = TRUE, options = "",
#                      stylesheet=file.path(r_path,"../www/empty.css")) %>%
#     gsub("&lt;!--/html_preserve--&gt;","",.) %>%
#     gsub("&lt;!--html_preserve--&gt;","",.) %>%
#     HTML %>%
#     withMathJax
# }

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

ui <- dashboardPage(
  dashboardHeader(title = "MLB App",
                  dropdownMenuOutput("messageMenu"),
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "Razbij markdown!",
                                 status="primary",
                                 icon("exclamation-triangle")
                               ),
                               notificationItem(
                                 text = "Uredi source kode!",
                                 icon("exclamation-triangle"),
                                 status = "warning"
                               )
                  ),
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 100, color = "green",
                                        "Presentation: Adacta"
                               ),
                               taskItem(value = 17, color = "aqua",
                                        "Project X"
                               ),
                               taskItem(value = 86, color = "yellow",
                                        "Natančnost"
                               )
                  )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("user")),
      menuItem("Import", tabName = "dataImport", icon = icon("wrench")),
      menuItem("Preprocess", tabName = "preprocess", icon = icon("wrench")),
      menuItem("Visualisation", tabName = "visualisation", icon = icon("bar-chart-o")),
      menuItem("Digging deeper", tabName = "dig", icon = icon("wrench")),
      menuItem("Supervised learning", tabName = "SVlearning", icon = icon("question")),
      menuItem("Dimensionality problem", tabName = "dimprob", icon = icon("exclamation-triangle")),
      menuItem("Class imbalance", tabName = "imb", icon = icon("exclamation-triangle")),
      menuItem("Deep learning & ML", tabName = "ml", icon = icon("codepen")),
      menuItem("App", tabName = "app", icon = icon("android")),
      menuItem("Conclusions", tabName = "conc", icon = icon("truck")),
      menuItem("Source code", tabName = "source", icon = icon("code"))
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
    
    tags$head(tags$style(HTML('
                              @import url("//fonts.googleapis.com/css?family=Lobster|Cabin:400,700");
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #4B0082;
                              color: #E6E6FA;
                              font-family: Lobster, cursive;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #4B0082;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #4B0082;
                              }        
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #4B0082;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #8A2BE2;
                              }
                              
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color:  #4B0082;
                              color: #FFF0F5;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #000080;
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color:#000080;
                              }
    
                              '))
    ),
    
    tabItems(
      tabItem(tabName = "intro",
              navbarPage("Introduction",
                         tabPanel("Title",
                                  column(12,align="center",
                                  br(),
                                  h1("Assigment Report",style="color:maroon"),
                                  br(),
                                  h2("Jakob Jelenčič"),
                                  br(),
                                  h3("23. 11. 2018")
                                  )
                         ),
                         tabPanel("Summary",
                                  #h1("d")
                                  includeMarkdown("test.Rmd")
                         )
              )
      ),
      tabItem(tabName = "dataImport",
              navbarPage("Data Import",
                         tabPanel("Presentation",
                                  h1("Nalimaj RMD")
                         ),
                         tabPanel("Deatils and Code",
                                  h1("Nalimaj kodo!")
                         )
              )
      ),
      tabItem(tabName = "preprocess",
              navbarPage("Data Preprocess",
                         tabPanel("Presentation",
                                  h1("Nalimaj RMD")
                         ),
                         tabPanel("Deatils and Code",
                                  h1("Nalimaj kodo!")
                         )
              )
      ),
      tabItem(tabName = "visualisation",
              navbarPage("Data Visualisation",
                         tabPanel("Dashboard",
                                  h1("Nalimaj RMD")
                         ),
                         tabPanel("Statistics",
                                  h1("Nalimaj RMD")
                         ),
                         tabPanel("Number of customers",
                                  h1("Nalimaj RMD")
                         ),
                         tabPanel("Deatils and Code",
                                  h1("Nalimaj kodo!")
                         )
              )
      ),
      tabItem(tabName = "dig",
              navbarPage("Digging deeper",
                         tabPanel("Presentation",
                                  h1("Nalimaj RMD")
                         ),
                         tabPanel("Deatils and Code",
                                  h1("Nalimaj kodo!")
                         )
              )
      ),
      tabItem(tabName = "SVlearning",
              navbarPage("Creating supervised learning",
                         tabPanel("Presentation",
                                  h1("Nalimaj RMD")
                         ),
                         tabPanel("Deatils and Code",
                                  h1("Nalimaj kodo!")
                         )
              )
      ),
      tabItem(tabName = "dimprob",
              navbarPage("Dimensionality problem",
                         tabPanel("Presentation",
                                  h1("Nalimaj RMD")
                         ),
                         tabPanel("Deatils and Code",
                                  h1("Nalimaj kodo!")
                         )
              )
      ),
      tabItem(tabName = "imb",
              navbarPage("Class imbalance",
                         tabPanel("Presentation",
                                  h1("Nalimaj RMD")
                         ),
                         tabPanel("Deatils and Code",
                                  h1("Nalimaj kodo!")
                         )
              )
      ),
      tabItem(tabName = "ml",
              navbarPage("Models",
                         tabPanel("Classification DNN",
                                  h1("Nalimaj RMD")
                         ),
                         tabPanel("Regression DNN",
                                  h1("Nalimaj kodo!")
                         ),
                         tabPanel("Random Forest",
                                  h1("Nalimaj kodo!")
                         ),
                         tabPanel("Source Code",
                                  h1("Nalimaj kodo!")
                         )
              )
      ),
      tabItem(tabName = "app",
              navbarPage("App",
                         tabPanel("Prototype",
                                  fluidPage(
                                    useShinyjs(),
                                    #theme=shinytheme("slate"),
                                    tags$style(type="text/css", "
                                               #loadmessage {
                                               position: fixed;
                                               top: 0%;
                                               left: 0px;
                                               width: 100%;
                                               padding: 5px 0px 5px 0px;
                                               text-align: center;
                                               font-weight: bold;
                                               font-size: 300%;
                                               color:#000000;
                                               background-color:'black' ;
                                               z-index: 105;
                                               }
                                               "),
                          tags$head(
                            tags$style(HTML("hr {border-top: 4px solid black;}"))
                          ),
                          align="center",id="side",
                          conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                           tags$div(withSpinner("Delam...",type=8,size=1.5,color = "black",color.background = "none"),id="loadmessage")),

                          fluidRow(id="glava",
                                   hr(),
                                   h1(icon("money", lib = "font-awesome"),icon("euro", lib = "font-awesome"),icon("money", lib = "font-awesome"),style="color:silver"),
                                   br(),
                                   numericInput("cas","Za koliko dni naprej želiš pogledati?:",7,min = 1,step=1,width = "25%"),

                                   h1(icon("question", lib = "font-awesome"),icon("question", lib = "font-awesome"),icon("question", lib = "font-awesome"),style="color:silver"),
                                   actionButton("calc","Preveri",icon=icon("fas fa-wrench", lib = "font-awesome")),
                                   helpText("Spodaj se bodo izpisale obveznosti v naraščujočem vrstnem redu."),
                                   br()
                          ),
                          hr(),
                          column(12,id="skrij",align="center",
                                 h1(icon("wrench", lib = "font-awesome"),icon("pencil", lib = "font-awesome"),icon("wrench", lib = "font-awesome"),style="color:steelblue"),
                                 h1("Prihajajoče:",style="color:maroon"),
                                 tableOutput("tabela2"),
                                 br(),
                                 hr(),
                                 br(),
                                 h1(icon("pencil", lib = "font-awesome"),icon("wrench", lib = "font-awesome"),icon("pencil", lib = "font-awesome"),style="color:steelblue"),
                                 h1("Tekoče:",style="color:seagreen"),
                                 tableOutput("tabela"),
                                 br(),
                                 hr(),
                                 column(12,id="rep",
                                        br(),
                                        h1(icon("fas fa-university", lib = "font-awesome"),icon("fas fa-university", lib = "font-awesome"),icon("fas fa-university", lib = "font-awesome"),style="color:silver"),
                                        actionButton("exc","V Excel",icon = icon("arrow-circle-up")),
                                        actionButton("det","Dodatna pojasnila",icon = icon("question")),
                                        actionButton("print","Natisni",icon = icon("print")),
                                        helpText('Za Dodatna pojasnila o tekočih in prihajajočih obveznostih klikni "Dodatna pojasnila".'),
                                        helpText('Za tiskanje klikni "Natisni", nato pa "CTRL + P".'),
                                        br(),
                                        hr())
                          )
                          )
                         ),
                         tabPanel("Source Code",
                                  h1("Nalimaj RMD!")
                         )
              )
      ),
      tabItem(tabName = "conc",
              navbarPage("Conclusions",
                         tabPanel("Findings",
                                  h1("Nalimaj RMD")
                         ),
                         tabPanel("Goodbye Gollum",
                                  column(12,align="center",
                                         br(),
                                         actionButton("bye","Say Goodbye",icon=icon("sign-out")),
                                         br()
                                  )
                                  
                         )
              )
              
              
              
      ),
      tabItem(tabName = "source",
              navbarPage("Source Code",
                         tabPanel("Presentation source code",
                                  h1("Nalimaj RMD")
                         ),
                         tabPanel("Files' source code",
                                  h1("Nalimaj kodo!")
                         )
              )
      )
      
      
    )
    
  )
  
  
)

server <- function(input, output,session){ 
  
  observe({
    showModal({
    modalDialog(
      title="",
      fluidRow(column(12,align="center",
                      br(),
                      h2("Welcome!",style="color:maroon"),
                      logo)),
      br(),
      
      easyClose = T,
      footer=column(12,align="center",modalButton("Close"))
    )
    })
  })
  

  observeEvent(input$calc,{
    Sys.sleep(5)
  })
  
  observeEvent(input$bye,{
    
    showModal({
      modalDialog(
        title=h3(icona_ring),
        fluidRow(column(12,align="center",
                        gollum)),
        br(),
        
        easyClose = FALSE,
        footer=column(12,align="center",
                      actionButton("rbye","RUN!")
                      )
      )
    })
  })
  

  observeEvent(input$rbye,{
    stopApp()
  })
  
  # output$mini_case_1 <- renderUI({
  #   tagList(
  #     #rmarkdown::render("test.Rmd", html_document()),
  #     inclRmd("test.Rmd")
  #   )
  # })
  
}


#runApp(
shinyApp(ui, server)#,options = list(port=1231,host="10.10.4.77")))

#library(keras)

