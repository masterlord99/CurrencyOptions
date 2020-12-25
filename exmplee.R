library(rhandsontable)
library(shiny)
library(shinyjs)
library(DT)
library(shinycssloaders)

df <- read.csv("ip.csv",stringsAsFactors = F)
cho <- c(df$USERNAME,df$TYPE,df$LAST_LOGIN)

ui <- shinyUI(
  fluidPage(
    useShinyjs(),
    column(12,align="center",
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
           br())
  )
)

server <- shinyServer(function(session,input, output) {
  tttt <- ""
  disable("save")
  hide("hot1")
  values <- reactiveValues()
  
  DF <- reactiveValues(DF=read.csv("ip.csv",stringsAsFactors = F))
  
  ## Handsontable
  obsss <- observe({
    values[["previous"]] <- isolate(DF$DF)
    DF$DF = hot_to_r(input$hot) %>% as.data.frame()
  },suspended = T)
  
  edit <- reactiveValues(edit=T)
  
  output$hot <- renderRHandsontable({
    #colnames(DF$DF) <-"EDIT"
    TMP <- (DF$DF)
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
    finalDF <- isolate(DF$DF)
    write.csv(finalDF,"ip.csv",row.names = F)
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
    #DF$DF <- hot_to_r(finalDF)
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
    DF$DF
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
  
})





## run app 
runApp(list(ui=ui, server=server))


# ( DF <- data.frame(Value = 1:10, Status = TRUE, Name = LETTERS[1:10],
#                    Date = seq(from = Sys.Date(), by = "days", length.out = 10),
#                    stringsAsFactors = FALSE) )
# 

