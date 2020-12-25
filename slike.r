library(shiny)


icona_narobe <- fluidRow(
  column(12,align="center",
         icon("fas fa-exclamation-triangle", lib = "font-awesome")
  ))

icona_ok <- fluidRow(
  column(12,align="center",
         icon("fas fa-check-circle", lib = "font-awesome")
  ))

icona_ring <- fluidRow(
  column(12, align="center",
         
         img(src="onering.jpg"),
         br()
  ))
################################################################################ narejene slikice



logo <- fluidRow(
  column(12, align="center",
         br(),
         img(src="sample_3.jpg"),
         br()))

bilbo <-fluidRow(
  column(12, align="center",
         br(),
         img(src="bilbo.jpg"),
         br()))

gollum <-fluidRow(
  column(12, align="center",
         br(),
         img(src="gollum.jpg"),
         br()))