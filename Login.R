#### Log in module ###
USER <- reactiveValues(Logged = Logged)
RUTA <- reactiveValues(US = US)
  
passwdInput <- function(inputId, label) {
  tagList(
    tags$br(),
    tags$label(label),
    tags$br(),
    tags$input(id = inputId, type="password", placeholder=" Password")
  )
}

textInput <- function(inputId, label) {
  tagList(
    tags$label(label),
    tags$br(),
    tags$input(id = inputId, type="text", placeholder=" Nombre de Usuario")
  )
}

output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    tags$div(class="container",
    tags$div(class="g-item",style="background-image:url('img/igm.gif')",
             tags$div(class="col-xs-9"),
             tags$div(class="col-xs-3" ,
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),tags$br(),tags$br(),tags$br(),
                      tags$img(src="img/effi.png" ,width = 150, height = 36),
                      tags$br(),
                      textInput("userName", ""),
                      passwdInput("passwd", ""),
                      tags$br(),
                      tags$br(),
                      actionButton(class="btn btn-primary","Login", tags$span(icon("sign-in"),"Ingresar")),
                      tags$br(),
                      textOutput("pass_erro"),
                      tags$br(),
                      tags$div(icon("envelope"),tags$span(" comercial@effitrade.cl"),style="color:#CFCFCF;font-size: 14px;"),
                      tags$div(icon("phone-square"),tags$span(" +56 71 2246100"),style="color:#CFCFCF;font-size: 14px;"),
                      tags$div(icon("mobile fa-2x"),tags$span(" +56 9 88694840"),style="color:#CFCFCF;font-size: 14px;"),
                      tags$div(icon("refresh"),tags$span("Camino al Cerro Parcela 17, Tejas Verdes, Talca-Chile"),style="color:#CFCFCF;font-size: 14px;"),
                      tags$br()
                      )
    )
    )
  }
})

output$pass_erro <- renderText({ 
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
      if (input$Login > 0) {
        source("www/connection/connectionBD.R")
        Username <- isolate(input$userName)
        Password <- isolate(input$passwd)
        x1 =paste("SELECT user FROM usuarios WHERE user = '",Username , sep="", "';")
        x=dbGetQuery(con,x1)
        Id.username <- which(x==Username)
        
        y1 =paste("SELECT pass FROM usuarios WHERE pass = '",Password , sep="", "';")
        y=dbGetQuery(con,y1)
        Id.password <- which(y==Password)
        
        if (length(Id.username) > 0 & length(Id.password) > 0) {
          if (Id.username == Id.password) {
            USER$Logged <- TRUE
            RUTA$US <- Username
          }
        } else  {
          h4("Usuario o Contrasena Incorrecta")
        }
      } 
    }
  }
  
})
