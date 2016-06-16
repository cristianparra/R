US = NULL;
consulta = function(){
  Username <- RUTA$US
  consulta2 =paste("SELECT markets FROM usuarios WHERE user = '",Username, sep="","';")
  resp=dbGetQuery(con,consulta2)
  return(resp)
}

nombre_empresa = function(){
  Username <- RUTA$US
  consulta2 =paste("SELECT empresa FROM usuarios WHERE user = '",Username, sep="","';")
  resp=dbGetQuery(con,consulta2)
  return(resp)
}

session_close=reactive({
  dbDisconnect(con)
  return("")
})

head=read.csv("www/files/head.csv",head=TRUE)
rownames(head)=head$Nombre
names.l=as.vector(head$Nombre)
markets=as.vector(unique(head$Mercado))
group <- data.frame(groupID = markets,stringsAsFactors =FALSE)

output$mercados = DT::renderDataTable ({
  head
},class = 'cell-border stripe', options = list(
  aLengthMenu = c(12, 30, 50), DisplayLength = 12 ,
  bLengthChange=FALSE, bInfo=FALSE))

textInputadmin <- function(inputId, label) {
  tagList(
    tags$label(label),
    tags$br(),
    tags$input(id = inputId, type="text",placeholder=""),
    tags$br()
  )
}

session_close=reactive({
  dbDisconnect(con)
  return("")
})

output$nav=renderUI({

  tags$div(class='navbar navbar-default',
           #style="background-color: #151515;",
           tags$a(class="navbar-brand",href="#",
                  tags$img(style="max-width:148px; margin-top: -13px;",src="img/igm.gif")
           ),
           
           tags$ul(class="nav navbar-nav navbar-right",
                   tags$li(class="",tags$a(href="#",tags$b(tags$span(icon("database")," Administrador de Usuarios")))),
                   tags$li(class="divider",""),
                   tags$li(class="",tags$a(href="#",onClick="location.reload();",tags$span(icon("sign-out")," Salir"))),
                   tags$li(tags$a(href="#"))))
})

output$paneladmin = renderUI({
  tabsetPanel(
    tabPanel(tags$span(icon("list"),"Lista Usuario de Sistema"),
             tags$br(),
             actionButton("update_table", "Actualizar Lista"),
             tags$br(),
             tags$br(),
             DT::dataTableOutput("tabla")
    ),
    tabPanel(tags$span(icon("plus"),"Agregar Usuario Nuevo"),
             tags$br(),
             fluidRow(
               column(6,
                      wellPanel(
                        h4("Formulario Etapa 1"),
                        tags$br(),
                        textInputadmin("ingresar_nombre_empresa","Nombre Empresa:"),
                        tags$br(),
                        textInputadmin("ingresar_usuario","Usuario: (Inicio de Sesion)"),
                        tags$br(),
                        tags$br(),
                        checkboxGroupInput("ing_merc","Seleccione Mercados: ","",choices=as.vector(unique(head$Mercado))),
                        tags$br(),
                        actionButton("goingresar1", "Agregar Usuario"),
                        tags$br(),
                        tags$br(),
                        textOutput("ok1")
                      )),
               column(6,
                      h4("Formulario Etapa 2"),
                      tags$br(),
                      textInputadmin("ingresar_Clave","Contrasena:"),
                      tags$br(),
                      actionButton("generar_clave", "Generar Contrasena"),h5("*(Opcional)"),
                      tags$br(),
                      tags$br(),
                      uiOutput("clave"),
                      tags$br(),
                      actionButton("goingresar2", "Agregar Contrasena"),
                      tags$br(),
                      tags$br(),
                      textOutput("ok2"),
                      tags$br()
               )
             )
    ),
    tabPanel(tags$span(icon("refresh"),"Actualizar Datos de Usuario"),
             tags$br(),
             fluidRow(
               column(3,
                      textInputadmin("upd","Ingrese ID Usuario a Actualizar"),
                      tags$br(),
                      actionButton("updButton", "Busqueda")
               ),
               column(9,
                      wellPanel(
                        "Resultado Busqueda",
                        tags$br(),
                        tags$br(),
                        dataTableOutput("resu.tabla"),
                        tags$br(),
                        tags$br(),
                        fluidRow(
                          column(6,checkboxGroupInput("upd_merc","Actualizar Mercados: ","",choices=as.vector(unique(head$Mercado))),
                                 tags$br(),
                                 actionButton("actualizar_mercados","Actualizar Mercados"),
                                 tags$br(),
                                 textOutput("text_up_merc")
                          ),
                          column(6,
                                 textInputadmin("actualizar_Clave","Cambiar Contrasena:"),
                                 tags$br(),
                                 actionButton("generar_clave2", "Generar Contrasena"),h5("*(Opcional)"),
                                 tags$br(),
                                 tags$a("Abrir Pagina MD5 para Descifrar Claves Usuarios",href="http://md5.gromweb.com/",target="_blank"),
                                 tags$br(),
                                 tags$br(),
                                 uiOutput("clave2"),
                                 tags$br(),
                                 actionButton("goupdat", "Actualizar Datos"),
                                 tags$br(),
                                 textOutput("ok_update")
                          )
                        )
                      ))
             )
    ),
    tabPanel(tags$span(icon("trash-o"),"Eliminar Usuario"),
             fluidRow(
               column(3,
                      tags$br(),
                      textInputadmin("buscar_eliminar","Ingrese Usuario a eliminar"),
                      tags$br(),
                      actionButton("deletebusquedaButton", "Buscar Usuario")),
               column(9,
                      tags$br(),
                      wellPanel(
                        "Resultado Busqueda",
                        tags$br(),
                        tags$br(),
                        dataTableOutput("tabla_delete"),
                        tags$br(),
                        tags$br(),
                        actionButton("goeliminar", "Eliminar Usuario"),
                        tags$br(),
                        tags$br(),
                        textOutput("ok_delete")
                      ))
             )),
    tabPanel(tags$span(icon("newspaper-o"),"Mercados"),
             fluidRow(
               column(12,tags$br(),dataTableOutput("mercados"))
             )
    )
  )
})

###########################################funcion generar palabra ramdom

MHmakeRandomString <- function(n=1, lenght=5)
{
  randomString <- c(1:n)                  
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    lenght, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}

generar_clave = eventReactive(input$generar_clave,{
  output$clave = renderUI({
    wellPanel(
      paste("Clave Generada: ",MHmakeRandomString())
    )
  })
})

###########################################Insertar
ntext <- eventReactive(input$goingresar1, {
  user <- isolate(input$ingresar_usuario)
  rutaa <- "www/effi.R"
  mercados <- isolate(input$ingresar_mercados)
  nombre_e <- isolate(input$ingresar_nombre_empresa)
  mercados=input$ing_merc
  mercados=paste0(mercados,collapse=",")
  n1 = paste0("INSERT INTO usuarios (user,pass,ruta,markets,empresa) VALUES ('",user,"','pass','",rutaa,"','",mercados,"','",nombre_e,"')",sep=" ")
  x=dbGetQuery(con,n1)
  output$ok1 = renderText({"Datos Agregados"})
})

observe({ntext()})

cla <- eventReactive(input$goingresar2, {
  user2 <- isolate(input$ingresar_usuario)
  pass2 <- isolate(input$ingresar_Clave)
  query_clave=paste("UPDATE usuarios SET pass=md5('",pass2,"') WHERE user='",user2,"';",sep = "")
  qclave=dbGetQuery(con,query_clave)
  output$ok2 = renderText({"Clave Agregada"})
})

observe({cla()})

###########################################Update
observe({generar_clave()})

generar_clave2 = eventReactive(input$generar_clave2,{
  output$clave2 = renderUI({
    paste("Clave Generada: ",MHmakeRandomString())
  })
})

observe({generar_clave2()})

upd_table <- eventReactive(input$updButton, {
  texto2 <- isolate(input$upd)
  x1 =paste("SELECT user,pass,markets,empresa FROM usuarios WHERE user = '",texto2 , sep="", "';")
  nombre_ee=dbGetQuery(con,x1)
  output$resu.tabla = DT::renderDataTable({nombre_ee},rownames = FALSE, options = list(dom='t'))
})

observe({upd_table()})

upd_user <- eventReactive(input$goupdat,{
  user_u <- isolate(input$upd)
  pass_u <- isolate(input$actualizar_Clave)
  #mercadosu=input$upd_merc
  #mercadosu=paste0(mercadosu,collapse=",")
  updat_use = paste("UPDATE usuarios SET pass=md5('",pass_u,"') WHERE user='",user_u,"';",sep = "")
  actualizar=dbGetQuery(con,updat_use)
  output$ok_update = renderText({"Datos Actualizados"})
})

observe({upd_user()})

updat_mercados <- eventReactive(input$actualizar_mercados,{
  user_u <-isolate(input$upd)
  mercadosu=input$upd_merc
  mercadosu=paste0(mercadosu,collapse=",")
  upda_mercados=paste("UPDATE usuarios SET markets='",mercadosu,"' WHERE user='",user_u,"';",sep = "")
  actua_merca=dbGetQuery(con,upda_mercados)
  output$text_up_merc = renderText({"Mercados Actualizados"})
})

observe({updat_mercados()})

###########################################Eliminar
del_table <- eventReactive(input$deletebusquedaButton, {
  texto2 <- isolate(input$buscar_eliminar)
  x1 =paste("SELECT user,pass,markets,empresa FROM usuarios WHERE user = '",texto2 , sep="", "';")
  nombre_de=dbGetQuery(con,x1)
  output$tabla_delete = DT::renderDataTable({nombre_de},rownames = FALSE,options = list(dom='t'))
})

observe({del_table()})

delete_text <- eventReactive(input$goeliminar, {
  texto1 <- isolate(input$buscar_eliminar)
  x2 =paste("DELETE FROM usuarios WHERE user = '",texto1,sep="","'")
  x=dbGetQuery(con,x2)
  output$ok_delete = renderText({"Usuario Eliminado"})
})

observe({delete_text()})

###########################################Vista Tabla

vista_tabla = function(){
  y1 =paste("SELECT * FROM usuarios;")
  y=dbGetQuery(con,y1)
  return(y)
}

output$tabla = DT::renderDataTable({
  vista_tabla()}
  ,class = 'cell-border stripe', options = list(
    aLengthMenu = c(12, 30, 50), DisplayLength = 12 ,
    bLengthChange=FALSE, bInfo=FALSE))
############################################

ut <- eventReactive(input$update_table, {
  output$tabla = DT::renderDataTable({
    vista_tabla()}
    ,class = 'cell-border stripe', options = list(
      aLengthMenu = c(12, 30, 50), DisplayLength = 12 ,
      bLengthChange=FALSE, bInfo=FALSE))
})

observe(ut())

###########################################
cs <- eventReactive(input$Salir,{
  dbDisconnect(con)
})

observe(cs())