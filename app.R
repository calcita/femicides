# cargo archivo de datos #
load(file="VBG_UY.Rdata")

# funciones para customizar valueBox #
source("valueBox.R")
source("tabla.sub.R")

licencia <-"This work by Gabriela Mathieu is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License."

# cargo paquetes necesarios #
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(htmlwidgets)
library(markdown)
library(extrafont)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(waffle)
library(ggplot2)
library(timevis)
#library(shinyforms)
#library(shinythemes)
#library(sweetalertR)
#library(data.table)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### [HEADER]: Title and icons ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#includeCSS("www/style.css")
header <- dashboardHeader(title = "Cartografía digital de feminicidios", 
                          titleWidth = 600, 
                          tags$li(class = "dropdown",
                                  tags$a(
                                    target = "_blank",
                                    tags$img(height = "20px",
                                             src = "images/share.png")
                                  )
                          ),
                          #Facebook Sharing
                          tags$li(class = "dropdown",
                                  tags$a(#href = "http://www.facebook.com/sharer.php?u=https://calcita.shinyapps.io/VBG_UY/",
                                    target = "_blank",
                                    tags$img(height = "20px",
                                             src = "images/facebook-icon.png")
                                  )
                          ),
                          # Linkedin link
                          tags$li(class = "dropdown",
                                  tags$a(#href = "http://www.linkedin.com/shareArticle?mini=true&url=https://calcita.shinyapps.io/VBG_UY/",
                                    target = "_blank",
                                    tags$img(height = "20px",
                                             src = "images/linkedin-icon.png")
                                  )
                          ),
                          # Twitter link
                          tags$li(class = "dropdown",
                                  tags$a(#href = "http://twitter.com/share?url=https://calcita.shinyapps.io/VBG_UY/  &text= App sobre VBG @calcita3",
                                    target = "_blank",
                                    tags$img(height = "20px",
                                             src = "images/twitter-icon.png")
                                )
                         ) 
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### [SIDEBAR]: Item list ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sidebar <- dashboardSidebar(width = 55, 
                   sidebarMenu(
                      menuItem("", tabName = "Feminicidios", icon = icon("globe", "fa-2x")),
                      menuItem("", tabName="EncuestaVBG",icon = icon("bar-chart", "fa-2x")),
                      menuItem("", tabName="MInterior",icon = icon("pie-chart", "fa-2x"))
                          )
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### [BODY]: Item list ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

body <- dashboardBody(
                tags$style(type = "text/css", "html, body {width:100%;height:100%}",
                          ".leaflet .legend i{
                          border-radius: 50%;
                          width: 10px;
                          height: 10px;
                          margin-top: 4px;
                          }
                          .leaflet-popup-content-wrapper {
                          background: #cccccc;
                          color: #252525;
                          padding: 1px;
                          width: 250px;
                          border-radius: 1px;
                          }",
 
              tags$style(HTML("
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #f4b943;
                              }


                            .box.box-solid.box-primary>.box-header {
                            color:#fff;
                            background:#807dba
                            }

                            .box.box-solid.box-primary{
                            border-bottom-color:#666666;
                            border-left-color:#666666;
                            border-right-color:#666666;
                            border-top-color:#666666;
                            }
                        "))
),
  
### item fem #####  
tabItems(
    tabItem(tabName = "Feminicidios",
            fluidRow(
              column(3,
                     box(style="text-align:justify", width = NULL,title = tagList(shiny::icon("info-circle",class = 'fa-lg'), "INICIO"), solidHeader = T, collapsible = F,status="primary",
                         "La", strong("Cartografía digital de feminicidios"),"es una aplicación interactiva para acceder a los datos más recientes que dan cuenta de la violencia que afecta a mujeres y niñxs
                         y se sustenta en el sistema patriarcal. Es parte de un proyecto más amplio denominado 'Observatorio de Violencia Basada en Género' que en su etapa definitiva pretende dar cuenta de los principales
                         indicadores de desigualdad de género. En esta primera versión, se presenta información relativa al tipo de violencia más extrema que afecta a mujeres y niñxs, que son los feminicidios.", br(),
                         br(),
                         p("El",em("Mapa de Feminicidios"),
                           "permite visualizar geográficamente los feminicidios ocurridos en Uruguay desde 2016 hasta hoy. Combina el uso de fuentes oficiales -Ministerio del Interior- y no oficiales -datos de prensa-. 
                            El uso de estas últimas responde a brindar una actualización en tiempo real de los acontecimientos que dado como se dieron los hechos, son presumiblemente un feminicidio. A su vez,
                           la figura de feminicidio que por ley surge en abril de 2017, en general, no es utilizada ni incluye las estadísticas oficiales hasta tanto la justicia coloque esa carátula. Para la descripción de los feminicidios -a la cual se accede haciendo click sobre cada punto-
se utilizan varias fuentes periodísticas y policiales para dar cuenta del hecho ocurrido evitando el lenguaje sexista y calificativos machistas usados hacia la víctima."), #, a('here.', href = 'https://moto.data.socrata.com/dataset/Washington-DC/2vfk-6rp4', target = "_blank")
                         br(),
                         p("El mapa presenta los puntos del lugar aproximado donde ocurrió el crimen. A un bajo nivel de ampliación cuando hay un grupo de puntos cercanos, se muestra los puntos agrupados en color naranja o verde que indica
la cantidad de puntos que representa -haciendo zoom en la zona, se accede a los puntos individuales-. El panel de la derecha permite seleccionar las variables a visualizar, pudiendo elegir el año, mes, departamento, vínculo -del feminicida con la víctima- y el nombre de la víctima."))
                     #column(12, 
                     
                         ),
              
              br(),
              column(7,
                     box(width = NULL, solidHeader = T, style="font-family:  Verdana; font-weight: bold; font-size: 13px; color: #f2f0f7", title="Mapa de Feminicidios en Uruguay durante el período 2016-2018", 
                         leafletOutput("map", height="680px"),
                         
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = F,
                                        draggable = F, top = 10, left = "auto", right = 5, bottom = "auto",
                                        width = "auto", height = "auto"#,
                                       
                                        )
   
              )),
              
                     box(style = "color: #252525; background-color:#cbc9e2",
                         width =2, solidHeader = T, height=NULL,
                         p("Selecciona las categorías de las variables a mostrar en el mapa:"),
                         #year
                         uiOutput("year"),
                         #month
                         uiOutput("month"),
                         #departamento
                         uiOutput("departamento"),
                         #parentesco
                         uiOutput("parentesco"),
                         #nombre
                         uiOutput("nombre"),
                         br(),
                         p("Para restablecer los valores iniciales:"), 
                         #Quitar filtros 
                         actionButton("clear","", icon=icon("filter"),style="color: #fff; background-color: #cbc9e2; border-color: #636363"),
                         #Quitar zoom
                         actionButton("zoomup","", icon=icon("search"),style="color: #fff; background-color: #cbc9e2; border-color: #636363"),
                         #descargar mapa
                         actionButton("download","", icon=icon("download"),style="color: #fff; background-color: #cbc9e2; border-color: #636363"),
                         br(),br(),br(),br(),br(),br(),br(),br(),br(),
                         helpText(shiny::icon("time",class = 'fa-lg'), "Última actualización:", em(" 31-12-2017"), style="font-size:11px")
                        )
),
            
#########################
#fila
########################
            fluidRow(
              column(3,
                     box(width = NULL, solidHeader = T, collapsible = F,
                         # HTML("<div class='info-box-content' align='left'>
                         #                <div><p> fsdfdsfsdfdsfsdfsdfs <label id='vbg'> 
                         #                violencia de género 
                         #              </label> dfsfsdfsdfsdf </p>
                         #                </div> </div>"),
                         # bsTooltip(id = "vbg", title = "texto con la definición", placement = "right", trigger = "hover")
                         div(
                           # Another way is to use the builder functions; see ?tags.
                           "Los conceptos del glosario aparecen en",
                           tags$label(      # Creates an HTML span.
                             id="foo",  # Any named args become attributes.
                             #tags$strong("another"),  # Unnamed args become children of the tag.
                             "negrita"
                           ),
                           "y pasando el mouse por arriba aparece el texto que lo define. 
                           La línea de tiempo muestra cada feminicidio",tags$label(      # Creates an HTML span.
                             id="vbg",  # Any named args become attributes.
                             #tags$strong("another"),  # Unnamed args become children of the tag.
                             "VBG"
                         ),", en la fecha en que ocurrió."
                         ), bsTooltip(id = "foo", title = "texto con la definición", placement = "right", trigger = "hover"),   
                         bsTooltip(id = "vbg", title = "texto con la definición", placement = "right", trigger = "hover"), br(),br(),br(), br(),br(),br(),br(),br()
                     )),
              column(9,box(timevisOutput("tiempo"),width = NULL,height = "265px",title = tagList(shiny::icon("calendar",class = 'fa-lg'), "Sucesos"), solidHeader = T, collapsible = F, status="primary")
              )
            ),
#  #########################
#  #fila
#  ########################
            fluidRow(
              column(3, 
                      box(width = NULL, solidHeader = FALSE, collapsible = F, 
                          uiOutput("feminicida"),  
                          plotOutput("waffle1", height="200px"),paste0("En casi la totalidad de los casos, el feminicida es una persona familiar o cercana a la víctima. Así en un ",table(complete.data$feminicida_mapa)[1], "% de los casos, el feminicida era la pareja o ex-pareja de la mujer asesinada."))),
              column(3, 
                     box(width = NULL, solidHeader = FALSE, collapsible = F,
                         uiOutput("arma"),
                         plotOutput("waffle3", height="200px"), paste0("El arma de fuego y el arma blanca son las más utilizadas por los feminicidas, en un 41% y 20% de los casos respectivamente. Esto no quita que recurre en algunos casos a una combinación de armas o golpes."))),
              column(3, 
                     box(width = NULL, solidHeader = FALSE, collapsible = F,
                         plotOutput("waffle2", height="200px"))),
              column(3, 
                     box(width = NULL, solidHeader = FALSE, collapsible = F,
                     plotOutput("waffle4", height="200px")))
                     ),
           
            fluidRow(column(12, height=1,hr(),
                            tags$p(align="center", licencia,tags$a(href = "https://github.com/calcita", target = "_blank", tags$img(height = "20px",src = "images/creativecommons.png")))))#,img(height = "20px",src='images/github.png')
            
           
            
            ),
    #########################
    #fila 
    ########################
    tabItem(tabName = "MInterior",
            fluidPage(
              tags$head(tags$style(
                HTML('
                     input_date_control {background-color: rgba(0,0,255,0.2);;}
                     sel_date {background-color: rgba(0,0,255,1);}')
                )),
              p("Acá irán datos sobre la encuesta VBGG y las respuestas del estado ante la  violencia contra las mujeres en Uruguay"))
            
              ),
    
  tabItem(tabName = "EncuestaVBG",
            div(
            
              HTML("Acá irán <strong>gráficos y textos</strong> sobre los datos  del<em>Ministerio del Interior</em> -denuncias, medidas, etc.-.")
            )
    ))  
  
)

ui <- dashboardPage(skin="purple",header,sidebar,body)
#dashboardSidebar(disable = T),

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### [SERVER]: #####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- function(input, output, session) {
  
  ###inicio
    output$year <- renderUI({
                            pickerInput("year", label = "Año", choices=c("2016","2017","2018"),multiple = TRUE, selected = c("2016","2017","2018"),options = list(
                            `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos", `none-selected-text` = "Ninguno seleccionado"))
                           })
  
  output$month <- renderUI({
                            pickerInput("month", label = "Mes", choices=levels(complete.data$mes), multiple = TRUE, selected = levels(complete.data$mes),options = list(
                            `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos", `none-selected-text` = "Ninguno seleccionado"))
                          })
  
  output$departamento <- renderUI({
                                   pickerInput("departamento", label = "Departamento", choices = levels(complete.data$departamento), multiple = TRUE, selected = levels(complete.data$departamento),options = list(
                                  `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos", `none-selected-text` = "Ninguno seleccionado"))
                                 })
  
  output$parentesco <- renderUI({
                                 pickerInput(inputId ="parentesco", label = "Parentesco", choices = levels(complete.data$feminicida_mapa), multiple = TRUE, selected = levels(complete.data$feminicida_mapa),options = list(
                                `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos", `none-selected-text` = "Ninguno seleccionado"))
                              })
    
  output$nombre <- renderUI({
                            pickerInput("nombre","Buscar por nombre", choices=levels(complete.data$nombre), multiple = TRUE, selected = levels(complete.data$nombre),options = list(
                            `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos", `none-selected-text` = "Ninguno seleccionado")) 
                            })
   
     
  actual="31 de diciembre de 2017"
  
  # guardar posiciones de clicks
  data_of_click <- reactiveValues(clickedMarker=NULL)

  zoom_map <- leafletOptions(zoomControl = TRUE, minZoom = 3, maxZoom = 18, dragging = TRUE)
  
  # define colors
  pal <- colorFactor(c("#8c510a","#01665e","#542788", "#b2182b", "black"), domain=complete.data$feminicida_mapa)
  icons <- awesomeIcons(
                        icon = 'ios-close',
                        iconColor = 'black',
                        library = 'ion',
                        markerColor = pal)
  
  # creat map
  output$map <- renderLeaflet({

    map.data <- complete.data %>%
       filter(year %in% input$year) %>%
       filter(mes %in% input$month) %>%
       filter(departamento %in% input$departamento) %>%
       filter(feminicida_mapa %in% input$parentesco) %>%
       filter(nombre %in% input$nombre)
    
   contenido <- paste("<p style=text-align:right>",paste0("<b><big>","#",map.data$id,"</big></b>"),paste0("<b><big>",map.data$nombre,"<br>",paste(map.data$edad, "años","</big></b><br>"),paste0("<p style=text-align:right>","<i>",map.data$departamento, ", ",map.data$fecha_mapa,"</i><hr>")),"<p style=text-align:justify>",map.data$descripción)
    
    if (nrow(map.data) != 0) {
      leaflet(map.data,options = zoom_map) %>%
      setView(-55.8517, -32.6186, zoom=7)%>%
      addProviderTiles("CartoDB.Positron") %>% #, group="2017"
      #addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      #addCircleMarkers(color = ~pal(Feminicida),stroke = FALSE, fillOpacity = 0.5, lng = ~lat, lat = ~lon, clusterOptions = markerClusterOptions(), popup=~contenido, group= "Todos")  #
      addCircleMarkers(data=map.data,~lat , ~lon, layerId=~id, popup=~contenido, color= ~pal(feminicida_mapa),  stroke = FALSE, fillOpacity = 0.5,
      clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {
                                                              var childCount = cluster.getChildCount();
                                                              var c = ' marker-cluster-';
                                                              if (childCount < 100) {
                                                              c += 'large';
                                                              } else if (childCount < 1000) {
                                                              c += 'medium';
                                                              } else {
                                                              c += 'small';
                                                              }
    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });

  }"))) %>% 
      addLegend("bottomleft", title = "Parentesco del feminicida", pal = pal, values = ~levels(map.data$feminicida_mapa), opacity = 0.5) %>%
      addSearchOSM(options = searchOSMOptions(zoom=15, position = 'topleft')) #%>%

      #addLayersControl(position = "topleft", baseGroups = c("2017"),overlayGroups = c("2017","2016"), options = layersControlOptions(collapsed = T))
    } else{
      leaflet()%>%
        setView(-55.8517, -32.6186, zoom=7)%>%
        addProviderTiles("CartoDB.Positron") 
    } 
  })
  
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  output$texto_mapa=renderText({
    my_place=data_of_click$clickedMarker$id
    if(is.null(my_place)){my_place="0"}
    if(my_place=="0"){
      print("Haz click en cualquiera de los puntos para ver la descripción")
    }else{
      print(as.character(map.data[map.data$id==my_place,]$descripción))
    }
  })

  ### ploteo waffles ####
  
  #defino colores para waffles
  colores=c("#969696","#4a1486","#807dba","#bcbddc","#66c2a4","#2ca25f","#006d2c")
  
  ## waffle1
  output$feminicida <- renderUI({
                          pickerInput("feminicida",helpText(icon("pie-chart"),"Selecciona las categorías a graficar:",style="font-size:12px; font-style:normal") ,choices=levels(complete.data$feminicida_mapa), multiple=T, selected=c("Pareja o ex-pareja","Sin determinar"),options = list(
                          `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos", `none-selected-text` = "Ninguno seleccionado")) 
  })
  
  
  output$waffle1 <- renderPlot({
    feminicida.input <-input$feminicida
    tabla <- tabla.sub(complete.data$feminicida_mapa, feminicida.input)
    waffle(tabla, rows=5, size=0.9, colors=colores[1:(length(feminicida.input)+1)], legend_pos="bottom") +
           labs(title = "Vínculo del feminicida con la víctima:",
           subtitle = "1 cuadrado = 1%",
           caption = "Datos 2016-2018") +
           theme(text = element_text(family = "Lato"))
  })
 
    output$waffle2 <- renderPlot({
    colores=c("#969696","#4a1486","#006d2c") #,"#006d2c"
    y <-table(complete.data$menor,useNA="ifany")
    tabla <- round(y/sum(y)*100,0)
    waffle(tabla, rows=5, size=0.9, colors=colores, legend_pos="bottom") +
           labs(title = "Víctimas de feminicidio según edad",
           subtitle = "1 cuadrado = 1%",
           caption = "Datos 2016-2018") +
           theme(text = element_text(family = "Lato"))
    
  })
  
  output$arma <- renderUI({
    pickerInput("arma",helpText(icon("pie-chart"),"Selecciona las categorías a graficar:",style="font-size:12px; font-style:normal") ,choices=levels(complete.data$arma), multiple=T, selected=c("Arma de fuego","Arma blanca"),options = list(
      `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos", `none-selected-text` = "Ninguno seleccionado")) 
  })
  
  
  output$waffle3 <- renderPlot({
    arma.input <-input$arma
    tabla <- tabla.sub(complete.data$arma, arma.input)
    waffle(tabla, rows=5, size=0.9, colors=colores[1:(length(arma.input)+1)], legend_pos="bottom") +
           labs(title = "Arma utilizada por el feminicida",
           subtitle = "1 cuadrado = 1%",
           caption = "Datos 2016-2018") +
           theme(text = element_text(family = "Lato"))
  })
  

  output$waffle4 <- renderPlot({
    colores=c("#969696","#4a1486","#006d2c") #,"#006d2c"
    y <-table(complete.data$espacio,useNA="ifany")
    tabla <- round(y/sum(y)*100,0)
    waffle(tabla, rows=5, size=0.9, colors=colores, legend_pos="bottom") +
      labs(title = "Lugar donde ocurrió el feminicidio",
           subtitle = "1 cuadrado = 1%",
           caption = "Datos 2016-2018") +
      theme(text = element_text(family = "Lato"))
    
  })
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## [Tiempo]: timevis
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  sucesos <- data.frame(
    id      = 1:7,
    content = c(paste(sep = "<br/>","Luna Chiodi", "(51 años)", "<img align=center src='images/nn.png' />")  , paste(sep = "<br/>","Mirtha Lorena Rocha", "(30 años)", "<img align=center src='images/nn.png' />")  ,paste(sep = "<br/>","Olga Costa", "(44 años)", "<img align=center src='images/nn.png' />"), paste(sep = "<br/>","Sirley Silva", "(25 años)", "<img align=center src='images/nn.png' />"), paste(sep = "<br/>","Vanesa Monzón", "(32 años)", "<img align=center src='images/nn.png' />"), paste(sep = "<br/>","Julia Olivera", "(29 años)", "<img align=center src='images/nn.png' />"), paste(sep = "<br/>","Edelma Suares", "(65 años)", "<img align=center src='images/nn.png' />")),
    start   = c("2018-03-19", "2018-03-10", "2018-03-08", "2018-02-21","2018-02-19","2018-02-09", "2018-01-23"),
    end     = rep(NA, 7),
    style = rep("background: #cccccc; color: black; height: 135px",length(id))
     )
  
  output$tiempo <- renderTimevis(timevis(data=sucesos,
                                         options = list(
                                           stack = FALSE, 
                                           stackSubgroups = TRUE, 
                                           orientation = "top",
                                           verticalScroll = TRUE,
                                           zoomKey = 'ctrlKey',
                                           start = "1/01/2018",
                                           end = "3/31/018",
                                           max = "2019",
                                           min = "2016",
                                           margin = list(item = 10, axis = 10),
                                           clickToUse=T
                                           #minHeight = 750, 
                                           #maxHeight = 750
                                         )))
  
  ####
  #  output$texting <- renderText({includeHTML("intro.html")})
}

shinyApp(ui, server)