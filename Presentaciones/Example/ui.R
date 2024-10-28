library(dygraphs)
library(shinydashboard) 
shinyUI(fluidPage(theme = shinytheme("sandstone"),

    # Application title
    titlePanel(
        h1(id = "title", "Wage Gap in Mexican Labor Market", aling = "center")),
        tags$style(HTML("#title{
                        font-style: italic;}")),
        navbarPage("by Ehekatzin García", 
                   tabPanel("Género", icon = icon("balance-scale"),
                          dashboardBody(
                              fluidRow(
                                  column(4, br(),align="center"),
                                  
                                  column(
                                      h1 (p("Brecha de género"
                                            ,style="color:white;text-align:center")),
                                      
                                      width=4,style="background-color:#360729;border-radius: 0px"),
                                  column(12, br(),align="center"),
                                  
                                  column(3, br(),align="center"),
                                  column(
                                      h3 (p("Las disparidades de ingreso entre los hombes y las mujeres en el mercado laboral han sido sistemáticos a lo largo del tiempo.
                                            De la misma forma, existen brechas importantes en la participción en el mercado laboral y la formalidad entre hombres y mujeres."
                                            ,style="color:black;text-align:justify")),
                                      
                                      width=6,style="background-color:#ffffff;border-radius: 10px"),
                                  column(12, br(),align="center"),
                                  column(3, br(),align="center"),
                                  column(
                                      h3 (p("En esta sección se presenta una descripción de la brecha salarial de género en el mercado laboral mexicano, así como en la
                                            participación en el mercado laboral y la condición de formalidad entre hombres y mujeres. Para ello, se utilizaron datos de
                                            la Encuesta Nacional de Ocupación y Empleo (ENOE) entre los años 2005 y 2021. En el caso de la brecha salarial de genero se 
                                            utilizaron regresiones cuantílicas y el método de descomposición Oaxaca-Blinder. Para el caso de la brecha en particiáción 
                                            y empleo se utilizaron modelos logísticos de regresión."
                                            ,style="color:black;text-align:justify")),
                                      
                                      width=6,style="background-color:#ffffff;border-radius: 10px"),
                                  
                              
                                  column(12, br(),align="center"),
                                  
                                  
                                  
                                  tabsetPanel(
                                    
                                   tabPanel(""),
                                      
                                  tabPanel("Brecha salarial",
                                  
                                      navlistPanel(widths=c(1,11),fluid = T,well = T,
                                    
                                      tabPanel("A lo largo de la distribución",
                                               
                                               fluidRow(column(
                                                   fluidRow(
                                                       sidebarLayout(
                                                           sidebarPanel(width = 2,
                                                               sliderInput('anio','Seleccione un año', 2005, 2021, 2007, step=2, animate = TRUE),
                                                               sliderInput('quant','Seleccione un cuantil', 1, 99, 2, step=1, animate = TRUE),
                                                           ),
                                                           mainPanel(width = 10,
                                                             column(
                                                                 h3 (p("La brecha salarial no es homogénea a lo largo de la distribución de ingresos,
                                                                       siendo esta más fuerte en la parte de la distribución (Piso pegajoso).
                                                                       En la parte superior de la distribución se observa que los ingresos no suelen
                                                                       ser mayores a los de los hombres (techo de cristal)."
                                                                       ,style="color:white;text-align:justify")),
                                                                         
                                                                         width=12,style="background-color:#360729;border-radius: 10px"),
                                                               column(6, br(),align="center",plotlyOutput("plot6")),
                                                               column(6, br(),align="center",plotlyOutput("plot7")),
                                                             
                                                           )
                                                       ),   
                                                       
                                                   ),width=11,
                                                   column(2, br(),align="center"),
                                                   column(
                                                       h3 (p("En promedio, la brecha salarial de género ha disminuido, pero la disminución no ha sido 
                                                             homogénea, siendo ésta mayor entre las trabajadoras de mayores ingresos."
                                                             ,style="color:white;text-align:justify")),
                                                       
                                                       width=10,style="background-color:#360729;border-radius: 10px"),
                                                   column(2, br(),align="center"),
                                                   column(10, br(),align="center",plotlyOutput("plot8")),
                                                   column(2, br(),align="center"),
                                                   column(10, br(),align="center",plotlyOutput("plot9")),
                                                   )),),
                                      
                                      tabPanel("Entidades",
                                               
                                               fluidRow(column(
                                                   fluidRow(
                                                   
                                                   sidebarLayout(
                                                     sidebarPanel(width = 2,
                                                         selectInput ('region','Seleccione una entidad', unique(quant_region$name_reg)),
                                                         sliderInput('regiondate','Seleccione un año', 2005, 2021, 2005, step=2, animate = TRUE),
                                                         sliderInput('cuants2','Seleccione un cuantil', 1, 99, 50, step=1, animate = TRUE),
                                                         
                                                     ) ,
                                             mainPanel(width = 10,
                                                       column(
                                                           h3 (p("La brecha salarial tampoco es homogénea entre entidades. Si bien se 
                                                           pueden observar los fenómenos de piso pegajoso y techo de cristal, existe
                                                           una gran disparidad en la dispersión y en la evolución."
                                                                 ,style="color:white;text-align:justify")),
                                                           
                                                           width=12,style="background-color:#360729;border-radius: 10px"),
                                                         column(6, br(),align="center",plotlyOutput("plot28")),
                                                         column(6, br(),align="center",plotlyOutput("plot30")),
                                                         column(1, br(),align="center"),
                                                         column(5, br(),align="center",plotlyOutput("plot29")),
                                                         column(5, br(),align="center",plotlyOutput("plot31")),
                                                     ),
                                                   ),
                                               ),width= 11),
                                      ),),
                                      tabPanel("Tablas",
                                               
                                               sliderInput('aniot','Seleccione un año', 2005, 2021, 2007, step=2, animate = TRUE),
                                               box(
                                                   title = "GENERAL", width = 12, background = "light-blue",
                                               ),
                                               DT::DTOutput('table'),
                                               
                                               sliderInput('aniot2','Seleccione un año', 2005, 2021, 2007, step=2, animate = TRUE),
                                               box(
                                                   title = "REGIONES", width = 12, background = "light-blue",
                                               ),
                                               DT::DTOutput('table1')
                                      )
                                  )),
                                    
                                  tabPanel("Descomposición de la brecha",
                                           navlistPanel(widths=c(1,11),fluid = T,well = T,
                                     
                                      tabPanel("General",
                                               column(
                                                   h3 (p("La brecha salarial puede descomponerse en dos componentes: 1) El asociado con 
                                                         las habilidades (Explained); y 2) El asociado con la discriminación de género (Unexplained).
                                                         La brecha de género se explica mayormente por el componente asociado a la discriminación.
                                                         "
                                                         ,style="color:white;text-align:justify")),
                                                   
                                                   width=12,style="background-color:#360729;border-radius: 10px"),
                                               column(1, br(),align="center"),
                                               column(5, br(),align="center",plotlyOutput("oax2")),
                                               column(5, br(),align="center",plotlyOutput("oax99")),
                                               column(
                                                   h3 (p("La disminución de la brecha está conducida por una disminución del componente asociado
                                                   a la discriminación, más que por el aumento en el componente asociado a las habilidades.
                                                         "
                                                         ,style="color:white;text-align:justify")),
                                                   
                                                   width=12,style="background-color:#360729;border-radius: 10px"),
                                               column(1, br(),align="center"),
                                               column(10, br(),align="center",plotlyOutput("oax100")),
                                               box(
                                                   title = "Tabla", width = 12, background = "light-blue",
                                               ),
                                               DT::DTOutput('tabl2')
                                               ,),
                                      tabPanel("Entidades",
                                               
                                               
                                       fluidRow(column(
                                           fluidRow(
                                               sidebarLayout(
                                                   sidebarPanel(width = 2,
                                                        selectInput ('Estado','Seleccione una entidad', unique(oax_ent$Estado)),
                                                        sliderInput('Year','Seleccione un año', 2005, 2021, 2005, step=1, animate = TRUE),             
                                                                
                                                   ) ,
                                                   mainPanel(width = 10,
                                                             column(
                                                                 h3 (p("Existe una fuerte heterogeneidad de la brecha y sus componentes entre
                                                                       entidades, de tal manera que incluso hay entidades con una brecha negativa,
                                                                       es decir que las mujeres percben, en promdio, un mayor ingreso que los hombres."
                                                                       ,style="color:white;text-align:justify")),
                                                                 
                                                                 width=12,style="background-color:#360729;border-radius: 10px"),
                                                             
                                                     column(4, br(),align="center",plotlyOutput("plot3")),
                                                     column(4, br(),align="center",plotlyOutput("plot5")),
                                                     column(4, br(),align="center",plotlyOutput("plot4")),
                                                     
                                                     column(6, br(),align="center",plotlyOutput('plot')),
                                                     column(6, br(),align="center",plotlyOutput("plot2")),
                                                     
                                                   ),
                                               ),
                                           ),width= 11),
                                       ),
                                               
                                               ),
                                      tabPanel("Mapas",
                                               
                                           fluidRow(column(
                                               fluidRow(
                                                   sidebarLayout(
                                                       sidebarPanel(width = 2,
                                                        sliderInput('maps1','Seleccione un año', 2005, 2021, 2005, step=2, animate = TRUE),                       
                                                                    
                                                       ) ,
                                                       mainPanel(width = 10,
                                                         column(12, br(),align="center",plotlyOutput("map3")),
                                                         column(6, br(),align="center",plotlyOutput("map1")),
                                                         column(6, br(),align="center",plotlyOutput("map2")),
                                                         DT::DTOutput('tabl3')
                                                       ),
                                                   ),
                                               ),width= 11),
                                           ),     
                                               
                                               
                                               
                                               
                                      )
                                  )),
                                
                                tabPanel("Participación y formalidad",
                                     navlistPanel(widths=c(1,11),fluid = T,well = T,
                                    
                                    
                                    tabPanel("Participación",
                                     fluidRow(column(
                                         fluidRow(
                                             sidebarLayout(
                                                 sidebarPanel(width = 2,
                                                    sliderInput('pls','Seleccione un año', 2005, 2021, 2005, step=1, animate = TRUE),                                 
                                                              
                                                 ) ,
                                                 mainPanel(width = 10,
                                                           column(
                                                               h3 (p("Las mujeres con pareja tienen una probabilidad menor de participar en el mercado 
                                                                     laboral que las mujeres sin pareja, lo que puede responder a ciertos roles sociales.
                                                                     En contraste, las mujeres con hijos tienen más probabilidades de participar en el
                                                                     el mercado laboral uqe las mujeres sin hijos."
                                                                     ,style="color:white;text-align:justify")),
                                                               
                                                               width=12,style="background-color:#360729;border-radius: 10px"),
                                                           
                                                           column(12, br(),align="center",plotlyOutput("plot12")),
                                                           column(6, br(),align="center",plotlyOutput('plot10')),
                                                           column(6, br(),align="center",plotlyOutput("plot11")),
                                                           
                                                 ),
                                             ),
                                         ),width= 11),
                                     ),      
                                    
                                      
                                     fluidRow(column(
                                         fluidRow(
                                             sidebarLayout(
                                                 sidebarPanel(width = 2,
                                                  sliderInput('formalix3','Seleccione un año', 2005, 2021, 2005, step=1, animate = TRUE),
                                                 ) ,
                                                 mainPanel(width = 10,
                                               column(
                                                   h3 (p("Las mujeres, en general, tienen menos probabilidades de participar en el mercado que los hombres
                                                         solteros. En contraste, los hombres con pareja tienen cerca del doble de probabilidades de participar
                                                         en el mercado que los hombres solteros."
                                                         ,style="color:white;text-align:justify")),
                                                   
                                                   width=12,style="background-color:#360729;border-radius: 10px"),
                                               
                                                   column(6, br(),align="center",plotlyOutput('plot19')),
                                                   column(6, br(),align="center",plotlyOutput('plot18')),          
                                                           
                                                 ),
                                             ),
                                         ),width= 11),
                                     ),
                                ),
                                tabPanel("Formalidad",
                                         column(12, br(),align="center"),        
                                 fluidRow(column(
                                     fluidRow(
                                         sidebarLayout(
                                             sidebarPanel(width = 2,
                                              sliderInput('formalix','Seleccione un año', 2005, 2021, 2005, step=1, animate = TRUE),        
                                             ) ,
                                             mainPanel(width = 10,
                                           column(
                                               h3 (p("Las mujeres, con pareja tienen menos probabilidades de ser formales que las mujeres sin pareja.
                                                     Por otra parte, las mujeres con hijos tienen probabilidades cercanas de ser formalles que las mujeres
                                                     sin hijos."
                                                     ,style="color:white;text-align:justify")),
                                               
                                               width=12,style="background-color:#360729;border-radius: 10px"),
                                                       
                                               column(12, br(),align="center",plotlyOutput("plot15")),
                                               column(6, br(),align="center",plotlyOutput('plot13')),
                                               column(6, br(),align="center",plotlyOutput("plot14")),

                                             ),
                                         ),
                                     ),width= 11),
                                 ),
                                
                                 fluidRow(column(
                                     fluidRow(
                                         sidebarLayout(
                                             sidebarPanel(width = 2,
                                                          
                                              sliderInput('formalix2','Seleccione un año', 2005, 2021, 2005, step=1, animate = TRUE),
                                                          
                                             ) ,
                                             mainPanel(width = 10,
                                                       column(
                                                           h3 (p("Las mujeres, con pareja tienen menos probabilidades de ser formales que los hombres solteros.
                                                     Por otra parte, las mujeres sin pareja y los hombres con pareja tienen  mayores probabilidades de ser 
                                                     formalles que los hombres solteros."
                                                                 ,style="color:white;text-align:justify")),
                                                           
                                                           width=12,style="background-color:#360729;border-radius: 10px"),
                                                       
                                               column(6, br(),align="center",plotlyOutput('plot17')),
                                               column(6, br(),align="center",plotlyOutput('plot16')),
                                            
                                             ),
                                         ),
                                     ),width= 11),
                                 ),
                                
                               
                                
                                ),
                                tabPanel("Entidades",
                                         
                                 box(
                                     title = "Probabilidad de participación en el mercado laboral", width = 12, background = "light-blue",
                                 fluidRow(column(
                                     fluidRow(
                                         
                                         sidebarLayout(
                                             sidebarPanel(width = 2,
                                              selectInput ('entidx','Seleccione una entidad', unique(formal_ent$av_ent)),
                                              sliderInput('formalixxxx','Seleccione un año', 2005, 2021, 2005, step=1, animate = TRUE),

                                             ) ,
                                             mainPanel(width = 10,
                                               column(
                                                   h3 (p("Probabilidad de participación en el mercado laboral"
                                                         ,style="color:white;text-align:center")),
                                                   
                                                   width=12,style="background-color:#360729;border-radius: 10px"),
                                                       
                                               column(6, br(),align="center",plotlyOutput('plot25')),
                                               column(6, br(),align="center",plotlyOutput('plot24')),
                                               column(6, br(),align="center",plotlyOutput('plot27')),
                                               column(6, br(),align="center",plotlyOutput('plot26')),

                                             ),
                                         ),
                                     ),width= 11),
                                 ),  
                                 box(
                                     title = "Probabilidad de formalidad", width = 12, background = "light-blue",
                                 ), 
                                 fluidRow(column(
                                     fluidRow(
                                         
                                         sidebarLayout(
                                             sidebarPanel(width = 2,
                                                  selectInput ('entid','Seleccione una entidad', unique(formal_ent$av_ent)),
                                                  sliderInput('formalixxx','Seleccione un año', 2005, 2021, 2005, step=1, animate = TRUE),

                                             ) ,
                                             mainPanel(width = 10,
                                               column(
                                                   h3 (p("Probabilidad de formalidad"
                                                         ,style="color:white;text-align:center")),
                                                   
                                                   width=12,style="background-color:#360729;border-radius: 10px"),
                                                       
                                               column(6, br(),align="center",plotlyOutput('plot21')),
                                               column(6, br(),align="center",plotlyOutput('plot20')),
                                               column(6, br(),align="center",plotlyOutput('plot23')),
                                               column(6, br(),align="center",plotlyOutput('plot22')),

                                             ),
                                         ),
                                     ),width= 11),
                                 ),     
                                 
                                     
                                 ), 
                                 
                                 
                                 
                                ),
                                
                        tabPanel("Mapas",
                                 navlistPanel(widths=c(1,11),fluid = T,well = T,
                                 
                                 tabPanel("Participación",
                                          
                                          fluidRow(column(
                                              fluidRow(
                                                  sidebarLayout(
                                                      sidebarPanel(width = 2,
                                                                   selectInput ('partmap','Seleccione un año', unique(mappart$y)),
                                                                   selectInput ('partmapq','Seleccione un trimestre', unique(mappart$q)),
                                                      ) ,
                                                      mainPanel(width = 10,
                                                                column(3, br()),
                                                                column(
                                                                    h3 (p("Probabilidad respecto a otras mujeres"
                                                                          ,style="color:white;text-align:center")),
                                                                    
                                                                    width=12,style="background-color:#360729;border-radius: 10px"),
                                                                column(6, br(),align="center",plotlyOutput('map8')),
                                                                column(6, br(),align="center",plotlyOutput('map9')),
                                                                column(3, br()),
                                                                column(
                                                                    h3 (p("Probabilidad respecto a hombres solteros"
                                                                          ,style="color:white;text-align:center")),
                                                                    
                                                                    width=12,style="background-color:#360729;border-radius: 10px"),
                                                                
                                                                column(6, br(),align="center",plotlyOutput('map4')),
                                                                column(6, br(),align="center",plotlyOutput('map5')),
                                                                column(6, br(),align="center",plotlyOutput('map6')),
                                                                
                                                      ),
                                                  ),
                                              ),width= 11),
                                          ),
                                          
                                          
                                          ),
                                 tabPanel("Formalidad",
                                          
                                          fluidRow(column(
                                              fluidRow(
                                                  sidebarLayout(
                                                      sidebarPanel(width = 2,
                                                                   
                                                                   selectInput ('formap','Seleccione un año', unique(mapfor22$y)),
                                                                   selectInput ('formapq','Seleccione un trimestre', unique(mapfor22$q)),
                                                      ) ,
                                                      mainPanel(width = 10,
                                                                column(3, br()),
                                                                column(
                                                                    h3 (p("Probabilidad respecto a otras mujeres"
                                                                          ,style="color:white;text-align:center")),
                                                                    
                                                                    width=12,style="background-color:#360729;border-radius: 10px"),
                                                                column(6, br(),align="center",plotlyOutput('map12')),
                                                                column(6, br(),align="center",plotlyOutput('map13')),
                                                                column(3, br()),
                                                                column(
                                                                    h3 (p("Probabilidad respecto a hombres solteros"
                                                                          ,style="color:white;text-align:center")),
                                                                    
                                                                    width=12,style="background-color:#360729;border-radius: 10px"),
                                                                column(6, br(),align="center",plotlyOutput('map7')),
                                                                column(6, br(),align="center",plotlyOutput('map10')),
                                                                column(6, br(),align="center",plotlyOutput('map11')),
                                                                
                                                      ),
                                                  ),
                                              ),width= 11),
                                          ),
                                          
                                          
                                 )),
                        
                                 
                                 
                                 ),
                                
                                ),))),
                          
                      )
                      )
             
             

),

))
