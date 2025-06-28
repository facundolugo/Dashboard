# app.R

#Instalar paquetes (solo una vez)
# install.packages("shiny")          # Para crear apps web interactivas
# install.packages("shinydashboard") # Para el diseno de dashboards
# install.packages("ggplot2")        # Para crear graficos
# install.packages("dplyr")          # Para manipular datos
# install.packages("readr")          # Para leer archivos (ej. CSV)

#Cargar librerias (necesario cada vez que corremos la app)
library(shiny)         # Carga Shiny
library(shinydashboard) # Carga Shinydashboard
library(ggplot2)       # Carga ggplot2
library(dplyr)         # Carga dplyr
library(readr)         # Carga readr

#Preparacion de Datos
#'ventas_pizzeria.csv' tiene que estar en la misma carpeta que 'app.R'
datos_ventas <- read_csv("ventas_pizzeria.csv")

#Convertimos la columna 'Monto ($)' a numerica.
datos_ventas$`Monto ($)` <- as.numeric(datos_ventas$`Monto ($)`)

# Convertimos 'Estado Cliente' a factor (para graficos de categorias)
datos_ventas$`Estado Cliente` <- as.factor(datos_ventas$`Estado Cliente`)

#Definicion de la Interfaz de Usuario (UI)
ui <- dashboardPage(
  # Header: barra superior del dashboard
  dashboardHeader(title = "El Horno al Dia"),
  
  #Sidebar: barra de navegacion lateral
  dashboardSidebar(
    sidebarMenu(
      #MenuItem para Dashboard Principal
      menuItem(
        "Dashboard", #Texto del menu
        tabName = "dashboard" #Nombre interno de la pestana
      ),
      #MenuItem para Storytelling
      menuItem(
        "Storytelling", #Texto del menu
        tabName = "storytelling" #Nombre interno de la pestana
      )
    )
  ),
  
  #Body: area principal de contenido
  dashboardBody(
      tabItems(
      #Contenido de la pestana "Dashboard"
      tabItem(tabName = "dashboard",
              h2("Panel de Control de la Pizzeria"), #Titulo de la seccion
              
              #Fila para los Value Boxes (KPIs)
              fluidRow(
                valueBoxOutput("totalVentasBox", width = 4), #Output para total de ventas
                valueBoxOutput("montoTotalBox", width = 4),  #Output para monto total
                valueBoxOutput("clientesContentosBox", width = 4) #Output para clientes contentos
              ),
              
              #Fila para los graficos
              fluidRow(
                #Box para grafico de Pizzas Mas Vendidas
                box(
                  title = "Pizzas Mas Vendidas", #Titulo de la caja
                  status = "primary", #Color del encabezado de la caja
                  solidHeader = TRUE, #Encabezado solido
                  collapsible = TRUE, #Permite minimizar/maximizar
                  width = 6, #Ancho de la caja (6 de 12 columnas)
                  plotOutput("ventasProductoPlot") #Output del grafico de barras
                ),
                
                #Box para grafico de Satisfaccion del Cliente
                box(
                  title = "Proporcion de Satisfaccion de Clientes", #Titulo de la caja
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  plotOutput("satisfaccionClientePlot") #Output del grafico de torta
                )
              )
      ),
      
      #Contenido de la pestaña "Storytelling"
      tabItem(tabName = "storytelling",
              h2("Storytelling: El Horno al Dia"), #Titulo de la seccion
              
              fluidRow(
                box(
                  title = "Proposito y Objetivos del Dashboard", #Titulo de la caja
                  status = "info", #Color del encabezado de la caja, "info" = "celeste"
                  solidHeader = TRUE,
                  width = 12, #Ancho completo de la fila
                  collapsible = TRUE,
                  #Parrafos de texto para el storytelling
                  p("Este tablero es el cerebro de nuestra pizzeria. Nos va a mostrar rapido como va el negocio. La idea es simple: ver los numeros clave para que podamos decidir mejor y seguir creciendo."),
                  p("Aca vamos a ver cuales son las pizzas que mas salen, las que la gente pide una y otra vez. Asi sabemos donde ponerle mas fichas y que ingredientes no pueden faltar. Tambien te mostramos cuantas pizzas vendimos en total y cuanta plata entro."),
                  p("Y porque lo mas importante es que nuestros clientes se vayan felices, tambien te mostraremos que tan contenta esta la gente. Veremos un resumen simple de cuantos clientes estan satisfechos y cuantos no.")
                )
              )
      )
    )
  )
)

#Definicion del Servidor (Server)
server <- function(input, output) {
  
  #Renderizar Value Box: Total de Ventas
  output$totalVentasBox <- renderValueBox({
    total_ventas <- nrow(datos_ventas) #Calcular total de filas
    valueBox(
      total_ventas, "Numero Total de Ventas",
      icon = icon("shopping-cart")
    )
  })
  
  #Renderizar Value Box: Monto Total Recaudado
  output$montoTotalBox <- renderValueBox({
    monto_total <- sum(datos_ventas$`Monto ($)`, na.rm = TRUE) # Sumar montos
    valueBox(
      paste0("$", formatC(monto_total, format="f", big.mark=".", digits=0)),
      "Monto Total Recaudado",
      icon = icon("dollar-sign")
    )
  })
  
  #Renderizar Value Box: Porcentaje de Clientes Contentos
  output$clientesContentosBox <- renderValueBox({
    contentos <- sum(datos_ventas$`Estado Cliente` == "Contento", na.rm = TRUE) #Contar contentos
    total_clientes_evaluados <- sum(!is.na(datos_ventas$`Estado Cliente`)) #Contar evaluados
    porcentaje_contentos <- ifelse(total_clientes_evaluados > 0, round((contentos / total_clientes_evaluados) * 100, 1), 0) #Calcular %
    valueBox(
      paste0(porcentaje_contentos, "%"),
      "Clientes Contentos",
      icon = icon("grin-alt")
    )
  })
  
  #Renderizar Grafico: Pizzas Mas Vendidas
  output$ventasProductoPlot <- renderPlot({
    datos_ventas %>%
      count(`Producto Vendido`, name = "Cantidad Vendida") %>% #Contar ventas por producto
      ggplot(aes(x = reorder(`Producto Vendido`, -`Cantidad Vendida`),
                 y = `Cantidad Vendida`,
                 fill = `Producto Vendido`)) +
      geom_bar(stat = "identity") + #Crear barras
      geom_text(aes(label = `Cantidad Vendida`), vjust = -0.5, size = 5) + #Etiquetas en barras
      labs(title = "Cantidad de Ventas por Tipo de Pizza", x = "Tipo de Pizza", y = "Cantidad Vendida") +
      theme_minimal() + #Tema limpio
      theme(
        legend.position = "none", # Sin leyenda
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12), #Rotar etiquetas X
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), #Titulo centrado y negrita
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12)
      ) +
      scale_fill_brewer(palette = "Set2") #Paleta de colores para barras
  })
  
  #Renderizar Grafico: Satisfaccion del Cliente (Pastel)
  output$satisfaccionClientePlot <- renderPlot({
    datos_satisfaccion <- datos_ventas %>%
      count(`Estado Cliente`, name = "Conteo") %>% #Contar estado cliente
      mutate(Porcentaje = Conteo / sum(Conteo) * 100) #Calcular porcentajes
    
    ggplot(datos_satisfaccion, aes(x = "", y = Porcentaje, fill = `Estado Cliente`)) +
      geom_bar(stat = "identity", width = 1) + # Base del pastel
      coord_polar("y", start = 0) + # Convertir a pastel
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")),
                position = position_stack(vjust = 0.5), size = 6, color = "black") + #Etiquetas de %
      labs(title = "Proporcion de Satisfaccion de Clientes", fill = "Estado") +
      theme_void() + #Tema vacio para pastel (Quita los ejes y el fondo gris para dejarlo mas visible)
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      ) +
      scale_fill_manual(values = c("Contento" = "green", "No contento" = "red"))
  })
  
}

#Corremos la Aplicacion Shiny
shinyApp(ui = ui, server = server)