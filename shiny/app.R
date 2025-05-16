library(shiny)
library(shinydashboard)
library(leaflet)
library(wordcloud2)
library(plotly)
library(DT)
library(readxl)
library(tidygeocoder)
library(tidyverse)
library(tidytext)
library(widyr)
library(stopwords)
library(textclean)
library(igraph)
library(ggraph)
library(httr)
library(jsonlite)

api_key <- "1691cc24"
serie_id <- "tt27740241" # ← tu API key de OMDb

# ---- Datos de la serie (OMDb API) ----
url_serie <- paste0("http://www.omdbapi.com/?i=", serie_id, "&apikey=", api_key)
res_serie <- GET(url_serie)
datos_serie <- fromJSON(content(res_serie, "text", encoding = "UTF-8"))

rating_general <- as.numeric(datos_serie$imdbRating)
temporadas_total <- as.numeric(datos_serie$totalSeasons)
fecha_estreno <- datos_serie$Released
# ---- Obtener detalles completos de cada episodio ----
get_episode_data <- function(season, episode) {
  url <- paste0("http://www.omdbapi.com/?i=", serie_id,
                "&Season=", season, "&Episode=", episode,
                "&apikey=", api_key)
  
  res <- GET(url)
  data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  
  imdb_rating <- ifelse(data$imdbRating == "N/A", NA, as.numeric(data$imdbRating))
  imdb_votes  <- ifelse(data$imdbVotes == "N/A", NA, as.integer(gsub(",", "", data$imdbVotes)))
  plot_text   <- ifelse(data$Plot == "N/A", NA, data$Plot)
  
  tibble(
    Episode = episode,
    Title = data$Title,
    Released = data$Released,
    imdbRating = imdb_rating,
    imdbVotes = imdb_votes,
    Plot = plot_text
  )
}

# Si querés podés cambiar esto por: 1:temporadas_total si hay varias
temporada <- 1
episodios <- 1:6

episodios_df <- map_df(episodios, ~get_episode_data(temporada, .x))
# ---- Stopwords en español ----
sw_es <- stopwords("es") %>%
  str_to_lower() %>%
  replace_non_ascii() %>%
  unique()

stopwords_extra <- c("oh", "uh", "eh", "sos", "vos","hace","dale","vez","vas","bien","van","aca","acá",
                     "hacer","ustedes","ah","va","veni","ver","ir","ahora", "asi","alla","ey","da","ay",
                     "ser","tan","puede","vi","despues","okey","ahi","años","anos","superpone","musica",
                     "distorsionada", "mira", "voy", "tenes")
sw_es <- unique(c(sw_es, stopwords_extra))

# ---- Subtítulos tokenizados ----
archivos_srt <- list.files(pattern = "\\.srt$")
subtitulos_raw <- map_dfr(archivos_srt, function(archivo) {
  lineas <- read_lines(archivo)
  tibble(
    archivo = archivo,
    linea = seq_along(lineas),
    texto = lineas
  )
})

# Limpiar el texto
subtitulos_limpios <- subtitulos_raw %>%
  mutate(
    texto = texto %>%
      str_to_lower() %>%
      replace_non_ascii() %>%
      str_remove_all("\\[.*?\\]") %>%
      str_replace_all("[^[:alpha:]]", " ") %>%
      str_squish()
  )

subtitulos_limpios <- subtitulos_limpios %>%
  mutate(bloque = (linea - 1) %/% 10 + 1)

tokens_con_id <- subtitulos_limpios %>%
  unnest_tokens(palabra, texto) %>%
  filter(!palabra %in% sw_es) %>%
  filter(str_detect(palabra, "[a-z]")) %>%
  select(archivo, bloque, palabra)

# ---- Noticias y TF-IDF ----
df <- read_excel("noticias_texto.xlsx")

palabras_noticias <- df %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% sw_es) %>%
  filter(str_detect(word, "^[a-záéíóúñ]+$"))

tf_idf_medio <- palabras_noticias %>%
  count(Medio, word) %>%
  bind_tf_idf(word, Medio, n)

comparar_medios <- function(medio1, medio2) {
  posicion <- position_jitter(width = 0.1, height = 0.1, seed = 123)
  medio1_sym <- rlang::sym(medio1)
  medio2_sym <- rlang::sym(medio2)
  
  tf_idf_medio %>%
    filter(Medio %in% c(medio1, medio2)) %>%
    select(Medio, word, tf_idf) %>%
    pivot_wider(names_from = Medio, values_from = tf_idf) %>%
    replace_na(setNames(list(0, 0), c(medio1, medio2))) %>%
    mutate(across(all_of(c(medio1, medio2)), ~ifelse(. == 0, 0.0001, .))) %>%
    ggplot(aes(x = !!medio1_sym, y = !!medio2_sym)) +
    geom_jitter(aes(color = abs(!!medio1_sym - !!medio2_sym)), 
                alpha = 0.3, size = 1.5, position = posicion) +
    ggrepel::geom_text_repel(aes(label = word), max.overlaps = 20, 
                             position = posicion, size = 3) +
    scale_x_log10() +
    scale_y_log10() +
    scale_color_gradient(high = "purple3", low = "darkgrey", guide = FALSE) +
    geom_abline(color = "red", linetype = 2) +
    labs(title = paste("Comparación TF-IDF de palabras entre", medio1, "y", medio2),
         x = medio1, y = medio2) +
    theme_minimal()
}

# ---- Correlaciones de palabras y grafo ----
pares_corr <- tokens_con_id %>%
  group_by(palabra) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  pairwise_cor(palabra, bloque, sort = TRUE)

pares_frec <- tokens_con_id %>%
  group_by(archivo, bloque) %>%
  summarise(linea = min(bloque)) %>%
  ungroup() %>%
  inner_join(tokens_con_id, by = c("archivo", "bloque")) %>%
  pairwise_count(palabra, bloque, sort = TRUE)

pares_filtrados <- pares_frec %>% filter(n >= 4)

palabras_correlacionadas <- function(palabra_objetivo, pares_corr_df, min_correlacion = 0.2, top_n = 10) {
  pares_corr_df %>%
    filter(item1 == palabra_objetivo, correlation >= min_correlacion) %>%
    slice_max(correlation, n = top_n)
}

# ---- Datos del mapa ----
addResourcePath("src", "src")
mapa_data <- read_excel("mapa_data.xlsx")

mapa_geo <- mapa_data %>%
  rename(direccion = Dirección, nota = Nota, imagen = Path) %>%
  geocode(address = direccion, method = "osm", lat = lat, long = lon) %>%
  mutate(
    imagen_rel = paste0("src/", basename(imagen)),
    popup_html = paste0(
      "<b>", direccion, "</b><br>",
      "<i>", nota, "</i><br><br>",
      "<img src='", imagen_rel, "' width='200px'>"
    )
  )

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "El Eternauta - Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "home", icon = icon("home")),
      menuItem("Localizaciones", tabName = "mapa", icon = icon("map")),
      menuItem("Guión", tabName = "guion", icon = icon("file-alt")),
      menuItem("Estadísticas", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Prensa", tabName = "prensa", icon = icon("newspaper")),
      menuItem("+ Info", tabName = "info", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      /* TIPOGRAFÍA GENERAL */
      body, .content-wrapper, .box, .sidebar, .main-header, .tab-content {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }

      /* Fondo e imagen en la home */
      #home_bg {
        position: relative;
        width: 100%;
        height: 100vh;
        background-image: url('src/portada.jpg');
        background-size: cover;
        background-position: center center;
        background-repeat: no-repeat;
        display: flex;
        justify-content: center;
        align-items: center;
        color: white;
      }

      #home_content {
        background-color: rgba(0, 0, 0, 0.6);
        padding: 30px;
        border-radius: 10px;
        max-width: 800px;
        text-align: center;
        box-shadow: 0 0 15px rgba(0,0,0,0.8);
      }

      /* HEADER - azul noche */
      .skin-blue .main-header .navbar {
        background-color: #003366;
      }

      /* SIDEBAR - azul más oscuro */
      .skin-blue .main-sidebar {
        background-color: #002244;
      }

      /* Ítem activo en sidebar */
      .skin-blue .sidebar-menu > li.active > a {
        background-color: #004080;
        color: #ffffff;
      }

      /* Texto del sidebar */
      .skin-blue .sidebar a {
        color: #cccccc;
      }

      /* Hover en ítems del sidebar */
      .skin-blue .sidebar-menu > li > a:hover {
        background-color: #004080;
      }
    "))
    ),
    tabItems(
      tabItem(tabName = "home",
              div(id = "home_bg",
                  div(id = "home_content",
                      h3("Descifrando El Eternauta: Una aventura de datos y mensajes"),
                      p("Este dashboard explora la localización, los diálogos, puntajes y repercusión de la serie.")
                  )
              )
      ),
      
      tabItem(tabName = "mapa",
              fluidRow(
                box(width = 12, title = "Mapa de localizaciones", solidHeader = TRUE,
                    p("Este mapa muestra las localizaciones reales que aparecen en la serie 'El Eternauta'. Al hacer clic en cada marcador, podrás ver una breve descripción sobre cómo ese lugar se relaciona con la historia."),
                    leafletOutput("mapa_lugares", height = 500)
                )
              )
      ),
      
      tabItem(tabName = "guion",
              fluidRow(
                box(width = 12,
                    tabBox(width = 12,
                           title = "Análisis del Guión",
                           id = "tabset_guion",
                           
                           tabPanel("Información",
                                    fluidRow(
                                      column(8,
                                             h4("Descripción del análisis"),
                                             p("Esta sección está dedicada al análisis textual de los subtítulos de la serie 'El Eternauta'."),
                                             p("Se descargaron los subtítulos completos y se procesaron para eliminar signos de puntuación, convertir todo a minúsculas y quitar las palabras vacías o frecuentes del español (stopwords)."),
                                             p("El objetivo es explorar el uso del lenguaje en la narrativa, identificar patrones, palabras clave y relaciones entre términos relevantes."),
                                             p("Este análisis permite visualizar la evolución semántica del guión, las palabras que predominan y los vínculos conceptuales más fuertes.")
                                      ),
                                      column(4,
                                             tags$img(
                                               src = "src/subtitulos.jpg",
                                               width = "100%",
                                               style = "border-radius: 10px; box-shadow: 0 0 8px rgba(0,0,0,0.5);"
                                             )
                                      )
                                    )
                           ),
                           
                           tabPanel("Palabras frecuentes",
                                    h4("Relaciones entre palabras"),
                                    p("En esta sección se puede explorar cómo ciertas palabras del guión están estadísticamente relacionadas con otras."),
                                    p("Al ingresar una palabra, se muestra un gráfico con aquellas que suelen aparecer en los mismos bloques del guión."),
                                    p("Esto permite identificar asociaciones temáticas, repeticiones narrativas o énfasis conceptuales dentro del texto."),
                                    textInput("input_palabra", "Palabra:", value = "juan"),
                                    actionButton("btn_buscar", "Buscar"),
                                    br(), br(),
                                    plotlyOutput("barras_palabras_relacionadas")
                           ),
                           
                           tabPanel("Relaciones entre palabras",
                                    h4("Grafo de coocurrencias"),
                                    p("El siguiente grafo muestra cómo se conectan las palabras más relevantes del guión entre sí."),
                                    p("Cada nodo representa una palabra, y los vínculos indican que estas palabras aparecen frecuentemente en el mismo contexto o bloque de texto."),
                                    p("Este tipo de visualización ayuda a descubrir estructuras narrativas implícitas, personajes centrales o temas recurrentes."),
                                    tags$img(src = "src/grafo_general.png", width = "100%")
                           )
                    )
                )
              )
      ),
      
      tabItem(tabName = "stats",
              fluidRow(
                valueBoxOutput("kpi_rating"),
                valueBoxOutput("kpi_temporadas"),
                valueBoxOutput("kpi_estreno")
              ),
              fluidRow(
                box(width = 12, title = "Episodios - Puntajes y Reseñas", solidHeader = TRUE,
                    DT::dataTableOutput("tabla_episodios"))
              )
      ),
      
      tabItem(tabName = "prensa",
              fluidRow(
                column(width = 12,
                       box(width = 12, title = "Diferencias de palabras (TF-IDF)",
                           p("Seleccioná dos medios de comunicación utilizando los desplegables a continuación para comparar las palabras más características de cada uno."),
                           p("El análisis utiliza la métrica TF-IDF para resaltar las palabras distintivas de cada medio en sus artículos sobre 'El Eternauta'."),
                           p("Cada punto del gráfico representa una palabra: si está cerca de una de las esquinas, indica un uso diferencial entre los medios; si está cerca de la diagonal, es una palabra compartida con similar intensidad."),
                           
                           div(style = "display: flex; align-items: center; gap: 15px; margin-top: 15px; margin-bottom: 10px;",
                               div(style = "flex: 1;",
                                   selectInput("medio1", "Medio 1", choices = unique(df$Medio), selected = unique(df$Medio)[1])
                               ),
                               div(style = "flex: 1;",
                                   selectInput("medio2", "Medio 2", choices = unique(df$Medio), selected = unique(df$Medio)[2])
                               ),
                               div(style = "flex: 0 0 auto;",
                                   actionButton("btn_comparar", "Graficar", icon = icon("chart-line"))
                               )
                           ),
                           plotOutput("grafico_tf_idf", height = "600px")
                       )
                )
              )
      ),
      
      tabItem(tabName = "info",
              fluidRow(
                box(width = 12, title = "Sobre este proyecto", solidHeader = TRUE,
                    HTML("
                      <p>Este proyecto fue realizado por <strong>Natalia Marín</strong>, utilizando <strong>R</strong>, <strong>Shiny</strong>, noticias y distintas fuentes.</p>
                      <p>Los datos fueron recolectados de diferentes sitios web, los cuales están detallados en el repositorio del proyecto en GitHub.</p>
                      <p>Repositorio: <a href='https://github.com/nataliamarinn/el-eternauta' target='_blank'>Repo de git</a></p>
                      <p>Contacto: <a href='mailto:nataliadcdjmarin@gmail.com'>nataliadcdjmarin@gmail.com</a></p>
                    ")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  output$mapa_lugares <- renderLeaflet({
    leaflet(mapa_geo) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addMarkers(
        lng = ~lon,
        lat = ~lat,
        popup = ~popup_html,
        label = ~direccion
      ) %>%
      setView(lng = -58.45, lat = -34.55, zoom = 11)
  })
  
 
  
  output$tabla_prensa <- renderDataTable({
    data.frame(
      Medio = c("Página 12", "Clarín", "Infobae"),
      Titular = c("El Eternauta como símbolo político", "Netflix lanza serie distópica", "El Eternauta revive debate"),
      Fecha = Sys.Date() - 1:3
    )
  })
  
  output$resumen_llm <- renderText({
    "El Eternauta es representado en la prensa como una mezcla de resistencia política y narrativa de ciencia ficción. Se destacan temas como dictadura, supervivencia, y memoria histórica."
  })
  
  output$kpi_palabras <- renderValueBox({
    valueBox("5.200", "Palabras analizadas", icon = icon("file-alt"), color = "purple")
  })
  
  output$kpi_episodios <- renderValueBox({
    valueBox("6", "Episodios", icon = icon("film"), color = "blue")
  })
  
  output$kpi_personajes <- renderValueBox({
    valueBox("12", "Personajes principales", icon = icon("users"), color = "green")
  })
  # ReactiveVal para palabra actual (inicia con "juan")
  palabra_actual <- reactiveVal("juan")
  
  # Cuando se aprieta el botón, actualizamos la palabra
  observeEvent(input$btn_buscar, {
    req(input$input_palabra)
    nueva_palabra <- tolower(input$input_palabra) %>% replace_non_ascii()
    palabra_actual(nueva_palabra)
  })
  
  # Data frame de palabras relacionadas según palabra_actual
  palabras_relacionadas_df <- reactive({
    palabra_buscada <- palabra_actual()
    df_rel <- palabras_correlacionadas(palabra_buscada, pares_corr, min_correlacion = 0.1, top_n = 8)
    if(nrow(df_rel) == 0){
      tibble(item2 = character(), correlation = numeric())
    } else {
      df_rel
    }
  })
  
  # Renderizar gráfico de barras con palabras relacionadas
  output$barras_palabras_relacionadas <- renderPlotly({
    df <- palabras_relacionadas_df()
    req(nrow(df) > 0)
    plot_ly(df, x = ~correlation, y = ~reorder(item2, correlation),
            type = 'bar', orientation = 'h',
            marker = list(color = 'steelblue')) %>%
      layout(title = paste("Palabras relacionadas con:", palabra_actual()),
             xaxis = list(title = "Correlación"),
             yaxis = list(title = "", autorange = "reversed"))
  })
  
  
  output$grafico_tf_idf <- renderPlot({
    if (input$btn_comparar == 0) {
      # Gráfico por defecto: compara los dos primeros medios
      comparar_medios(unique(df$Medio)[1], unique(df$Medio)[2])
    } else {
      # Al hacer clic en el botón, actualiza con los medios seleccionados
      medio1 <- input$medio1
      medio2 <- input$medio2
      comparar_medios(medio1, medio2)
    }
  })
  output$kpi_rating <- renderValueBox({
    valueBox(
      value = rating_general,
      subtitle = "Puntaje en IMDB",
      icon = icon("star"),
      color = "yellow"
    )
  })
  
  output$kpi_temporadas <- renderValueBox({
    valueBox(
      value = temporadas_total,
      subtitle = "Temporadas",
      icon = icon("film"),
      color = "purple"
    )
  })
  
  output$kpi_estreno <- renderValueBox({
    valueBox(
      value = fecha_estreno,
      subtitle = "Fecha de estreno",
      icon = icon("calendar"),
      color = "teal"
    )
  })
  output$tabla_episodios <- DT::renderDataTable({
    episodios_df %>%
      select(Título = Title, Fecha = Released, Puntaje = imdbRating, Votos = imdbVotes) %>%
      mutate(
        Puntaje = ifelse(is.na(Puntaje), "N/A", sprintf("%.1f ⭐", Puntaje)),
        Votos = formatC(Votos, format = "d", big.mark = ".")
      ) %>%
      datatable(
        options = list(
          pageLength = 10,
          dom = 't',
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          autoWidth = TRUE
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      )
  })
  
}

shinyApp(ui, server)
