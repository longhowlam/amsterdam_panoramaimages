library(shinydashboard)
library(dplyr)
library(leaflet)
library(stringr)
library(data.table)

#### import benodigde data
clf_classes2 = readRDS("data/clf_classes2.RDs")
Amsterdam_Clarifai_classes = readRDS("data/Amsterdam_Clarifai_classes.RDs")
AmsterdamWijken = readRDS("data/AmsterdamWijken.RDs")
fn = readRDS("data/fn.RDs")

ui <- dashboardPage(
  dashboardHeader(title = "Amsterdam door de ogen van computer vision", titleWidth = 600),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inleiding", tabName = "introduction", icon = icon("dashboard")),
      menuItem("Beelden classificaties", tabName = "imagestab", icon = icon("th")),
      selectInput("imagelabel", "image label", clf_classes2$entities),
      checkboxInput("randombeelden", "toon ook de random beelden")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "introduction",
        h4("Inleiding"),
        list(
            p(
              "De gemeente Amsterdam heeft panoramabeelden van Amsterdam beschikbaar gesteld.",
              a(href="https://api.data.amsterdam.nl/panorama/", "Zie hier."),
              "Een camera auto heeft door Amsterdam gereden en foto's genomen. Ik heb per buurt in Amsterdam 400 random images gepakt
              en deze door ",
             a(href = "https://www.clarifai.com/models/general-image-recognition-model-aaa03c23b3724a16a56b629203edc62c", "Clarifai image model"),
             "heen gehaald. Per image krijg je dan een aantal labels."
             ),
            
            p(" "),
            p("Onderstaande wordcloud geeft een overzicht van de labels die het clatifai model teruggeeft op de panoramabeelden van Amsterdam"),
            img(SRC="amsterdamlabels.png", height = 250),
            p(" "),
            p("Deze shiny app laat het percentage per buurt zien van een gekozen label, klick op de buurt voor animated gifs en klik op een punt voor
              een specifiek image."),
            p("Cheers, Longhow")
                )
              
              ),
      tabItem(tabName = "imagestab",
              fluidRow(
                h5("kaartje met per buurt het percentage images van de gekozen label"),
                leafletOutput('images', width  = "1400px", height = "650px" )
              )
      )
      
  )
    )
)


###############################################################



server <- function(input, output) {

  output$images = renderLeaflet({
    h5("percentage per buurt van gekozen label: ", input$imagelabel)
    #### create the data for shapes for the map
    Amsterdam_image_perc = Amsterdam_Clarifai_classes %>% 
      mutate(
        imlabel = entities == input$imagelabel 
      ) %>% 
      group_by(BU_NAAM) %>% 
      summarise(n=n(), imlabel = mean(imlabel))
   
    AmsterdamWijken3 = AmsterdamWijken
    
    pp = left_join(AmsterdamWijken3@data, Amsterdam_image_perc, by = c("BU_NAAM" = "BU_NAAM"))
    AmsterdamWijken3@data = pp
    
    polpopup = paste0(
      "<b>",
      AmsterdamWijken3$BU_NAAM, ": ", 
      input$imagelabel, " percentage ", round(100*AmsterdamWijken3$imlabel,1), "%", 
      "</b>",
      "<br/>",
      "<img border='0' src='http://www.lhldsd.nl/amsterdam/",
      fn,
      "' width='325' height='200'>"
    )
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g procent dichtheid van label %s",
      AmsterdamWijken3$BU_NAAM, round(100*AmsterdamWijken3$imlabel,2), input$imagelabel
    ) %>% lapply(htmltools::HTML)
    
    TMP = clf_classes2$n[clf_classes2$entities == input$imagelabel]
    N_quantiles = ifelse(TMP < 3500, 3, 7)
    
    ### maak leaflet
    m = leaflet()
    m = m %>%
      setView(lng = 4.959431, lat = 52.34, zoom = 12) %>%
      addTiles() %>%
      addPolygons(
        weight=2,
        smoothFactor = 0.2, fillOpacity = 0.55, 
        color = ~colorQuantile(n = N_quantiles, "Greens", AmsterdamWijken3$imlabel)(imlabel),
        data = AmsterdamWijken3,
        popup = polpopup ,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      )
    
    if(input$randombeelden){
      
    #### create set for markers for the random selected images
      locs = Amsterdam_Clarifai_classes[.(input$imagelabel )]
      polpopup2 = paste0(
        "<strong>panorambeeld geclassificeerd als ", 
        input$imagelabel, "</strong> <br/>",
        "<img border='0' src='",
        locs$link,
        "' width='315' height='200'>"
      )
      m  = leaflet() %>%
        setView(lng = 4.959431, lat = 52.34, zoom = 13) %>%
        addTiles() %>%
        addPolygons(
          weight=2,
          smoothFactor = 0.2, fillOpacity = 0.55, 
          color = ~colorQuantile(n = N_quantiles, "Greens", AmsterdamWijken3$imlabel)(imlabel),
          data = AmsterdamWijken3 
        ) %>% 
          addCircleMarkers(
            lng=~x, lat = ~y, radius = 0.4,
            opacity = .421,
            popup = polpopup2,
            data = locs
          )
      }
   
    m
    
  })
}

shinyApp(ui, server)