library(dplyr)
library(ggplot2)
library(shiny)
library(shinyjs)
library(shinythemes)
library(DT)
library(plotly)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(geojsonio)
library(htmlwidgets)
library(htmltools)
library(shinycssloaders)

options(warn=-1) #to stop those introduced infinite y warnings

####################### READ CSV #############################
rm(list = ls())
ncov <- read.csv("https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv")
# convert Date from factor to date class
ncov <- ncov %>% mutate(Date = ymd(Date))
# save As latest Date
last_Date_unf = max(ncov$Date)
last_Date = format(last_Date_unf,"%d %b %Y")
save(ncov, file = paste0("ncov-",gsub("-","",last_Date_unf),".Rdata",sep=""))

#The lines below are only used because when the project was initially done the csv file contained a lat and long as well
load('ncov-20201015.Rdata') 
last_Date_unf = max(ncov$Date)
last_Date = format(last_Date_unf,"%d %b %Y")
#We can of course remove this by finding another way to add a lat and long and accounting for any other differences from initial file

ncov = ncov %>% mutate(Confirmed = ifelse(is.na(Confirmed), 0, Confirmed)) %>% mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths)) %>% mutate(Recovered = ifelse(is.na(Recovered), 0, Recovered))
#Due to Canada being split into Recovered Aggregate
##############################################################

####################### MAP CODE #############################
#Greenland was listed as a region in Denmark but we think it should be shown separately
ncov = ncov %>% mutate(Country.Region = ifelse(Province.State == "Greenland","Greenland",paste0(Country.Region))) %>% mutate(Province.State = ifelse(Province.State == "Greenland","",paste0(Province.State)))
ncov = ncov %>% arrange(Country.Region)

#Get one point for each country
ncovgroupint <- ncov %>% group_by(Country.Region,Date) %>% summarise(Confirmed = sum(Confirmed),Recovered = sum(Recovered),Deaths = sum(Deaths))
ncovcord = ncov %>% dplyr::select(Country.Region,Lat,Long) #Just need one Lat,Long per country
ncov2 = ncovcord %>% group_by(Country.Region) %>% slice(1L)
ncovgroup = left_join(ncovgroupint,ncov2, by = "Country.Region")
ncovgroup = ncovgroup %>% rename(Country = "Country.Region")
#View(ncovgroup)

n1 <- ncovgroup %>% filter(Date == last_Date_unf)  #Only take data from latest date

# Creating a ranking variable in order to use for colour coding instead of region
ncov1 <- n1 %>% mutate(ranking = ifelse(Confirmed <= 100, 1, ifelse(Confirmed <= 200, 2, ifelse(Confirmed <=400, 3, ifelse(Confirmed <= 1000, 4, ifelse(Confirmed <= 2000, 5, ifelse(Confirmed <= 10000, 6, ifelse(Confirmed <= 20000, 7, ifelse(Confirmed <= 30000, 8, ifelse(Confirmed <= 35000, 9, ifelse(Confirmed <= 40000, 10, 11))))))))))) %>% 
  mutate(choice = ifelse(Confirmed <= 100, "blue", ifelse(Confirmed <= 200, "blue", ifelse(Confirmed <=400, "blue", ifelse(Confirmed <= 1000, "green", ifelse(Confirmed <= 2000, "blue", ifelse(Confirmed <= 10000, "blue", ifelse(Confirmed <= 20000, "blue", ifelse(Confirmed <= 30000, "blue", ifelse(Confirmed <= 35000, "blue", ifelse(Confirmed <= 40000, "blue", "blue")))))))))))

pal5 <- colorFactor(palette = "Spectral", domain = c(1:11))

ncov1 <- ncov1 %>%
  mutate(RANKING = ifelse(ranking <= 4, "Lowest Confirmed Cases", ifelse(ranking <= 8, "Medium Confirmed Cases", ifelse(ranking >= 9, "Highest Confirmed Cases", ""))))


covranking1 <- ncov1 %>%
  filter(RANKING == "Lowest Confirmed Cases")

covranking2 <- ncov1 %>%
  filter(RANKING == "Medium Confirmed Cases")

covranking3 <- ncov1 %>%
  filter(RANKING == "Highest Confirmed Cases")

img <- "https://www.topevents.co.za/wp-content/uploads/2020/04/covid-19-1024x431.jpg"
#img


##################### MAP CODE END ########################

####################### For Tables #########################

countryU = unique(ncovgroup$Country)

min_date <- min(ncovgroup$Date)
max_date <- max(ncovgroup$Date)

############################################################

####################### Tooltips ###########################
plot_text = "Scatter plots of confirmed/recovered/death cases for different countries"
tab_text = "Table displaying the confirmed/recovered/death cases for different countries"
info_text = "General information about COVID-19"
rsa_text = "South African specific information about COVID-19"
map_text = "Interactive map with the latest COVID-19 information"

#We are using tooltips so we have to match our names to the format below
plot_text_ext = paste0('<span title=\"',plot_text,'\">Plot</span>',sep="")
tab_text_ext = paste0('<span title=\"',tab_text,'\">Table</span>',sep="")
map_text_ext = paste0('<span title=\"',map_text,'\">Map</span>',sep="")
#############################################################

# Used information from the following sites to help set up our different themes
# https://stackoverflow.com/questions/61580746/r-shiny-tables-changing-headings-and-light-dark-mode-switch
# https://stackoverflow.com/questions/61632272/r-shiny-light-dark-mode-switch

css <- HTML(" body {
    background-color: lightblue;
}")


ui <- fluidPage(
  theme = shinytheme("darkly"),
  tags$head(
    tags$style(
      "
                  body {
                    font-family: Helvetica Neue,Helvetica,Arial,sans-serif;
                  }

                  ul.nav-tabs li a {
                      background-color: #C75054;
                      color: white !important;
                  }
                  
                  ul.nav-tabs li.active a {
                      background-color: #c9777a !important;
                  }
                  
                  ul.navbar-nav li.active a {
                  background-color: #272b2e !important;
                  }
                  
                  navbar-header span.navbar-brand {
                  background-color: #272b2e !important;
                  }
                  
                  nav.navbar-static-top .container-fluid {
                  background-color: #3a3f44 !important;
                  }
                  
                  thead { color: white; background-color: #3a3f44 !important;}
                  
                  span.irs-bar, span.irs-from, span.irs-to, span.irs-single, span.irs-bar-edge{
                    border-top: #C75054 !important;
                    border-bottom: #C75054 !important;
                    background: #C75054 !important;
                  }
                  .irs-bar-edge{
                  border-left-color: #C75054 !important
                  }
                  
                  .irs-grid-pol{
                  background: #C75054 !important
                  }
                  
                  }
                 "
    )),
  titlePanel(title=div(img(src="https://www.pon-cat.com/application/files/7915/8400/2602/home-banner.jpg", width = 550, height = 150))),
  useShinyjs(),
  checkboxInput(
    inputId = "themeToggle",
    label = icon("sun")
  ),
  tags$script(
    "
        // define css theme filepaths
        const themes = {
            dark: 'shinythemes/css/darkly.min.css',
            light: 'shinythemes/css/flatly.min.css'
        }

        // function that creates a new link element
        function newLink(theme) {
            let el = document.createElement('link');
            el.setAttribute('rel', 'stylesheet');
            el.setAttribute('text', 'text/css');
            el.setAttribute('href', theme);
            return el;
        }

        // function that remove <link> of current theme by href
        function removeLink(theme) {
            let el = document.querySelector(`link[href='${theme}']`)
            return el.parentNode.removeChild(el);
        }

        // define vars
        const darkTheme = newLink(themes.dark);
        const lightTheme = newLink(themes.light);
        const head = document.getElementsByTagName('head')[0];
        const toggle = document.getElementById('themeToggle');

        // define extra css and add as default
        const extraDarkThemeCSS = ' .dataTables_filter label, .dataTables_info {color: white !important;} .dataTables_length {color: white !important;} ' +
        '.dataTables_length label select {color: black; }  .paginate_button { background: white!important;}  tbody { color: #000000;} body{color: #c8c8c8 !important}'
        const extraDarkThemeElement = document.createElement('style');
        extraDarkThemeElement.appendChild(document.createTextNode(extraDarkThemeCSS));
        head.appendChild(extraDarkThemeElement);


        // define event - checked === 'light'
        toggle.addEventListener('input', function(event) {
            // if checked, switch to light theme
            if (toggle.checked) {
                removeLink(themes.dark);
                head.removeChild(extraDarkThemeElement);
                head.appendChild(lightTheme);
                
            }  else {
                // else add darktheme
                removeLink(themes.light);
                head.appendChild(extraDarkThemeElement)
                head.appendChild(darkTheme);
            }
        })
        "
  ), 
  
  navbarPage("",
             tabPanel("Anna Kaplan and Arlton Gilbert", div(id ="Sidebar", sidebarPanel(
               selectInput("country", "Country", choices = countryU, selected = "South Africa", multiple = TRUE), 
               sliderInput("dates", label = "Date range", min = min_date, max = max_date, step = 10, value = c(min_date, max_date)),
               sliderInput("size", "Point size", min = 0, max = 5, value = 1),
               sliderInput("alpha", "Opacity", min = 0, max = 1, step = 0.05, value = 1),
               checkboxInput("line", "Plot line"),
               checkboxInput("logs", "Use Log Scale"),
               selectInput("type", "Type", choices = c("Confirmed","Recovered","Deaths"), selected = "Confirmed"),
               downloadButton("downloadData", "Download")
             )),
             mainPanel(
               tabsetPanel(id = "tabs", type = "tabs",
                           tabPanel(span("Map",title = map_text), icon = icon("map"),
                                    tags$br(),
                                    withSpinner(leafletOutput("map")),
                                    tags$style(type = "text/css", "#map {width: calc(75vh - 80px) !important;}","#map {height: calc(45vh) !important;}"),
                                    uiOutput("MAP")),
                           tabPanel(span("Plot",title = plot_text),icon = icon("chart-line"),
                                    withSpinner(plotlyOutput("plot"))),
                           tabPanel(span("Table",title = tab_text), icon = icon("table"),
                                    tags$br(),
                                    withSpinner(dataTableOutput("table"))),
                           tabPanel(span("Info",title = info_text), icon = icon("info"),
                                    tags$br(),
                                    uiOutput("info")),
                           tabPanel(span("South Africa",title = rsa_text), icon = icon("globe-africa"),
                                    tags$br(),
                                    uiOutput("SouthAfrica")))
             )
             )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    ncovgroup %>%
      filter(Country %in% input$country, between(Date, input$dates[1], input$dates[2]))
  })
  
  #when tab is clicked
  observeEvent(input$tabs,{
    if(input$tabs %in% c(plot_text_ext,tab_text_ext)) shinyjs::show(id = "Sidebar") else shinyjs::hide(id = "Sidebar")
    if(input$tabs == map_text_ext) react_map(base_map()) 
  })
  
  output$plot <- renderPlotly({
    dats = data() %>% ungroup()
    
    p = ggplot(dats, aes(x = Date,y = Confirmed))
    
    if (input$type == "Deaths")  p <- ggplot(dats, aes(x = Date,y = Deaths))
    if (input$type == "Recovered")  p <- ggplot(dats, aes(x = Date,y = Recovered))
    
    p = p + theme(legend.title = element_blank()) + 
      geom_point(aes(color = Country), size = input$size, alpha = input$alpha) +
      scale_y_continuous(labels = scales::comma)
    
    suppressMessages(if (input$logs)  p <- p + scale_y_log10(paste0(input$type," - log Scale",sep="")))
    if (input$line) p <- p + geom_line(aes(group = Country))
    p
  })
  output$table <- DT::renderDT({
    data() %>%
      group_by(Country) %>%
      arrange(Country, desc(Date)) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(Country)  
  })
  
  base_map <- function(){
    ncov1 %>%
      leaflet()  %>%
      setMaxBounds(-150, 104,174, -82) %>%
      addTiles(group = "OSM",options = providerTileOptions(minZoom = 1,maxZoom=4)) %>%
      addProviderTiles("CartoDB", group = "Carto",options = providerTileOptions(maxZoom=4)) %>%
      addProviderTiles("CartoDB.DarkMatter", group = "Black World",options = providerTileOptions(maxZoom=4)) %>%
      addSearchOSM() %>%
      addResetMapButton() %>%
      setView(42, 16, 1.4) %>% 
      leafem::addLogo (img, url = "https://www.topevents.co.za/wp-content/uploads/2020/04/covid-19-1024x431.jpg",
                       width = 130,
                       height = 50) %>%
      addCircleMarkers(data = covranking1, radius = ~ifelse(Confirmed <= 50 , 2, ifelse(Confirmed <= 100, 3, ifelse(Confirmed <= 200, 4, ifelse(Confirmed <= 1000, 6, ifelse(Confirmed <= 20000, 8, ifelse(Confirmed <=40000, 10, 12)))))),
                       lng = ~Long,
                       lat = ~Lat,
                       popup = ~paste0("Country: ", "<b>", Country, "</b>", "<br/>", "Date: ", "<b>", Date, "</b>", "<br/>", "Confirmed: ", "<b>", Confirmed, "</b>", "<br/>", "Recovered: ", "<b>",  Recovered, "</b>", "<br/>", "Deaths: ", "<b>", Deaths, "</b>"),
                       group = "Lowest Confirmed Cases",
                       color = ~pal5(ncov1$ranking),
                       stroke = FALSE, fillOpacity = 0.5)  %>%
      addCircleMarkers(data = covranking2, radius = ~ifelse(Confirmed <= 50 , 2, ifelse(Confirmed <= 100, 3, ifelse(Confirmed <= 200, 4, ifelse(Confirmed <= 1000, 6, ifelse(Confirmed <= 20000, 8, ifelse(Confirmed <=40000, 10, 12)))))),
                       lng = ~Long,
                       lat = ~Lat,
                       popup = ~paste0("Country: ", "<b>", Country, "</b>", "<br/>", "Date: ", "<b>", Date, "</b>", "<br/>", "Confirmed: ", "<b>", Confirmed, "</b>", "<br/>", "Recovered: ", "<b>",  Recovered, "</b>", "<br/>", "Deaths: ", "<b>", Deaths, "</b>"),
                       group = "Medium Confirmed Cases",
                       color = ~pal5(ncov1$ranking),
                       stroke = FALSE, fillOpacity = 0.5)  %>%
      addCircleMarkers(data = covranking3, radius = ~ifelse(Confirmed <= 50 , 2, ifelse(Confirmed <= 100, 3, ifelse(Confirmed <= 200, 4, ifelse(Confirmed <= 1000, 6, ifelse(Confirmed <= 20000, 8, ifelse(Confirmed <=40000, 10, 12)))))),
                       lng = ~Long,
                       lat = ~Lat,
                       popup = ~paste0("Country: ", "<b>", Country, "</b>", "<br/>", "Date: ", "<b>", Date, "</b>", "<br/>", "Confirmed: ", "<b>", Confirmed, "</b>", "<br/>", "Recovered: ", "<b>",  Recovered, "</b>", "<br/>", "Deaths: ", "<b>", Deaths, "</b>"),
                       group = "Highest Confirmed Cases",
                       color = ~pal5(ncov1$ranking),
                       stroke = FALSE, fillOpacity = 0.5)  %>%
      addLayersControl(baseGroups = c("OSM", "Carto", "Black World"),
                       overlayGroups = c("Highest Confirmed Cases", "Medium Confirmed Cases", "Lowest Confirmed Cases")) 
  }
  
  # reactiveVal for the map object, and corresponding output object.
  react_map <- reactiveVal(base_map())
  output$map <- renderLeaflet({
    react_map()
  }) 
  
  output$MAP <- renderUI({
    tags$div(tags$b(paste0("The map above is interactive. Click  on the markers and use the icons in the top left and right corners to 
                           explore the data! Data Updated: ",last_Date)), tags$br(), 
             
             "    " , tags$br(), "If you would like to zoom in or out of any specific region use the plus and minus sign in the 
             top left hand corner. You can also use your mouse to click on and move the map in order to find the country of your choice. 
             Another quick and easy way of finding a specific country is to use the magnifying glass and type in your desired search - 
             this tool will take you directly to any specific point. 
             To return to the original screen click on the button below the magnifying glass.", tags$br(), 
             
             "    " , tags$br(),
             
             "The map not only allows you to choose between three map types: OSM, Carto and Black World but also allows you to 
             choose which data points are shown on the graph.
             The data has been separated into three groups: Lowest Confirmed Cases, Medium Confirmed Cases and Highest Confirmed Cases. 
             You have the option as to what combination of data points you would like to be shown on the map.",  
             tags$br()
    ) })
  output$info <- renderUI({
    tags$div("The data used to create this app originated from", 
             tags$a(href="https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv", "github"), " and contains ", nrow(ncovgroup), " records.", 
             tags$br(), "    " , tags$br(),
             tags$b("Frequently asked questions about COVID-19, answered by valid sources such as the World Health Organization (WHO) 
                    and the National Institution for Communicable Diseases (NICD):") , tags$br(), 
             
             "    " , tags$br(), tags$b("What is the Coronavirus (COVID-19) and why is it called this?"), tags$br(),  "    " , tags$br(),
             
             "Coronaviruses are commonly identified in animal but only a small number of these cases can cause diseases in humans. 
             COVID-19 is evidently not the first coronavirus that humans have been exposed to. Each coronavirus differs from the next and 
             becomes a new (novel) coronavirus for humans. 

On 7 January 2020, severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2) was confirmed as the causative agent of coronavirus disease 2019 (COVID-19). 

The name corona (crown) originates from the visual characteristics of the virus when viewed under an electron microscope as these viruses have 
crown-like spikes on their surface (NICD, 2020).", 
             tags$br(),  "    " ,tags$br(), tags$b("What are the symptoms of COVID-19?"), tags$br(),  "    " ,tags$br(), "The virus will 
             affect people differently however, those that show symptoms may experience any of the symptoms from the list below (WHO, 2020).", 
             tags$br(),  "    " ,tags$br(),
             "Common symptoms include:", tags$br(),
             "•	Fever", tags$br(),
             "•	Tiredness", tags$br(),
             "•	Dry cough" , tags$br(), "    " ,tags$br(),
             "Other symptoms include:", tags$br(), "    " ,tags$br(),
             "•	Shortness of breath", tags$br(),
             "•	Aches and pains", tags$br(),
             "•	Sore throat", tags$br(),
             "•	Very few people will report diarrhoea, nausea or a runny nose", tags$br(), "    " ,tags$br(),
             
             "Individuals can experience all the symptoms from the list above or only a few. While some, although carrying the virus, may 
             be asymptomatic and not show any of the symptoms above. Individuals who are asymptomatic can still pass the virus on to others. 
             One’s medical conditions and age will affect the way in which one’s body handles the virus.", tags$br(), 
             
             "    " , tags$br(), tags$b("Is there a cure for COVID-19?"), tags$br(), 
             
             "    " , tags$br(), "There is currently no specific vaccines or treatments for COVID-19. There are however several ongoing clinical 
             trials evaluating potential treatments for the virus (WHO, 2020).", tags$br(), 
             
             "    " , tags$br(), 
             
             "The FDA has created an emergency program directed specifically at finding a cure for COVID-19. The emergency program, 
             Coronavirus Treatment Acceleration Program (CTAP), have used all possible methods to move new treatments to patients as quickly 
             as possible, while simultaneously finding out whether they are helpful or harmful. Due to the urgent need to find a cure for the virus, 
             there are several companies and researchers developing COVID-19 related therapies. As of April 19 2020 there are 72 active trials of 
             therapeutic agents and 211 other development programs for therapeutic agents in the planning stages (FDA, 2020).", 
             tags$br(), 
             
             "    " , tags$br(), tags$b("How does the virus spread from one person to the another?"), tags$br(), 
             
             "    " , tags$br(), "The COVID-19 virus spreads primarily through droplets of saliva or discharge from the nose or mouth when an 
             infected person coughs or sneezes (WHO, 2020). If an infected person coughs or sneezes their germs may either land directly onto 
             another person or a surface that may be touched by another person who then touches their nose, mouth or eyes which puts them at risk of 
             contracting the virus. The virus could also be directly transferred from one person to another through their saliva (WHO, 2020).", tags$br(), 
             
             "    " , tags$br(), tags$b("How can one protect themselves and reduce the spread of COVID-19?"), tags$br(), 
             
             "    " , tags$br(), "Protect yourself and others from infection by washing your hands or using an alcohol based rub 
             frequently and not touching your face (WHO, 2020). If one comes into contact with an individual who has tested positive for 
             COVID-19 they should self-isolate for approximately 14 days. If no symptoms show in this time one may leave isolation but 
             still be cautious about spreading their germs. If any symptoms show within the 14 days, individuals should go for testing 
             and only leave isolation until their tests come back negative.", tags$br(), 
             
             "    " , tags$br(), tags$b("Who is the most at risk group?"), tags$br(), 
             
             "    " , tags$br(), "People who have underlying medical conditions and those over 60 years old have a higher risk of getting 
             very sick from the virus or potentially dying from being infected by the virus (WHO, 2020).", tags$br(), 
             
             "    " , tags$br(), tags$b("A list of articles you may be interested in reading:"), tags$br(), 
             
             "    " , tags$br(), tags$b("1.	The Global Economic Impacts of COVID-19"), tags$br(), 
             tags$a(href="https://www.csis.org/analysis/global-economic-impacts-covid-19", "global-economic-impacts-19"), tags$br(), 
             
             "    " , tags$br(), tags$b("2.	The Economic Impact of COVID-19 in Low and Middle Income Countries"), tags$br(), 
             tags$a(href="https://www.cgdev.org/blog/economic-impact-covid-19-low-and-middle-income-countries", "economic-impact-covid-19-low-and-middle-income-countries"), tags$br(), 
             
             "    " , tags$br(), tags$b("3. Why outbreaks like coronavirus spread exponentially, and how to “flatten the curve”"), tags$br(), 
             tags$a(href="https://www.washingtonpost.com/graphics/2020/world/corona-simulator/", "global-economic-impacts-19"), tags$br(), 
             
             "    " ,tags$br(), tags$em("The video below answers several of questions stated above. It is an animation of what COVID19 
                                        is and how it affects the human body."), tags$br(), 
             
             "    " , tags$br(), tags$iframe(
               src = "https://www.youtube.com/embed/5DGwOJXSxqg",
               width = 560,
               height = 315
             ), tags$a(
               "Nucleus Medical Media",
               href = "https://www.youtube.com/embed/uEmUJcHysds",
               class = "site"
             )
             
    )
    
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$country, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  output$SouthAfrica <- renderUI({
    tags$div(
      tags$b("What is South Africa doing to flatten the curve?"), tags$br(), 
      
      "    " , tags$br(), "On the 15th of March 2020, Cyril Ramaphosa addressed the nation regarding the initial steps 
      the government will be taking in order to prevent the spread of the virus within the country. 
      Some of these measures included the closing of schools and universities, travel bans and the closure of several ports and boarders. 
      The President also encouraged all those entering the country from overseas countries to enter self-isolation for 14 days on their arrival. 
      If these individuals experienced any symptoms within this time frame they were encouraged to go for testing.", 
      tags$br(), "    " , tags$br(),
      "From 27 March 2020, South Africa entered what was supposed to be a three week complete lockdown with only essential services operating. 
      The lockdown was extended by two weeks, until 30 April 2020. All non-essential businesses were closed and the general population was only 
      be allowed to leave their houses in order to go to the doctor/hospital or to go get tested for the virus. The general population was also 
      be allowed to leave their homes in order stock up on food and to fill petrol.", tags$br(), 
      "    " ,tags$br(), tags$em("The video below shows President Cyril Ramaphosa addressing the nation (23/03/2020) and announcing the measures 
                                 taken by the South African government to try contain the spread of the virus:"), tags$br(), 
      
      "    " , tags$br(), tags$iframe(
        src = "https://www.youtube.com/embed/Xk1UIqibUt8",
        width = 560,
        height = 315
      ), tags$br(), "    " , tags$br(), 
      "When the government announced the end of the five week lockdown they implemented a nation-wide strategy that was directed towards slowly 
      opening up the economy. The government has decided to open the economy up in stages. They have referred to the initial, complete lockdown 
      (27 March 2020 – 30 April 2020) as Stage 5 in their Risk Adjustment Strategy.",
      tags$br(), "    " , tags$br(), 
      "The whole country moved from Stage 5 to Stage 4 on 30 April 2020. When this was announced the government had a disclaimer 
      that stated each province is subjected to their own stage. I.e. Gauteng may be in Stage 4 while the Western Cape is in Stage 5. 
      In Stage 4 the country has seen some workers return to work while others remain unemployed due to COVID-19 lockdown laws. 
In order to support those who have lost their jobs due to COVID-19, Ramaphosa has allocated a significant amount of South Africa’s GDP to 
      fund those that have suffered from the harsh repercussions of the measures taken by the government to prevent the spread of the disease. ",
      tags$br(), "    " , tags$br(), 
      tags$em("The video below shows President Cyril Ramaphosa addressing the nation (21/04/2020) regarding the way in which support will be 
              given to those in need, in South Africa:"), tags$br(), 
      
      "    " , tags$br(), tags$iframe(
        src = "https://www.youtube.com/embed/568M7_bf2sc",
        width = 560,
        height = 315
      ), tags$br(), "    " , tags$br(), 
      
      tags$b("How can you help South Africans in need?"), tags$br(), 
      "    " ,tags$br(), "In order to try combat the devastating effects COVID-19 will have on lower-income South Africans and small 
      South African businesses the government, under President Cyril Ramaphosa, has created a Solidarity Fund. This fund aims to prevent 
      the spread of the disease, detect the magnitude of the disease within the country, care for those in need of medical assistance and 
      support those whose lives have been affected by the pandemic.", tags$br(), 
      "    " ,tags$br(), tags$b(tags$em("If you would like to donate to this fund click on the link below to find the Solidarity Fund banking details:")),tags$br(), 
      tags$a(href="https://www.solidarityfund.co.za/", "Solidarity-Fund"),  tags$br(), "    " ,tags$br(),
      
      "There are also several other NGOs that need your help helping those less fortunate:", tags$br(), "    " , tags$br(), tags$b("Ladels of Love"), tags$br(),
      "Ladles of Love is an extensive community project dedicated to feeding hungry people. Their soup kitchens are currently feeding the 
      homeless and young children in Cape Town.", tags$br(), 
      tags$a(href="https://www.ladlesoflove.org.za/donations/", "Ladels-of-Love"),  tags$br(), "    " , tags$br(), tags$b("BackaBuddy"), tags$br(),
      "Several BackaBuddy initiatives have been started in order to raise funds. The link below will take you to a BackaBuddy page that is 
      directed towards raising funds for doctors, nurses and all front-line workers in order to ensure these individuals have the right equipment 
      needed to treat infected individuals.", tags$br(), 
      tags$a(href="https://www.backabuddy.co.za/champion/project/coronavirus-support", "BackaBuddy-Healthcareworkers")
    )
    
  })
  
  outputOptions(output, "info", suspendWhenHidden = FALSE)
  outputOptions(output, "SouthAfrica", suspendWhenHidden = FALSE)
  outputOptions(output, "MAP", suspendWhenHidden = FALSE)
}

shinyApp(ui = ui, server = server)
