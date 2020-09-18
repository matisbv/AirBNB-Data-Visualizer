library(shiny)
library(shinythemes)
library(shinyjs)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(data.table)
library(R.utils)
library(bit64)

source("objects.R")

ui <- fluidPage(
    
    useShinyjs(),
    
    theme = shinytheme("cosmo"),
    
    titlePanel("AirBNB Data Visualizer"),
    
    navbarPage("",
               tabPanel("City selection",
                        sidebarLayout(
                            sidebarPanel(
                                helpText("This tab allows you to select a city to analyze."),
                                helpText("The selected city's data will then be used in the
                                         following tabs."),
                                selectInput("city", "Select the city of interest:",
                                            choices = c(citydat$cities),
                                            selected = "Amsterdam"),
                                helpText("Ex. Montreal, Paris, London, Athens..."),
                                checkboxInput("logcitymap", "Log scale", value = T),
                            ),
                            mainPanel(
                                leafletOutput("citymap", height = 700)
                            )
                        )),
               tabPanel("Neighbourhood selection",
                        sidebarLayout(
                            sidebarPanel(
                                helpText("This tab allows you to select a neighbourhood
                                          to analyze for the city that was chosen at the previous tab."),
                                helpText("It is also possible to select all neighbourhoods."),
                                selectInput("hood", "Select the neighbourhood of interest:",
                                            choices = c("All neighbourhoods"),
                                            selected = "All neighbourhoods"),
                                checkboxInput("logmap", "Log scale", value = T),
                                helpText("You can also scroll your mouse on the map to visualize each
                                          neighbourhood's delimitations and its number of AirBNB's.")
                            ),
                            mainPanel(
                                leafletOutput("broadmap", height = 700)
                            )
                        )
               ),
               tabPanel("Univariate analysis",
                        sidebarLayout(
                            sidebarPanel(helpText("This tab allows you to analyze the distribution of one feature
                                                   at a time of the AirBNB data for the area that was selected in
                                                   the previous tab."),
                                         conditionalPanel("input.hood == 'All neighbourhoods'",
                                                          selectInput("stat1dtq", "Choose which feature to analyze:",
                                                                      choices = c(setdiff(choices_graph, c(""))))),
                                         conditionalPanel("input.hood != 'All neighbourhoods'",
                                                          selectInput("stat1d", "Choose which feature to analyze:",
                                                                      choices = c(setdiff(choices_graph, c("Neighbourhood (neighbourhood_cleansed)")))))
                                         
                            ),
                            mainPanel(
                                plotOutput("graph1", height = 700),
                                textOutput("texte1")
                            )
                        )
               ),
               tabPanel("Bivariate analysis",
                        sidebarLayout(
                            sidebarPanel(helpText("This tab allows you to analyze the relationship between the features of AirBNBs 
                                                   and their daily price, for the area that was selected in
                                                   the previous tab."),
                                         conditionalPanel("input.hood == 'All neighbourhoods'",
                                                          selectInput("stat2dtq", "Analyze daily price with respect to... :",
                                                                      choices = setdiff(choices_graph, c("Prix quotidien (price)",
                                                                                                         "Daily price (price)",
                                                                                                         "Logarithm of daily price (log_price)")))),
                                         conditionalPanel("input.hood != 'All neighbourhoods'",
                                                          selectInput("stat2d", "Analyze daily price with respect to... :",
                                                                      choices = setdiff(choices_graph, c("Neighbourhood (neighbourhood_cleansed)",
                                                                                                         "Daily price (price)",
                                                                                                         "Logarithm of daily price (log_price)")))),
                                         checkboxInput("logboxplot", "Log scale", value = T)
                            ),
                            mainPanel(
                                plotOutput("graph2", height = 700),
                                textOutput("texte2")
                            )
                        )
               ),
               tabPanel("About",
                        includeMarkdown("README.md")
               )
    )
)

server <- function(input, output, session) {
    
    observeEvent(input$city,
        updateSelectInput(session, "hood",
                          label = "Select the neighbourhood of interest:",
                          choices = c("All neighbourhoods", neighbourhoods()$neighbourhood),
                          selected = "All neighbourhoods")
    )
    
    listings <- reactive({
            dat <- as.data.frame(fread(citydat$listings_url[citydat$cities == as.character(input$city)], encoding = "UTF-8"))
            dat <- dat[, c("host_acceptance_rate", "host_is_superhost",
                                     "latitude", "longitude", "property_type", "room_type",
                                     "accommodates", "bathrooms", "bedrooms", "beds",
                                     "price", "availability_365", "number_of_reviews", "review_scores_rating",
                                     "instant_bookable", "neighbourhood_cleansed")]
            dat$price <- as.numeric(gsub(",", "", gsub("\\$", "", dat$price)))
            dat$host_acceptance_rate <- as.numeric(gsub("%", "", dat$host_acceptance_rate))
            dat$host_is_superhost <- ifelse(dat$host_is_superhost == "t", T, F)
            dat$instant_bookable <- ifelse(dat$instant_bookable == "t", T, F)
            dat$availability_365 <- as.numeric(dat$availability_365)
            dat$number_of_reviews <- as.numeric(dat$number_of_reviews)
            dat$review_scores_rating <- as.numeric(dat$review_scores_rating)
            dat$log_price <- log(dat$price)
            dat
    })
    
    listings.dat <- reactive({
        if (input$hood != "All neighbourhoods")
            listings()[listings()$neighbourhood_cleansed == input$hood, ]
        else
            listings()
    })
    
    listings.formap <- reactive({
        if (nrow(listings.dat()) > 5000)
            listings.dat()[sample(1:nrow(listings.dat()), 5000), ]
        else
            listings.dat()
    })
    
    neighbourhoods <- reactive({
        
        dat <- suppressWarnings(rgdal::readOGR(citydat$neighbourhoods_url[citydat$cities == input$city], encoding = "UTF-8", use_iconv = T))
        dat
    })
    
    palmap <- reactive({
        if (input$logmap == T)
        {
            colorNumeric(
                palette = colorRamp(c("white", "red"), interpolate = "spline"),
                domain = listings.dat()$log_price,
                reverse = F)(listings.dat()$log_price)
        }
        else
        {
            colorNumeric(
                palette = colorRamp(c("pink", "red"), interpolate = "spline"),
                domain = listings.dat()$price,
                reverse = F)(listings.dat()$price)
        }
    })
    
    palmaplegend <- reactive({
        if (input$logmap == T)
        {
            colorNumeric(
                palette = colorRamp(c("white", "red"), interpolate = "spline"),
                domain = listings.dat()$log_price,
                reverse = F)
        }
        else
        {
            colorNumeric(
                palette = colorRamp(c("pink", "red"), interpolate = "spline"),
                domain = listings.dat()$price,
                reverse = F)
        }
    })
    
    stat1d <- reactive({
        if (input$hood != "All neighbourhoods")
            gsub(".*\\(", "", gsub("\\).*", "", input$stat1d))
        else
            gsub(".*\\(", "", gsub("\\).*", "", input$stat1dtq))
    })
    
    stat2d <- reactive({
        if (input$hood != "All neighbourhoods")
            gsub(".*\\(", "", gsub("\\).*", "", input$stat2d))
        else
            gsub(".*\\(", "", gsub("\\).*", "", input$stat2dtq))
    })
    
    price <- reactive({
        if (input$logboxplot == T)
            "log_price"
        else
            "price"
    })
    
    xlab1d <- reactive({
        if (input$hood != "All neighbourhoods")
            input$stat1d
        else
            input$stat1dtq
    })
    
    xlab2d <- reactive({
        if (input$hood != "All neighbourhoods")
            input$stat2d
        else
            input$stat2dtq
    })
    
    ylabboxplot <- reactive({
        if (input$logboxplot == T)
            "Logarithm of daily price (log_price)"
        else
            "Daily price (price)"
    })
    
    output$citymap <- renderLeaflet({
        withProgress({
            if (input$logcitymap == T)
            {
                leaflet(listings.formap()) %>% 
                    addProviderTiles(providers$CartoDB.Voyager) %>%
                    addCircleMarkers(lng = ~longitude,
                                     lat = ~latitude,
                                     color = ~palmap(),
                                     radius = 0.005,
                                     opacity = 1,
                                     fill = TRUE,
                                     fillColor = ~palmap(),
                                     fillOpacity = 1,
                                     group = 'Composante 4'
                    ) %>%
                    addLegend("bottomleft", pal = palmaplegend(), values = ~log(price),
                              title = "Log(Daily price)",
                              opacity = 1,
                              group = 'price'
                    )
            }
            else
            {
                leaflet(listings.formap()) %>% 
                    addProviderTiles(providers$CartoDB.Voyager) %>%
                    addCircleMarkers(lng = ~longitude,
                                     lat = ~latitude,
                                     color = ~palmap(),
                                     radius = 0.005,
                                     opacity = 1,
                                     fill = TRUE,
                                     fillColor = ~palmap(),
                                     fillOpacity = 1,
                                     group = 'Composante 4'
                    ) %>%
                    addLegend("bottomleft", pal = palmaplegend(), values = ~price,
                              title = "Daily price",
                              opacity = 1,
                              group = 'price'
                    )
            }
        }, message = "The map is loading...")
    })
    
    output$broadmap <- renderLeaflet({
        withProgress({
            if (input$hood == "All neighbourhoods")
            {
                if (input$logmap == T)
                {
                    leaflet(listings.formap()) %>% 
                        addProviderTiles(providers$CartoDB.Voyager) %>%
                        addCircleMarkers(lng = ~longitude,
                                         lat = ~latitude,
                                         color = ~palmap(),
                                         radius = 0.005,
                                         opacity = 1,
                                         fill = TRUE,
                                         fillColor = ~palmap(),
                                         fillOpacity = 1,
                                         group = 'Composante 4'
                        ) %>%
                        addLegend("bottomleft", pal = palmaplegend(), values = ~log(price),
                                  title = "Log(Daily price)",
                                  opacity = 1,
                                  group = 'price'
                        ) %>%
                        addPolygons(data = neighbourhoods(),
                                    label = ~paste0(neighbourhood, " : ", 
                                                    sapply(neighbourhood, 
                                                           function(i) sum(listings.dat()$neighbourhood_cleansed == i)), 
                                                    " AirBNB"),
                                    fillOpacity = 0,
                                    smoothFactor = 0.3,
                                    color = "grey",
                                    weight = 1,
                                    opacity = 1,
                                    labelOptions = labelOptions(textsize = "14px"))
                }
                else
                {
                    leaflet(listings.formap()) %>% 
                        addProviderTiles(providers$CartoDB.Voyager) %>%
                        addCircleMarkers(lng = ~longitude,
                                         lat = ~latitude,
                                         color = ~palmap(),
                                         radius = 0.005,
                                         opacity = 1,
                                         fill = TRUE,
                                         fillColor = ~palmap(),
                                         fillOpacity = 1,
                                         group = 'Composante 4'
                        ) %>%
                        addLegend("bottomleft", pal = palmaplegend(), values = ~price,
                                  title = "Daily price",
                                  opacity = 1,
                                  group = 'price'
                        ) %>%
                        addPolygons(data = neighbourhoods(),
                                    label = ~paste0(neighbourhood, " : ", 
                                                    sapply(neighbourhood, 
                                                           function(i) sum(listings.dat()$neighbourhood_cleansed == i)), 
                                                    " AirBNB"),
                                    fillOpacity = 0,
                                    smoothFactor = 0.3,
                                    color = "grey",
                                    weight = 1,
                                    opacity = 1,
                                    labelOptions = labelOptions(textsize = "14px"))
                }
            }
            else
            {
                if (input$logmap == T)
                {
                    leaflet(listings.formap()) %>% 
                        addProviderTiles(providers$CartoDB.Voyager) %>%
                        addCircleMarkers(lng = ~longitude,
                                         lat = ~latitude,
                                         color = ~palmap(),
                                         radius = 0.005,
                                         opacity = 1,
                                         fill = TRUE,
                                         fillColor = ~palmap(),
                                         fillOpacity = 1,
                                         group = 'Composante 4'
                        ) %>%
                        addLegend("bottomleft", pal = palmaplegend(), values = ~log(price),
                                  title = "Log(Daily price)",
                                  opacity = 1,
                                  group = 'price'
                        ) %>%
                        addPolygons(data = subset(neighbourhoods(), neighbourhood %in% input$hood),
                                    label = ~paste0(input$hood, " : ", 
                                                    sapply(input$hood, 
                                                           function(i) sum(listings.dat()$neighbourhood_cleansed == i)), 
                                                    " AirBNB"),
                                    fillOpacity = 0,
                                    smoothFactor = 0.3,
                                    color = "grey",
                                    weight = 1,
                                    opacity = 1,
                                    labelOptions = labelOptions(textsize = "14px"))
                }
                else
                {
                    leaflet(listings.formap()) %>% 
                        addProviderTiles(providers$CartoDB.Voyager) %>%
                        addCircleMarkers(lng = ~longitude,
                                         lat = ~latitude,
                                         color = ~palmap(),
                                         radius = 0.005,
                                         opacity = 1,
                                         fill = TRUE,
                                         fillColor = ~palmap(),
                                         fillOpacity = 1,
                                         group = 'Composante 4'
                        ) %>%
                        addLegend("bottomleft", pal = palmaplegend(), values = ~price,
                                  title = "Daily price",
                                  opacity = 1,
                                  group = 'price'
                        ) %>%
                        addPolygons(data = subset(neighbourhoods(), neighbourhood %in% input$hood),
                                    label = ~paste0(input$hood, " : ", 
                                                    sapply(input$hood, 
                                                           function(i) sum(listings.dat()$neighbourhood_cleansed == i)), 
                                                    " AirBNB"),
                                    fillOpacity = 0,
                                    smoothFactor = 0.3,
                                    color = "grey",
                                    weight = 1,
                                    opacity = 1,
                                    labelOptions = labelOptions(textsize = "14px"))
                }
            }
        }, message = "The map is loading...")
    })
    
    output$graph1 <- renderPlot({
        if (class(listings.dat()[, stat1d()]) %in% c("logical", "integer"))
        {
            ggplot(data = listings.dat(), aes_string(x = stat1d())) +
                geom_bar() +
                theme_bw() +
                xlab(xlab1d()) +
                theme(text = element_text(size = 16))
        }
        else if (class(listings.dat()[, stat1d()]) %in% c("character"))
        {
            ggplot(data = listings.dat(), aes_string(x = stat1d())) +
                geom_bar() +
                theme_bw() +
                xlab(xlab1d()) +
                coord_flip() +
                theme(text = element_text(size = 16))
        }
        else
        {
            ggplot(data = listings.dat(), aes_string(x = stat1d())) +
                geom_histogram() +
                theme_bw() +
                xlab(xlab1d()) +
                theme(text = element_text(size = 16))
        }
    })
    
    output$texte1 <- renderText({
        paste0("Number of missing observations: ", sum(is.na(listings.dat()[, stat1d()])), 
               "/", nrow(listings.dat()), ".")
    })
    
    output$graph2 <- renderPlot({
        if (class(listings.dat()[, stat2d()]) %in% c("logical", "integer"))
        {
            ggplot(data = listings.dat(), aes_string(x = paste0("as.factor(", stat2d(), ")"), y = price())) +
                geom_boxplot() +
                theme_bw() +
                xlab(xlab2d()) +
                ylab(ylabboxplot()) +
                theme(text = element_text(size = 16))
        }
        else if (class(listings.dat()[, stat2d()]) %in% c("character"))
        {
            ggplot(data = listings.dat(), aes_string(x = stat2d(), y = price())) +
                geom_boxplot() +
                theme_bw() +
                xlab(xlab2d()) +
                ylab(ylabboxplot()) +
                coord_flip() +
                theme(text = element_text(size = 16))
        }
        else
        {
            ggplot(data = listings.dat(), aes_string(x = stat2d(), y = price())) +
                geom_point() +
                theme_bw() +
                xlab(xlab2d()) +
                ylab(ylabboxplot()) +
                theme(text = element_text(size = 16))
        }
    })
    
    output$texte2 <- renderText({
        paste0("Number of missing observations: ", sum(is.na(listings.dat()[, stat2d()])), 
               "/", nrow(listings.dat()), ".")
    })
    
}

shinyApp(ui = ui, server = server)