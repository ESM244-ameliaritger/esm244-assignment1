#attach packages
library(tidyverse)
library(janitor)
library(shiny)
library(shinythemes)
library(shinyWidgets)

#read in data
reef <- read_csv("MBONReef_Histogram.csv")

#Tidy up data
reef_tidy <- reef %>%
  clean_names() %>%
  pivot_longer("annelida_cirriformia_luxuriosa":"substrate_amphipod_tube_complex") %>%
  separate(name, into="phylum", sep="_", remove=FALSE) %>%
  mutate(vectorized_name=str_split(name, pattern="_")) %>%
  filter(!phylum=="no") %>% 
  filter(!phylum=="substrate")

reef_tidy$binary <- ifelse(reef_tidy$value>0, 1, 0)
reef_tidy$species <- gsub("^[^_]*_","",reef_tidy$name, perl=TRUE)

reef_tidy <- reef_tidy %>%
  filter(binary > "0")


#Create user interface
ui <- navbarPage("Amelia's navigation bar",
                 theme = shinytheme("cerulean"),
                 tabPanel("First tab!",
                          h1("first tab header"),
                          p("here's some regular text"),
                          sidebarLayout(
                            sidebarPanel("here's some text",
                                         radioButtons(inputId="focal",
                                                     label="pick a phylum!",
                                                     choices=unique(reef_tidy$phylum)),
                                         pickerInput(inputId="coocurring",
                                                     label="pick some more phyla!",
                                                     choices=unique(reef_tidy$phylum),
                                                     options = list(`actions-box`=TRUE,
                                                                    `selected-text-format` = "count > 3"),
                                                     multiple = TRUE)),
                            mainPanel("some more text is now here",
                                      plotOutput(outputId="plot1")
                            ))),
                 tabPanel("Second tab!",
                          h1("second tab header"),
                          p("here's some more regular text"),
                          sidebarLayout(
                            sidebarPanel("some text is here",
                                         radioButtons(inputId="locationselect",
                                                            label="pick a location!",
                                                            choices=unique(reef_tidy$location))),
                            mainPanel("some more text is here",
                                      plotOutput(outputId="plot2"))
                          )))


# Create server
server <- function(input, output){
  
  reef_select <- reactive({
    reef_tidy %>%
      mutate(focal_phylum=input$focal) %>%
      mutate(presence = ifelse(phylum==focal_phylum, filename, "FALSE")) %>%
      filter(filename %in% presence) %>%
      filter(phylum==c(input$coocurring))
  })
  
  output$plot1 <- renderPlot({
    ggplot(reef_select(), aes(x=phylum)) +
      geom_bar(aes(phylum)) +
      coord_flip()
  })
  
  reef_site <- reactive({
    reef_tidy %>%
      filter(location==input$locationselect)
  })
  
  output$plot2 <- renderPlot({
    ggplot(reef_site(), aes(x=phylum)) +
      geom_bar(aes(phylum))
  })
}


# Let R know you want to combine ui and server into an app
shinyApp(ui=ui, server=server)