#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(see)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Length Distribution and demultiplexing success"),
  
  # Fluid Row layout
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "length.file",
                label= "Select  length file",
                multiple = F)    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("First anchoring",
                 plotOutput("Histogram_1")),
        
        tabPanel("First Step demultiplex",
                 plotOutput("Histogram_2"),
                 
                 plotOutput("Barplot_1"),
                 plotOutput("Barplot_1b")),
        tabPanel("Second anchoring",
                 plotOutput("Histogram_3")),
                 
        tabPanel("Second Step demultiplex",
                 plotOutput("Histogram_4"),
                 plotOutput("Barplot_2"))
        ))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=300*1024^2) 
  
  data <- reactive({
    req(input$length.file)  # Wait for file to be uploaded
    read_table(input$length.file$datapath,
               col_names = c("seq","length", "file"))|> 
      mutate (file = basename(file)) |> 
      mutate(step = case_when(str_detect (file, "all.fastq.anchored.fastq") ~ "first.anchoring",
                              str_detect (file, "anchored_second_round.fastq") ~ "first.anchoring.0.4",
                              str_detect (file, "all.fastq") ~ "starting.point",
                              str_detect (file, "round2_rc.fastq") ~ "second.barcode",
                              str_detect (file, "anchored.rc.fastq") ~ "second.anchoring",
                              str_detect (file, "round1.fastq") ~ "first.barcode"
                              )) |> 
      mutate(step = fct_relevel(step , "starting.point", "first.anchoring","first.anchoring.0.4", "first.barcode", "second.anchoring", "second.barcode" )) |> 
      mutate (file = basename(file))
  })
  
  output$Histogram_1 <- renderPlot({
    # Generate Lengths histograms
   
      
    
    
    data() |> 
      filter (step %in% c("starting.point","first.anchoring","first.anchoring.0.4" )) |> 
      ggplot ( aes(y = length, x = step, fill = step)) +
      geom_violindot(fill_dots = "black") +
      theme_modern()+
      scale_fill_material_d() +
      scale_y_continuous(limits = c(0,2000))+
       # facet_wrap(~step, nrow = 1, scales = "free_x") +
      guides(fill = "none")
  })
  
  output$Histogram_2 <- renderPlot({
   data() |>
      filter (step %in% c("starting.point","first.anchoring","first.anchoring.0.4", "first.barcode" )) |>
      ggplot ( aes(y = length, x = step, fill = step)) +
      geom_violindot(fill_dots = "black") +
      theme_modern()+
      scale_fill_material_d() +
      scale_y_continuous(limits = c(0,2000))+
      # facet_wrap(~step, nrow = 1, scales = "free_x") +
      guides(fill = "none")
  })
  
  output$Barplot_1 <- renderPlot({

    data() |>
      filter (step =="first.barcode" ) |>
      ggplot( aes(x = file, fill= length>800)) +
      theme_modern(legend.position=c(.9,.75))+
      geom_bar(position = "stack")
  })
  
  output$Barplot_1b <- renderPlot({
    
    data() |>
      filter (step %in% c("starting.point","first.anchoring","first.anchoring.0.4", "first.barcode" )) |>
      ggplot( aes(x = step, fill= length>800)) +
      theme_modern(legend.position=c(.9,.75))+
      geom_bar(position = "stack")
  })
  # 
  output$Histogram_3 <- renderPlot({
    # Generate Lengths histograms
    
    
    data() |>
      filter (step %in% c("starting.point","first.anchoring","first.anchoring.0.4", "first.barcode", "second.anchoring" )) |>
      ggplot ( aes(y = length, x = step, fill = step)) +
      geom_violindot(fill_dots = "black") +
      theme_modern()+
      scale_fill_material_d() +
      scale_y_continuous(limits = c(0,2000))+
      # facet_wrap(~step, nrow = 1, scales = "free_x") +
      guides(fill = "none")
  })
  
  output$Histogram_4 <- renderPlot({
    # Generate Lengths histograms
    

    data() |>
      filter (step %in% c("starting.point","first.anchoring","first.anchoring.0.4", "first.barcode", "second.anchoring", "second.barcode" )) |>
      ggplot ( aes(y = length, x = step, fill = step)) +
      geom_violindot(fill_dots = "black") +
      theme_modern()+
      scale_fill_material_d() +
      scale_y_continuous(limits = c(0,2000))+
      # facet_wrap(~step, nrow = 1, scales = "free_x") +
      guides(fill = "none")
  })

  output$Barplot_2 <- renderPlot({

    data() |>
      filter (step =="second.barcode" ) |>
      ggplot( aes(x = file, fill= length>1000)) +
      theme_modern(legend.position=c(.9,.75))+
      geom_bar(position = "stack")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
