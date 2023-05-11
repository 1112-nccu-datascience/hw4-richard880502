library(shiny)
library(ggbiplot)
library(dplyr)
library(tidyr)
library(ca)

data(iris)

ui <- navbarPage(
  title = "PCA and CA Analysis",
  tabPanel(
    "PCA",
    tabsetPanel(
      tabPanel(
        "Plot",
        sidebarLayout(
          sidebarPanel(
            selectInput("xcol", "X-axis", names(iris[,1:4])),
            selectInput("ycol", "Y-axis", names(iris[,1:4]), selected=colnames(iris)[[2]])
          ),
          mainPanel(plotOutput("plotPCA"))
        )
        
      ),
      tabPanel(
        "input data",
        sidebarLayout(
          sidebarPanel(
            selectInput("numEntries", "Choose entries:",
                        choices = c(10, 25, 50, 100),selected = 25) 
          ),
          mainPanel(tableOutput("rawData"))
        )
      ),
    )
  ),
  tabPanel(
    "CA",
    fluidPage(
      plotOutput("clusterPlot")
    )
  ),
  # Add new tabPanel
  tabPanel(
    "About",
    fluidPage(
      h1("About The Iris Dataset"),
      tableOutput("irisInfo"),
      tags$img(src = "https://s3.amazonaws.com/assets.datacamp.com/blog_assets/Machine+Learning+R/iris-machinelearning.png", height = 250, width = 800)
    )
  )
)

server <- function(input, output,session) {
  # Reactive expression for the data subsetted to what the user selected
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  observe({
    choices <- names(iris[,1:4])
    choices <- choices[choices != input$xcol]
    updateSelectInput(session, "ycol",choices=choices)
  })
  
  # Create a scatterplot of the selected variables
  output$plotPCA <- renderPlot({
    # log transform 
    log.ir <- log(selectedData())
    ir.species <- iris[, 5]
    
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    print(g)
  })
 
  iris$Species <- as.factor(iris$Species)
  mytable <- table(iris$Species, iris$Sepal.Width)
  fit <- ca(mytable)
  
  output$clusterPlot <- renderPlot({
    print(fit) # basic results
    summary(fit) # extended results
    plot(fit) # symmetric map
    plot(fit, mass = TRUE, contrib = "absolute", map =
           "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map
  })
  
  # Reactive expression for basic information about the iris dataset
  irisInfo <- reactive({
    data.frame(
      "Variable" = c("Number of observations", "Number of variables", "Variable names", "Species"),
      "Value" = c(nrow(iris), ncol(iris) - 1, paste(names(iris)[1:4], collapse = ", "), paste(unique(iris$Species), collapse = ", "))
    )
  })
  
  # Render the table of iris information
  output$irisInfo <- renderTable({
    irisInfo() %>% pivot_longer(cols = c("Value"), names_to = NULL)
  })
  
  output$rawData <- renderTable({
    head(iris, as.numeric(input$numEntries))
  })
  
  output$pcaInfo <- renderTable({
    colnames(pca_res()$rotation) <- paste0("pc", 1:ncol(pca_res()$rotation))
    pca_res()$rotation[, 1:4]
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
