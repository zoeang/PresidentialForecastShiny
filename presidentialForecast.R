library(shiny)
library(EBMAforecast)
data(presidentialForecast)
dataset<-cbind(seq(1952, 2008,4), presidentialForecast)
colnames(dataset)[1]<-"Year"
# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Presidential Forecast"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Selector for choosing dataset ----
        selectInput(inputId = "method",
                    label = "Choose a Method:",
                    choices = c('Actual', 'Campbell', 'Lewis-Beck', 'EWT2C2', 'Fair', 'Hibbs', 'Abramowitz'),
                    selected="Actual"
                    ),
          #end selector
        # Input: Specification of range within an interval ----
        sliderInput("range", h3("Years:"),
                    min = 1952, max = 2008,
                    value = c(1952,2008), step=4, sep=""
                    ),
        #end slider
         helpText("Here are the results of presidential forecasts from 1952-2008")
         ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("presForecast", click = "plotmethod"),
      verbatimTextOutput("info"),
      tableOutput("view")
    )
    
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  #format data
  library(EBMAforecast)
  data(presidentialForecast)
  dataset<-cbind(as.integer(seq(1952, 2008,4)), presidentialForecast)
  colnames(dataset)[1]<-"Year"

  # year slider
  sliderValues <- reactive({
    
    data.frame(
      Name = "Year Range",
      Value = as.character(paste(input$Year, collapse = " ")),
      stringsAsFactors = FALSE)
  })
  #plot 
  output$presForecast<-renderPlot({
    plot(dataset$Actual[1:(((input$range[2]-input$range[1])/4)+1)], type="l", axes=F, main="Predicted Democratic Vote Share", xlab="Year", 
         ylab="Prediction")
    axis(1,at= seq(from=1,to=(((input$range[2]-input$range[1])/4)+1),by=1), #---------------------------------
         labels = as.character(seq(input$range[1], input$range[2], 4)))
    legend("topright",
           legend=c("Actual", paste(input$method)), 
           lty=c(1,1), col=c("black","red"), bty="n")
    #seq(input$range[1]-1951,input$range[2]-1951,1)
    axis(side=2, las=2)
    if(input$method=="Campbell"){
      points(dataset$Campbell, type="l", col="red")
    }
    if(input$method=="Lewis-Beck"){
      points(dataset[,3], type="l", col="red")
    }
    if(input$method=="EWT2C2"){
      points(dataset[,4], type="l", col="red")
    }
    if(input$method=="Fair"){
      points(dataset[,5], type="l", col="red")
    }
    if(input$method=="Hibbs"){
      points(dataset[,6], type="l", col="red")
    }
    if(input$method=="Abramowitz"){
      points(dataset[,7], type="l", col="red")
    }
  })
 #click output
  output$info <- renderText({
    paste0("x=", input$plotmethod$x, "\ny=", input$plotmethod$y)})

}

# Create Shiny app ----
shinyApp(ui, server)
