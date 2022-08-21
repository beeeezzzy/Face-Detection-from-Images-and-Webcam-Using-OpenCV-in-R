#Loads all the necessary library used in this project
library(shiny)
library(magick)
library(opencv)

#This section is the UI of the Project
ui = fluidPage(
  titlePanel(h1(strong("Live and Image Face Detection App"))),
  #This creates an input button in choosing an image
  fluidRow(
    column(5, 
           fileInput("file", "Choose Image"),
    )),
  #This creates an action button in detecting an image
  fluidRow(
    column(5, 
           actionButton("detect", 
                        "Detect Image", 
                        style = "color: white; background-color: #4284f5; style=display:center-align",
                        width = 150),
           
           br(),
           br(),
           
    )),
  #This creates a save button in saving a detected image
  fluidRow(
    column(5, 
           actionButton("saveimage", 
                        "Save Image", 
                        style = "color: white; background-color: #4284f5; style=display:center-align",
                        width = 150),
           br(),
           br(),
           
    )),
  #This creates an action button in live face detection
  fluidRow(
    column(5, 
           actionButton("live", 
                        "Live Face Detection", 
                        style = "color: white; background-color: #4284f5; style=display:center-align",
                        width = 200),
    )),
  #This contains the output elements 
  mainPanel(
    column(1,plotOutput("detectimage")),
    column(1,plotOutput("imagesave")),
    column(1,plotOutput("liveimagedetect"))
  )
)

#This contains the instructions we used in order to create our app
server = function(input,output){
  
  #This reads the image and in our case we use ocv_read to read the image
  originalImage <- reactive({
    file_name <- input$file
    suppressWarnings({
      if (is.null(file_name)){
        return(NULL) 
      }else{
        return(ocv_read(file_name$datapath))
      }
    })
  })
  
  #This detects the face by using ocv_face
  cam <- reactive({
    req(originalImage())
    y <- ocv_face(originalImage())
    return(y)
  })
  
  #This detects faces in live format by using ocv_video(ocv_face)
  liveimg <- reactive({
    u <- ocv_video(ocv_face)
    return(u)
  })
  
  #This triggers a value which is cam() and updates in response to an event.
  detect.click = eventReactive(input$detect, {
    cam()
  }) 
  
  #This triggers a value which saves the detected image and updates in response to an event.
  saveimage.click = eventReactive(input$saveimage, {
    ocv_write(cam(), 'C:/Users/HelloKitty/Desktop/Sample.jpg')
  })
  
  #This triggers a value which is liveimg() and updates in response to an event.
  live.click = eventReactive(input$live,{
    liveimg()
  })
  
  #This outputs the image being chosen
  output$original <- renderPlot({
    plot(originalImage(), all=TRUE)
  })
  
  #This outputs the image being detected
  output$detectimage = renderPlot({
    detect.click()
  })
  
  #This outputs the image being saved
  output$imagesave = renderPlot({
    saveimage.click()
  })
  
  #This outputs the live face detection video
  output$liveimagedetect = renderPlot({
    live.click()
  })
}

shinyApp(ui,server)
