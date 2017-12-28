library(shiny)
library(DT)
mydata = data.frame(id=letters[1:5], val=sample(10,5,T))

ui = fluidPage(dataTableOutput("table"),
               textInput('NewID', 'Enter new ID'),
               numericInput('NewVal', 'Enter new val', 1),
               actionButton("goButton", "Update Table"))

server = function(input,output){
  output$table = renderDataTable(mydata)
  update = eventReactive(input$goButton, {
    newrow = data.frame(id = input$NewID, val = input$NewVal)
    mydata = rbind(mydata, newrow)
  })
}

shinyApp(ui,server)