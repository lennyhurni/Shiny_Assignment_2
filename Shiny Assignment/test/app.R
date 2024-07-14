ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)


server1 <- function(input, output, server) {
  output$greeting <- renderText({
    paste0("Hello ", input$name)
  })
}

server2 <- function(input, output, server) {
  output$greeting <- renderText({
    greeting <- paste0("Hello ", input$name)
    greeting
  })
  
  server3 <- function(input, output, server) {
    output$greeting <- renderText({
      paste0("Hello ", input$name)
    })
  }
  
}