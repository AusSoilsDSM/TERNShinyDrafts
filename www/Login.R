#### Log in module ###
USER <- reactiveValues(Logged = Logged)

passwdInput <- function(inputId, label) {
  tagList(
    tags$label(label),
    tags$input(id = inputId, type="password", value="t")
    #tags$input(id = inputId, type="password", value="tenosol")
  )
}



output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {

    titlePanel("TERN Landscapes")
    wellPanel(
      tags$div(class = "span22", width=200, 
      h3("NAWRA Draft Land Suitabilities"),
      p("The data in this site is only available to project members at this point in time"),
      p("Please ",HTML('<a href=mailto:ross.searle@csiro.au?subject=Access%20to%20Draft%20NAWRA%20Land%20Suitability%20Data>email</a>'), " Ross Searle  if you would like to access this data"),
      textInput("userName", "User Name:", value=""),
      passwdInput("passwd", "Password:"),
      
      br(),
      br(),
      br(),
      actionButton("Login", "Log in")
      
      )
    )
  }
})

output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
   if (input$Login > 0) {
     
      Username <- isolate(input$userName)
      Password <- isolate(input$passwd)
      
      print(input$userName)
      print(input$passwd)
      
      Id.username <- which(PASSWORD$usr == Username)
      Id.password <- which(PASSWORD$pwd    == Password)
      if (length(Id.username) > 0 & length(Id.password) > 0) {
        if (Id.username == Id.password) {
          USER$Logged <- TRUE
        } 
      } else  {
        "User name or password failed!"
      }
    } 
    }
  }
})

