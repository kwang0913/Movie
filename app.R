library(shiny)
library(tidyverse)
library(recommenderlab)
data("MovieLense")

ui <- fluidPage(
    
    titlePanel("Movie Recommender System"),

    fluidRow(
        column(4,
               numericInput("usern", label = h3("Minimum Number of Movies per User"),
                            value = 20, min = 0, max = 50),
               numericInput("movien", label = h3("Minimum Number of Viewers per Movie"),
                            value = 20, min = 0, max = 50),
               selectInput("selected", label = h3("Select Movies You Like"),
                           choices = MovieLenseMeta$title, multiple = T),
               selectInput("methoded", label = h3("Recommender Method"),
                           choices = recommenderRegistry$get_entries(dataType = "binaryRatingMatrix") %>%
                               names() %>% str_remove("_binaryRatingMatrix"), selected = "RANDOM")
               ),
        
        column(8,  
               h3("You Might Like These Too!"),
               tableOutput("table")
               )
    )
)

movie_recommendation <- function(usern, movien, selected, methoded){
    
    row_num <- MovieLenseMeta$title %in% selected
    userSelect <- matrix(NA, dim(MovieLense)[2])
    userSelect[row_num] <- 5
    userSelect <- t(userSelect)
    
    ratingmat <- as(MovieLense[rowCounts(MovieLense) >= usern, ], "matrix")
    colnames(userSelect) <- colnames(ratingmat)
    ratingmat2 <- rbind(userSelect, ratingmat)
    
    ratingmat2 <- as(ratingmat2, "realRatingMatrix") %>% 
        .[, colCounts(.) >= movien] %>% binarize(minRating = 4) 
    
    recommender_model <- Recommender(ratingmat2, method = methoded)
    recom <- predict(recommender_model, ratingmat2[1], n = 10)
    recom_list <- as(recom, "list") %>% as.data.frame()
    
    colnames(recom_list) <- "Your Recommendation"
    
    return(recom_list)
}

server <- function(input, output) {
    
    output$table <- renderTable({
        movie_recommendation(input$usern, input$movien, input$selected, input$methoded)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
