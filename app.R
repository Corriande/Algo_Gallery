library(shiny)
library(dplyr)
library(ggplot2)
library(cluster)
library(FactoMineR)
library(factoextra)
library(readr)
library(shinyjs)
library(shinyalert)  # For notifications







# Load the modern_gallery dataset
modern_gallery <- read_csv("colors_gallery.csv")

# Select numeric columns for clustering
numeric_columns <- modern_gallery[, c("Impressionist", "Many_Colors",
                                      "Highly_Detailed", "Abstract", 
                                      "Landscape", "Ornamental_Pattern", 
                                      "Human_Subject", 
                                      "Animal_Subject", "Architecture_Subject")]





# Function to calculate cosine similarity
cosine_similarity <- function(v1, v2) {
  sum(v1 * v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2)))
}

# UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Algo Gallery"),
  tags$style(HTML("
    .resizable-text {
      font-size: 24px;
      line-height: 1.5;
    }
    .fixed-position-buttons {
      position: fixed;
      top: 70%;
      left: 80%;
      transform: translate(-50%, -50%);
      z-index: 1000;
    }
  ")),
  tabsetPanel(
    tabPanel("Home",
              h1("Content Recomendation Algorithm using AI Image Classification"),
              h3("By Gabriel del Valle"),
              h4("September 2024"),
              h4("NYC Data Science Academy"),
              h4("www.linkedin.com/in/gabrielxdelvalle"),
             HTML("
                    
                    <p>The purpose of this project is to create a simplified context to apply content recommendation techniques in an interactive app.</p>
                    
                    <p>Interactions are limited to the like / dislike of artworks.</p>
                    
                    <p>There are 974 stylistically diverse modern artworks in this app, which I webscraped from a public domain database.</p>
                    
                    <p>Each artwork has been analyzed by OpenAI's open source CLIP image classification model, which takes as inputs a list of categories to score the image on. I chose the following categories:</p>
                    
                    <ul>
                    <li>Impressionist</li>
                    <li>Landscape</li>
                    <li>Abstract</li>
                    <li>Human Subject</li>
                    <li>Animal Subject</li>
                    <li>Architecture Subject</li>
                    <li>Ornamental Pattern</li>
                    <li>Highly Detailed</li>
                    <li>Many Colors</li>
                    </ul>
                    
                    <p>The scores for each of these categories are interdependent, and thus the values sum to 1.</p>
                    
                    <p>These values are used to model the user's aesthetic preferences, based on which images they like or dislike.</p>
                    
                    <p>In most real life cases, rather than using a visual analysis of each content, comparing the likes and dislikes of user profiles would allow a platform to efficiently capture the nuances of user preferences.</p>
                    
                    <p>Example: If a group of users consistently like and dislike similar content, then if a user in the group likes a post the others haven't yet seen, it's sensible to recommend that post to the group of users.</p>
                    
                    <p>The visual analysis used in this project is used to compensate for a lack of user profile data. While unconventional and potentially computationally expensive, this kind of visual analysis may have context-specific benefits.</p>
                 
             "),
             h2("User Guide"),
             h3("Gallery"),
             HTML("<p> This is where images are presented to the user to like / dislike </p>
                  <p> The first 20 images are chosen to maximize the variance in visual qualities that the user is exposed to, in order to efficiently calibrate their preference data. </p>
                  <p> After liking/disliking the first 20 images, the following images presented to the user will be chosen randomly. The app will predict if the user will like or dislike each new image based on their previous likes and dislikes."),
             h3("Art Cluster"),
             HTML("<p> K Means Clustering, an unsupervised machine learning algorithm, groups artworks based on distance in a dimensional space R^n for n variables.</p>
                  <p> The integrity of the clusters can be assesed by the Silhouette Scores of each observation. Silhouette Scores measure how well an artwork fits its cluster, with scores ranging -1 to 1. A value of 0 indicates an artwork lies on the boundary of two clusters. </p>
                  <p> The number of clusters was set to 5 based on a comparison of the Within Cluster Sum of Squares of models with various numbers of clusters.</p>
                  <p> Each time the app is run a new cluster model will be fit, with random variations each time. </p>
                  <p> In this project, clustering was used as one of the first means used to assess how well the chosen variables used in the image classification with CLIP captured the variance and nuance of each artwork. Clustering works well for this kind of data because the values are interdependent and sum to 1. </p>"),
             h3("Cluster Gallery"),
             HTML("<p> Allows the user to directly asses the effectiveness of cluster (and thus the chosen categories for CLIP image classification) by examining which artworks are in which cluster.</p>
                  <p> For each cluster, provides a list of the average value of each visual variable, and the number of artwroks in each cluster. </p>"),
             h3("PCA Analysis"),
             HTML("<p> Projects the multi-dimensional variable space into 2D to portrays the impact of each variable on the variance of the overall data as a vector.</p>
                  <p> Example interpretation: In this project the vertical axis Dim1 can be interpreted as a spectrum of subjects in the artworks, with Abstract shapes and objects at the top, and more identifiable subjects such as animals and people at the bottom. </p>"),
             h3("Initiation"),
             HTML("<p> Demonstrates how the first 20 images, chosen to maximize variance and used for user preference callibration are calculated. Displays the images. </p>"),
             h3("Similarity"),
             HTML("<p> Uses a Cosine Similarity Matrix to identify what is the most similar image to each image. A way to assess the effectiveness of Cosine Similarity for user preference prediction. </p>"),
             h3("Accuracy"),
             HTML("<p> Plots the accuracy of predictions over number of interactions to demonstrate how well predictions are working </p> 
                  <p> Also keeps track of the user's like to dislike ratio for incorrect predictions to identify bias in the recomendation system. </p>
                  <p> Example: If every incorrect prediction is a dislike, then the predictions are not valid, since the app is just predicting that the user will like every image. </p>")
             
             
  ),
    
    
    
    tabPanel("Gallery",
             fluidRow(
               column(8, uiOutput("artImage")),
               column(4,
                      h3(textOutput("artTitle")),
                      h4(uiOutput("artInfo"), class = "resizable-text"),
                      div(
                        actionButton("likeBtn", "Like", class = "btn-success", style = "display:none;"),
                        actionButton("dislikeBtn", "Dislike", class = "btn-danger", style = "display:none;"),
                        class = "fixed-position-buttons"
                      ),
                      h4(textOutput("counter"))
               )
             )
    ),
    tabPanel("Art Cluster",
             mainPanel(
               h4("Cluster Plot"),
               plotOutput("clusterPlot"),
               h4("Silhouette Score Summary"),
               verbatimTextOutput("silhouetteScore"),
               plotOutput("silhouettePlot")
             )
    ),
    tabPanel("Cluster Gallery",
             uiOutput("galleryCluster5")
    ),
    tabPanel("PCA Analysis",
             h4("PCA of Clusters"),
             plotOutput("pcaPlot"),
             verbatimTextOutput("pcaSummary")
    ),
    tabPanel("Initiation",  # New tab for variance-representative images
             h2("Variance-Representative Images"),
             h5(
               HTML(
                 "<p style='text-indent: 30px; line-height: 1.5; margin-left: 20px; margin-right: 20px;'>
                  Each image represents either the centroid of a cluster
                (the typical values of the cluster) or an extreme
                end (either Max or Min) of a PCA dimension. These few images are
                selected to represent the widest spectrum of variance in the 
                visual qualities of the artworks. By prompting the user
                with these images as the first sequence of content, the user
                preference data can be quickly and efficiently calibrated,
                providing a starting point for content recommendations that can
                be better tuned.
                </p>")
             ),
             h3("Centroid Images (K-means Clusters)"),
             uiOutput("centroidImages"),  # Display cluster centroid images
             h3("PCA Extreme Images"),
             uiOutput("pcaImages")        # Display PCA extreme images
    ),
    tabPanel("Similarity",
             h3("Most Similar Image Pairs"),
             uiOutput("similarityPairs")
    ),
    tabPanel("Accuracy",
             h4("Recommendation Accuracy Over Time"),
             plotOutput("accuracyPlot"),
             verbatimTextOutput("accuracySummary"),
             h4("Incorrect Predictions by Interaction Type"),
             plotOutput("incorrectPredictionPlot")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive value to track the current index
  current_index <- reactiveVal(1)
  initiation_mode <- reactiveVal(TRUE)  # Track if user is in initiation phase
  #initiation_images <- reactiveValues(images = list())  # Store the indices of initiation images
  liked_images <- reactiveVal(data.frame())  # Store liked images
  disliked_images <- reactiveVal(data.frame())  # Store disliked images
  
  
  # Reactive value to track the counter
  counter <- reactiveVal(0)
  
  
  # Display the counter value
  output$counter <- renderText({
    paste("Images liked/disliked:", counter())
  })
  
  # Function to update the displayed art and its info
  update_art <- function(index) {
    current_art <- data_gallery[index, ]
    image_url <- current_art$Image_URL
    
    output$artImage <- renderUI({
      tags$img(src = image_url, alt = current_art$Title, style = "max-width:100%;height:auto;")
    })
    
    output$artTitle <- renderText({
      current_art$Title
    })
    
    output$artInfo <- renderUI({
      HTML(paste(
        "<strong>Artist:</strong> ", current_art$Artist, "<br/>",
        "<strong>Year:</strong> ", current_art$Year, "<br/>",
        "<strong>Nationality:</strong> ", current_art$Nationality, "<br/>",
        "<strong>Birth:</strong> ", current_art$Birth
      ))
    })
  }
  
  
  
  # Updated function to predict if a new image will be liked or disliked based on comparison to liked and disliked images
  predict_liked <- function(new_image, liked_images, disliked_images, similarity_threshold = 0.7) {
    # Ensure liked_images and disliked_images are initialized
    if (nrow(liked_images) == 0 && nrow(disliked_images) == 0) {
      return(FALSE)  # No prior images to compare, default to predicting dislike
    }
    
    # Select only numeric columns for comparison
    numeric_new_image <- as.numeric(new_image[names(numeric_columns)])
    
    # Initialize similarity values
    max_like_similarity <- -Inf
    max_dislike_similarity <- -Inf
    
    # Compare with liked images if there are any
    if (nrow(liked_images) > 0) {
      numeric_liked_images <- liked_images[, names(numeric_columns)]
      new_image_norm <- sqrt(sum(numeric_new_image^2))
      liked_images_norms <- sqrt(rowSums(numeric_liked_images^2))
      
      # Compute cosine similarities with liked images
      like_similarities <- apply(numeric_liked_images, 1, function(liked_image_row) {
        sum(numeric_new_image * as.numeric(liked_image_row)) / (new_image_norm * sqrt(sum(liked_image_row^2)))
      })
      
      max_like_similarity <- max(like_similarities)
    }
    
    # Compare with disliked images if there are any
    if (nrow(disliked_images) > 0) {
      numeric_disliked_images <- disliked_images[, names(numeric_columns)]
      new_image_norm <- sqrt(sum(numeric_new_image^2))
      
      # Compute cosine similarities with disliked images
      dislike_similarities <- apply(numeric_disliked_images, 1, function(disliked_image_row) {
        sum(numeric_new_image * as.numeric(disliked_image_row)) / (new_image_norm * sqrt(sum(disliked_image_row^2)))
      })
      
      max_dislike_similarity <- max(dislike_similarities)
    }
    
    # Compare the max similarities between liked and disliked images
    if (max_like_similarity > max_dislike_similarity && max_like_similarity > similarity_threshold) {
      return(TRUE)  # Predict like
    } else if (max_dislike_similarity > similarity_threshold) {
      return(FALSE)  # Predict dislike
    }
    
    return(FALSE)  # Default to dislike if thresholds are not exceeded
  }
  
  
  # Like/Dislike button logic
  handle_user_interaction <- function(liked) {
    
      current_art <- data_gallery[current_index(), ]
      
      interaction_data <- data.frame(
        Title = current_art$Title,
        Artist = current_art$Artist,
        Year = current_art$Year,
        Nationality = current_art$Nationality,
        Birth = current_art$Birth,
        Impressionist = current_art$Impressionist,
        Many_Colors = current_art$Many_Colors,
        Highly_Detailed = current_art$Highly_Detailed,
        Abstract = current_art$Abstract,
        Landscape = current_art$Landscape,
        Ornamental_Pattern = current_art$Ornamental_Pattern,
        Human_Subject = current_art$Human_Subject,
        Animal_Subject = current_art$Animal_Subject,
        Architecture_Subject = current_art$Architecture_Subject,
        Liked = liked,
        stringsAsFactors = FALSE
      )

      
      #used for Accurarcy calculation
      image_id <- current_art$Title
      
      if (initiation_mode() == FALSE){
        # Update accuracy after the interaction
        
        prediction <- predict_liked(current_art, liked_images(), disliked_images())
        
        
        update_accuracy(prediction, liked)
        
      }
      
      # Update liked or disliked images
      if (liked) {
        if (nrow(liked_images())== 0){
          liked_images(interaction_data)
        } 
        else{
          liked_images(rbind(liked_images(), interaction_data))
        }
      } 
      else {
        if (nrow(liked_images())== 0){
          disliked_images(interaction_data)
        } 
        else {
          disliked_images(rbind(disliked_images(), interaction_data))
        }
      }
      
      
      # Increment the counter
      counter(counter() + 1)
      current_index(current_index() + 1)
      
      
      
      
      
      if (initiation_mode()) {
        # Handle initiation phase
        if (counter() > length(initiation_images)) {
          initiation_mode(FALSE)
          shinyalert("Calibration Complete", 
                     "Your initial content calibration is complete. Each of the next images are randomly sampled. The app will predict if you will like or dislike the image and record if it was right or wrong. Prediction data will be updated with each interaction.", 
                     type = "success")
        }
      } 
      
      
      observeEvent(current_index(), {
        update_art(current_index())
      })
  }
  
  
  
  # Like button logic
  observeEvent(input$likeBtn, {
    handle_user_interaction(TRUE)
  })
  
  # Dislike button logic
  observeEvent(input$dislikeBtn, {
    handle_user_interaction(FALSE)
  })
  

  
  
  
  # Clustering and gallery logic...
  # Perform k-means clustering for 5 clusters
  cluster5_result <- kmeans(numeric_columns, centers = 5)
  
  # Define colors for each cluster
  cluster_colors <- c("#00AFBB", "#E7B800", "#f1b1fc", "#FF5733", "#C70039")
  
  # Perform PCA for dimensionality reduction
  pca_result <- PCA(numeric_columns, graph = FALSE)
  
  n_centroids_per_cluster <- 3
  
  # Select centroids from k-means
  centroid_indices <- unlist(apply(cluster5_result$centers, 1, function(center) {
    distances <- apply(numeric_columns, 1, function(row) sum((row - center)^2))
    
    # Sort by distance and get indices of the n closest points
    closest_indices <- order(distances)[1:n_centroids_per_cluster]
    
    return(closest_indices)
  }))
  
  
  # Select extreme points from PCA (extreme values along PC1 and PC2)
  pca_extremes <- apply(pca_result$ind$coord[, 1:2], 2, function(coord) {
    max_index <- which.max(coord)
    min_index <- which.min(coord)
    c(max_index, min_index)
  })
  
  
  
  # Combine centroids and PCA extremes into a single vector
  initiation_images <- c(centroid_indices, as.vector(pca_extremes))
  
  # Move initiation images to the top of the dataset
  initiation_df <- modern_gallery[initiation_images, ]
  
  # Exclude initiation images from the remaining dataset
  remaining <- modern_gallery[-initiation_images, ]
  
  # Randomly sample the remaining images
  sample_df <- remaining %>% sample_n(nrow(remaining))
  
  # Combine initiation images and random sample
  data_gallery <- bind_rows(initiation_df, sample_df)
  
  
  
  
observe({
    # Show the like and dislike buttons
    shinyjs::show("likeBtn")
    shinyjs::show("dislikeBtn")
    
    # Initialize initiation images and start the gallery with the first initiation image
    update_art(current_index())
})
  
  
  
  # Compute the cosine similarity matrix for all images at once
  cosine_similarity_matrix <- function(image_data) {
    norms <- sqrt(rowSums(image_data^2))
    similarity_matrix <- image_data %*% t(image_data) / (norms %*% t(norms))
    diag(similarity_matrix) <- -Inf  # Set diagonal to -Inf to avoid self-pairing
    return(similarity_matrix)
  }
  
  # Function to find most similar image for each image
  find_most_similar_images_optimized <- function(image_data) {
    similarity_matrix <- cosine_similarity_matrix(as.matrix(image_data))
    most_similar <- apply(similarity_matrix, 1, which.max)
    return(most_similar)
  }
  
  
  # Compute most similar images when the app starts
  most_similar <- find_most_similar_images_optimized(numeric_columns)
  
  
  
  
  # Render similarity pairs with artist name
  output$similarityPairs <- renderUI({
    image_pairs <- lapply(1:nrow(modern_gallery), function(i) {
      img_index <- most_similar[i]
      tags$div(
        tags$div(style = "display: inline-block; vertical-align: top;",
                 tags$img(src = modern_gallery$Image_URL[i], width = "300px", height = "auto"),
                 tags$p(paste(modern_gallery$Title[i], "by", modern_gallery$Artist[i]))
        ),
        tags$div(style = "display: inline-block; vertical-align: top; margin-left: 20px;",
                 tags$img(src = modern_gallery$Image_URL[img_index], width = "300px", height = "auto"),
                 tags$p(paste("Most similar to:", modern_gallery$Title[img_index], "by", modern_gallery$Artist[img_index]))
        ),
        tags$hr()  # Add a separator line between image pairs
      )
    })
    
    do.call(tagList, image_pairs)  # Combine all image pairs into a single UI element
  })
  
  
  
  
  
  
  
  # Display cluster centroid images
  output$centroidImages <- renderUI({
    images <- lapply(1:length(centroid_indices), function(i) {
      img_index <- centroid_indices[i]
      
      # Calculate the cluster number and the index within the cluster
      cluster_number <- ((i - 1) %/% n_centroids_per_cluster) + 1
      centroid_position <- ((i - 1) %% n_centroids_per_cluster) + 1
      
      tags$div(
        tags$img(src = modern_gallery$Image_URL[img_index], style = "max-width:500px; height:auto; margin:5px;"),
        tags$p(paste("Cluster", cluster_number, "- Centroid", centroid_position))  # Label the cluster and centroid number
      )
    })
    do.call(tagList, images)
  })
  
  # Display PCA extreme images
  output$pcaImages <- renderUI({
    images <- lapply(1:ncol(pca_extremes), function(i) {
      max_index <- pca_extremes[1, i]
      min_index <- pca_extremes[2, i]
      component <- colnames(pca_result$ind$coord)[i]
      list(
        tags$div(
          tags$img(src = modern_gallery$Image_URL[max_index], style = "max-width:500px; height:auto; margin:5px;"),
          tags$p(paste("PCA", component, "Max"))
        ),
        tags$div(
          tags$img(src = modern_gallery$Image_URL[min_index], style = "max-width:500px; height:auto; margin:5px;"),
          tags$p(paste("PCA", component, "Min"))
        )
      )
    })
    do.call(tagList, unlist(images, recursive = FALSE))
  })
  
  
  
  # Gallery for Cluster 5 with color-coded titles
  output$galleryCluster5 <- renderUI({
    # Define colors for each variable
    variable_colors <- list(
      "Human_Subject" = "darkblue",
      "Animal_Subject" = "darkred",
      "Landscape" = "darkgreen",
      "Impressionist" = "darkorange",
      "Abstract" = "purple",
      "Highly_Detailed" = "darkcyan",
      "Architecture_Subject" = "brown",
      "Ornamental_Pattern" = "darkgoldenrod",
      "Many_Colors" = "blue"
    )
    
    clusters <- 1:5
    galleries <- lapply(clusters, function(cluster) {
      # Calculate the mean of each category for the cluster
      cluster_means <- colMeans(numeric_columns[cluster5_result$cluster == cluster, ])
      
      # Sort the categories by their mean values in descending order
      sorted_means <- sort(cluster_means, decreasing = TRUE)
      
      # Create a title with all categories and their rounded mean values with line breaks and color-coding
      category_ranks <- paste0(
        lapply(names(sorted_means), function(var) {
          # Color each variable consistently
          color <- variable_colors[[var]]
          paste0("<span style='color:", color, ";'>", var, " (", round(sorted_means[var], 2), ")</span>")
        }), collapse = "<br>"
      )
      
      # Create a black-colored title for each cluster
      cluster_title <- HTML(paste(
        "<span style='color:black;'>Cluster ", cluster, ":</span><br>", 
        category_ranks, 
        "<br><span style='color:black;'>- Rows: ", sum(cluster5_result$cluster == cluster), "</span>"
      ))
      
      # Display images for each artwork in the cluster
      images <- lapply(which(cluster5_result$cluster == cluster), function(i) {
        tags$img(src = data_gallery$Image_URL[i], width = "300px", height = "auto", style = "margin:5px;")
      })
      
      # Return a list of cluster title and images
      list(tags$h4(cluster_title), tags$div(images))
    })
    
    do.call(tagList, galleries)  # Combine the UI elements
  })
  
  
  
  
  
  
  
  
  
  # Plot for PCA
  output$pcaPlot <- renderPlot({
    fviz_pca_var(pca_result, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
  })
  
  # Output PCA summary
  output$pcaSummary <- renderPrint({
    summary(pca_result)
  })
  
  # Clustering plot (fixed to 5 clusters)
  output$clusterPlot <- renderPlot({
    fviz_cluster(list(data = numeric_columns, cluster = cluster5_result$cluster), 
                 geom = "point", ellipse.type = "convex", 
                 palette = cluster_colors, 
                 ggtheme = theme_minimal())
  })
  
  # Output the silhouette score summary
  output$silhouetteScore <- renderPrint({
    dist_matrix <- dist(numeric_columns)  # Distance matrix
    silhouette_scores <- silhouette(cluster5_result$cluster, dist_matrix)
    summary(silhouette_scores)
  })
  
  # Plot the silhouette scores
  output$silhouettePlot <- renderPlot({
    dist_matrix <- dist(numeric_columns)
    silhouette_scores <- silhouette(cluster5_result$cluster, dist_matrix)
    plot(silhouette_scores, col = cluster_colors, border = NA)
  })
  
  
  ###Accuracy tab code
  
  # Reactive value to store correct and total predictions
  accuracy_tracker <- reactiveValues(correct = 0, 
                                     total = 0,
                                     accuracy_over_time = numeric(),
                                     correctness = logical(),
                                     interaction_type = character(),
                                     incorrect_like = 0,
                                     incorrect_dislike = 0)
  
  
  
  # Update accuracy after each interaction
  update_accuracy <- function(prediction, liked) {
    
    correct <- (prediction == liked)
    
    # Increment total interactions
    accuracy_tracker$total <- accuracy_tracker$total + 1
    
    # Increment correct predictions if applicable
    if (correct) {
      accuracy_tracker$correct <- accuracy_tracker$correct + 1
    }else {
      # Track incorrect predictions for likes or dislikes
      if (liked) {
        accuracy_tracker$incorrect_like <- accuracy_tracker$incorrect_like + 1
      } else {
        accuracy_tracker$incorrect_dislike <- accuracy_tracker$incorrect_dislike + 1
      }
    }
    # Calculate accuracy and store it at each interaction step
    accuracy_tracker$correctness <- c(accuracy_tracker$correctness, correct)
    current_accuracy <- accuracy_tracker$correct / accuracy_tracker$total
    accuracy_tracker$accuracy_over_time <- c(accuracy_tracker$accuracy_over_time, current_accuracy)
  }
  
  # Render the accuracy line graph dynamically for each interaction
  output$accuracyPlot <- renderPlot({
    interaction_count <- 1:accuracy_tracker$total
    accuracy_over_time <- accuracy_tracker$accuracy_over_time
    correctness <- accuracy_tracker$correctness 
    
    # Ensure there's data to plot
    if (length(interaction_count) > 0) {
      plot(interaction_count, accuracy_over_time, type = "l", col = "blue",
           xlab = "Number of Interactions", ylab = "Accuracy (Correct/Total)",
           main = "Recommendation Accuracy Over Time")
      # Add points to indicate correct/incorrect predictions
      points(interaction_count, accuracy_over_time, 
             pch = 19,  # solid circle marker
             col = ifelse(correctness, "green", "red"))  # Green for correct, Red for incorrect
    }
  })
  
  # Display accuracy summary
  output$accuracySummary <- renderText({
    paste("Correct predictions:", accuracy_tracker$correct,
          "\nTotal interactions:", accuracy_tracker$total,
          "\nAccuracy:", round(accuracy_tracker$correct / accuracy_tracker$total, 2),
          "\nIncorrect Likes:", accuracy_tracker$incorrect_like,
          "\nIncorrect Dislikes:", accuracy_tracker$incorrect_dislike)
  })
  
  # Render bar plot for incorrect predictions (like vs dislike)
  output$incorrectPredictionPlot <- renderPlot({
    # Data for bar plot
    categories <- c("Like", "Dislike")
    incorrect_counts <- c(accuracy_tracker$incorrect_like, accuracy_tracker$incorrect_dislike)
    
    # Create the bar plot
    barplot(incorrect_counts, 
            names.arg = categories, 
            col = c("blue", "red"), 
            main = "Incorrect Predictions by Interaction Type", 
            ylab = "Number of Incorrect Predictions")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)