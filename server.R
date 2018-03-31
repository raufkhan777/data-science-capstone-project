# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Load libraries
suppressWarnings(library("shiny"))
suppressWarnings(library("rsconnect"))
suppressWarnings(library("tokenizers"))
suppressWarnings(library("dplyr"))
suppressWarnings(library("tidyr"))
suppressWarnings(library("stringr"))
suppressWarnings(library("textclean"))

#######################################################
# Load data
all_sample <- scan(file="./Data/all_sample.csv",what ="character", sep="\n",encoding = "UTF-8")

#######################################################
# Tokenize blogs and unlist
unigram_all <- unlist(tokenize_ngrams(all_sample, n = 1))
bigram_all <- unlist(tokenize_ngrams(all_sample, n = 2))
trigram_all <- unlist(tokenize_ngrams(all_sample, n = 3))
quadgram_all <- unlist(tokenize_ngrams(all_sample, n = 4))

# Create dataframe
unigram_all <- data.frame(term = unigram_all)
bigram_all <- data.frame(term = bigram_all)
trigram_all <- data.frame(term = trigram_all)
quadgram_all <- data.frame(term = quadgram_all)

# Count frequencies 
count_unigram_all <- unigram_all %>% count(term, sort = T)
count_bigram_all <- bigram_all %>% count(term, sort = T)
count_trigram_all <- trigram_all %>% count(term, sort = T)
count_quadgram_all <- quadgram_all %>% count(term, sort = T)

#######################################################
# Separate one column into multiple columns
count_bigram_all <- count_bigram_all %>% separate(term, c("Term1", "Term2"), remove = FALSE)
count_trigram_all <- count_trigram_all %>% separate(term, c("Term1", "Term2", "Term3"), remove = FALSE)
count_quadgram_all <- count_quadgram_all %>% separate(term, c("Term1", "Term2", "Term3", "Term4"), remove = FALSE)

count_bigram_all <- unite(count_bigram_all, FirstOne, Term1, sep=' ')
count_trigram_all <- unite(count_trigram_all, FirstTwo, Term1:Term2, sep=' ')
count_quadgram_all <- unite(count_quadgram_all, FirstThree, Term1:Term3, sep=' ')

#######################################################

# Define server logic 
shinyServer(function(input, output) {
  
  get_inputWords<-function(inputText){
    
    inputText <- strip(inputText, digit.remove = TRUE, apostrophe.remove = FALSE, lower.case = FALSE)
    inputText <- stringr::str_replace_all(inputText,"[\\s]+", " ")
    inputText <- inputText %>% paste(collapse=" ")
    test_term <- inputText %>% tolower()
    test_term1 <- paste(word(test_term, -3), word(test_term, -2), word(test_term, -1))
    test_term2 <- paste(word(test_term, -2), word(test_term, -1))
    test_term3 <- word(test_term, -1)
    lambda = 0.4
    
    # Find subset of candidates in 4-grams
    count_quadgram_sub <- subset(count_quadgram_all, FirstThree == test_term1)
    # Find freq of 3-gram
    f_3gram <- count_trigram_all$n[match(c(test_term1), count_trigram_all$term)]
    # Create MLE score
    count_quadgram_sub$Score <- count_quadgram_sub$n/(f_3gram)
    # Order by score
    count_quadgram_sub <- count_quadgram_sub[order(-count_quadgram_sub$Score, -count_quadgram_sub$n), ]
    # Subset for scoring
    quadgram_sub <- count_quadgram_sub[c(1,3,5)]
    colnames(quadgram_sub)[2] <- "Prediction"
    colnames(quadgram_sub)[3] <- "SBO_Score"
    quadgram_sub <- quadgram_sub[1:3,]
    quadgram_sub$term <- as.character(quadgram_sub$term)
    quadgram_sub$Prediction <- as.character(quadgram_sub$Prediction)
    
    # Find subset of candidates in 3-grams
    count_trigram_sub <- subset(count_trigram_all, FirstTwo == test_term2)
    # Find freq of 2-gram
    f_2gram <- count_bigram_all$n[match(c(test_term2), count_bigram_all$term)]
    # Create MLE score
    count_trigram_sub$Score <- count_trigram_sub$n/(f_2gram)
    # Order by score
    count_trigram_sub <- count_trigram_sub[order(-count_trigram_sub$Score, -count_trigram_sub$n), ]
    # Multiply score by lambda
    count_trigram_sub$ScoreL <- count_trigram_sub$Score*lambda
    # Subset for scoring
    trigram_sub <- count_trigram_sub[c(1,3,6)]
    colnames(trigram_sub)[2] <- "Prediction"
    colnames(trigram_sub)[3] <- "SBO_Score"
    trigram_sub <- trigram_sub[1:3,]
    trigram_sub$term <- as.character(trigram_sub$term)
    trigram_sub$Prediction <- as.character(trigram_sub$Prediction)
    
    # Find subset of candidates in 2-grams
    count_bigram_sub <- subset(count_bigram_all, FirstOne == test_term3)
    # Find freq of 1-gram
    f_1gram <- count_unigram_all$n[match(c(test_term3), count_unigram_all$term)]
    # Create MLE score
    count_bigram_sub$Score <- count_bigram_sub$n/(f_1gram)
    # Order by score
    count_bigram_sub <- count_bigram_sub[order(-count_bigram_sub$Score, -count_bigram_sub$n), ]
    # Multiply score by lambda^2
    count_bigram_sub$ScoreLL <- count_bigram_sub$Score*lambda*lambda
    # Subset for scoring
    bigram_sub <- count_bigram_sub[c(1,3,6)]
    colnames(bigram_sub)[2] <- "Prediction"
    colnames(bigram_sub)[3] <- "SBO_Score"
    bigram_sub <- bigram_sub[1:3,]
    bigram_sub$term <- as.character(bigram_sub$term)
    bigram_sub$Prediction <- as.character(bigram_sub$Prediction)
    
    # Find 1-gram
    # Create 1-gram score
    N <- sum(count_unigram_all$n)
    count_unigram_all$Score <- count_unigram_all$n/N
    unigramScore <- count_unigram_all$Score[match(c(test_term3), count_unigram_all$term)]
    # Multiply score by lambda^3
    count_unigram_all$ScoreLLL <- count_unigram_all$Score*lambda*lambda*lambda
    # Subset for scoring
    unigram_sub <- count_unigram_all[c(1,1,4)]
    unigram_sub$term <- "unigram"
    colnames(unigram_sub)[2] <- "Prediction"
    colnames(unigram_sub)[3] <- "SBO_Score"
    unigram_sub <- unigram_sub[1:3,]
    unigram_sub$term <- as.character(unigram_sub$term)
    unigram_sub$Prediction <- as.character(unigram_sub$Prediction)
    
    # Combine scoring datasets
    AllBind <- bind_rows(quadgram_sub, trigram_sub, bigram_sub, unigram_sub)
    AllBind <- AllBind[order(-AllBind$SBO_Score), ]
    
    # Result: print highest score
    outputText <- print(AllBind$Prediction[1])

    # return(tolower(inputText))
    return(outputText)
  }
  
  output$wordPred <- renderText({
    x <- get_inputWords(input$text)
    x
  })   

  
})
