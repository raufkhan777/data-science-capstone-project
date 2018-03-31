  Data Science Capstone project
#  data set

# Run garbage collection to clear out memory that's no longer being used
gc()

# ThinkPad Yoga 260 Signature Edition
# Installed RAM 8.0 GB (7.87 GB usable)
# memory.limit() # Confirms memory limit
# ls() # list of R objects occupying RAM
rm(list = ls()) # clear objects from R memory that are no longer required

suppressWarnings(library("dplyr"))
suppressWarnings(library("tidyr"))
suppressWarnings(library("stringr"))
suppressWarnings(library("textclean"))

# Set working directory and load libraries
setwd("~/R/ Data Science Capstone/Capstoneproject/Data") 

# Load datasets
data_blogs <- scan(file="en_US.blogs.txt",what ="character", sep="\n",encoding = "UTF-8")
data_news <- scan(file="en_US.news.txt",what ="character", sep="\n",encoding = "UTF-8")
# data_twitter <- scan(file="en_US.twitter.txt",what ="character", sep="\n",encoding = "UTF-8")

# Bind data and take sample
data_all <- rbind(data_blogs, data_news)
n = 0.15
all_sample <- sample(data_all, floor(n*length(data_blogs)), replace = FALSE)

# Remove everything but letters and apostrophes
all_sample <- strip(all_sample, digit.remove = TRUE, apostrophe.remove = FALSE, lower.case = FALSE)
# Remove all but one white space
all_sample <- stringr::str_replace_all(all_sample,"[\\s]+", " ")
# Collapse into single vector
all_sample <- all_sample %>% paste(collapse=" ")
# Make everything lowercase
all_sample <- all_sample %>% tolower()

# Write to csv
write.table(all_sample, file = "~/R/ Data Science Capstoneproject/TextPredDataProd/Data/all_sample.csv", row.names=FALSE, col.names = FALSE)
© 2018 GitHub, Inc.
