# Read data

  blog.file <- "data/en_US/en_US.blogs.txt"
  blogs <- readLines(blog.file, encoding="UTF-8")

  news.file <- "data/en_US/en_US.news.txt"
  news  <- readLines(news.file, encoding="UTF-8")

  twitter.file <- "data/en_US/en_US.twitter.txt"
  twitter <- readLines(twitter.file, encoding="UTF-8")

# Descriptive Analysis
  table <- as.data.frame(matrix(ncol = 3))
  table <- rbind(table, c('en_US.blogs.txt', round(file.info(blog.file)$size / 1024^2, 3), length(blogs) ) )
  table <- rbind(table, c('en_US.news.txt', round(file.info(news.file)$size / 1024^2, 3), length(news) ) )
  table <- rbind(table, c('en_US.twitter.txt', round(file.info(twitter.file)$size / 1024^2, 3), length(twitter) ) )
  names(table) <- c('File', 'Size (MB)', 'Lines')
  table <- table[-1,]
  rownames(table) <- NULL
  table
  saveRDS(table, file="table.RDS")
  readRDS(file="table.RDS")

# Sample Data
  
  set.seed(1234)
  size <- 100000   # Only 100 000 entries for each database
  
  blogs <- sample(blogs, size)
  news <- sample(news, size)
  twitter <- sample(twitter, size)
  
  # To guarantee the reproducibility of this study, the results obtained in the first execution are saved.
  # In the rest of executions, this code does not execute and, next, the original saved results will be loaded.
  
  writeLines(blogs, "sample_blogs.txt")
  writeLines(twitter, "sample_twitter.txt")
  writeLines(news, "sample_news.txt")
  rm(list=ls(all=TRUE))
  
  
  size_sample <- 50000 #50 000
  sample <- sample(c(blogs, news, twitter), size_sample)
  rm(list=c("blogs", "news", "twitter"))
  
  saveRDS(sample, file="sample.RDS")
  rm(sample)
  
