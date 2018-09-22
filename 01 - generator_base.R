setwd("/home/garcia/Escritorio/coursera/10/5")
options(java.parameters="-Xmx15g")

# Import functions
  source("** - functions.R")


# Create one clean corpus using sample data
  sample <- readRDS(file="sample.RDS")
  
  corpus <- create_corpus(sample, do_clean = T, remove_stop_words = F, remove_dirty_obscene_words = T, do_steam = F)
  rm(sample)
  saveRDS(corpus, file="corpus.RDS")
  
  corpus_df <- corpus_2_dataframe(corpus)
  rm(corpus)
  
  saveRDS(corpus_df, file="corpus_df.RDS")
  rm(corpus_df)


# N-grams
  corpus <- readRDS(file="corpus_df.RDS")
  
  library(foreach)
  library(doParallel)
  no_cores <- detectCores() - 1
  registerDoParallel(no_cores)
  cl <- makeCluster(no_cores)
  
  gram<-list()
  
  gram <- foreach(i=1:4) %dopar% {
    result <- generate_N_gram_df(corpus, i)
    return(result)
  }
  
  rm(corpus)
  corpus_dfsave(gram, file="gram.Rda")
  rm(gram)
 
  adapt_grams()
  

  
  
  
