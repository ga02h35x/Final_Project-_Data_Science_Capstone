require(tm)

clean_corpus <- function(corpus, remove_stop_words = T, remove_dirty_obscene_words = T, do_steam = T) {
  
  # https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/
  put_space <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
  
  corpus <- tm_map(corpus, put_space, "-")
  corpus <- tm_map(corpus, put_space, ":")
  corpus <- tm_map(corpus, put_space, "`")
  corpus <- tm_map(corpus, put_space, "´")
  corpus <- tm_map(corpus, put_space, " -")
  corpus <- tm_map(corpus, put_space, "[\x82\x91\x92]")   # ""
  corpus <- tm_map(corpus, put_space, '(ftp|http|https)[^([:blank:]|\\"|<|&|#\n\r)]+')  #URL
  corpus <- tm_map(corpus, put_space, '(@|#)[^\\s]+')   # @ and # 
  corpus <- tm_map(corpus, put_space, '^[[:alnum:].-_]+@[[:alnum:].-]+$')   #composed noums
  
  corpus <- tm_map(corpus, stripWhitespace)
  
  no_ASCII <- function(x) iconv(x, "latin1", "ASCII", sub="")
  corpus <- tm_map(corpus, content_transformer(no_ASCII))
  
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(removeNumbers))
  corpus <- tm_map(corpus, content_transformer(removePunctuation))
  
  if (remove_stop_words) {
    print("Remove stop words")
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
  }
  
  if (remove_dirty_obscene_words) {
    print("Remove dirty and obscene words")
    
    require(RCurl)
    x <- getURL("https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en")
    y <- read.csv(text = x)  
    y <- as.character(y[,1])
    y <- y[-length(y)]        # Last entry is a symbol. Not text
    
    corpus <- tm_map(corpus, removeWords, y)
  }
  
  if (do_steam) {
    print("Steam text")    # https://cran.r-project.org/web/packages/corpus/vignettes/stemmer.html
    corpus <- tm_map(corpus, stemDocument, language="english")
  }
  
  corpus <- tm_map(corpus, stripWhitespace)
  
  return(corpus)
}

create_corpus <- function(text_corpus, do_clean = T, remove_stop_words = T, remove_dirty_obscene_words = T, do_steam = F) {
  
  require(tm)
  
  text_corpus <- iconv(text_corpus, "latin1", "ASCII", sub="")
  corpus <- VCorpus( VectorSource( text_corpus ) )   #Optimize futher computational cost. 
  
  if(do_clean) {
    corpus <- clean_corpus(corpus, remove_stop_words, remove_dirty_obscene_words, do_steam)
  }

  return(corpus)
}

corpus_2_dataframe <- function(corpus){

  require(tm)
  data <- data.frame( text=unlist( sapply(corpus,`[`, "content") ), stringsAsFactors = FALSE)
  
  return(data)
}

generate_N_gram_df <- function(text_corpus, N) {
  
  require(tm)
  require(RWeka)
  
  # Taken from package texmineR on https://github.com/TommyJones/textmineR/blob/master/extra_functions/NgramTokenizer.R
  create_tokens <- function(x) NGramTokenizer(x, RWeka::Weka_control(min = N, max = N)) 

  # I cant use control tokenize from tm because I need tokenize by N groups. 
  # https://www.rdocumentation.org/packages/tm/versions/0.7-5/topics/termFreq
  
  # Control sets from https://www.rdocumentation.org/packages/tm/versions/0.7-5/topics/TermDocumentMatrix
  # Generate N-grams
  #n_gram <- TermDocumentMatrix(text_corpus, control = list(tokenize = create_tokens)) 
  # Transform N-grams into format dataframe, what are tractable by ggplot
  # gram_df <- data.frame( Words = n_gram$dimnames$Terms, Freq = n_gram$v )

  n_gram_df <- data.frame( table( create_tokens(text_corpus) ) )
  
  # Reorder N-gram dataframe
  n_gram_df <- n_gram_df[order(n_gram_df$Freq, decreasing = T),]
  names(n_gram_df) <- c("Words", "Freq")
  
  # Make words as character
  n_gram_df$Words <- as.character(n_gram_df$Words)  
  
  # Select only significant appearances
  
  if(N>1){
    n_gram_df<-n_gram_df[n_gram_df$Freq > 1,]
  }
  
  return(n_gram_df)
}

freq_average <- function(sentence)  {
  
  require(NLP)
  require(tm)
  require(magrittr)

  sentence <- sentence %>% 
    removeWords(stopwords('en')) %>% 
    removeNumbers() %>% 
    tolower() %>% 
    removePunctuation(preserve_intra_word_dashes = TRUE) %>% 
    stripWhitespace() %>% 
    strsplit(split=" ") %>%
    extract2(1)
  
  freq_total <- 0
  
  load(file="gram.Rda")
  
  for (i in 1:4) {
    
    input_words <- sentence %>% 
      tail(n=i) %>%
      paste(collapse = " ")
    
    freq <- gram[[i]] %>%
      extract(gram[[i]]$Words==input_words,) %>%
      use_series("Freq") 
    
    
    if( length(freq)==0 ){
      freq <- 0
    }
    
    freq_total <- freq_total + i/10 * freq     # 1+2+3+4 = 10
    
    #cat(sprintf("with %d words the total frequency is %.2f\n",i, freq_total))
  }
  
  return(freq_total)
}

adapt_grams <- function(){
  require(stringr)
  
  load("gram.Rda")
  
  if(length(gram)>1){
    for (i in 2:length(gram)){

      gram[[i]][,3] <- sub(gram[[i]]$Words, pattern = " [[:alpha:]]*$", replacement = "")
      colnames(gram[[i]])[3] <- "Partial"
      
      gram[[i]][,4] <- word(gram[[i]]$Words, -1)
      colnames(gram[[i]])[4] <- "Latest"
      
    }
  }
  
  save(gram, file="gram_adapt.Rda")
  
}


next_word <- function(sentence){
  
  require(NLP)
  require(tm)
  require(magrittr)
  require(data.table)
  
  sentence <- sentence %>% 
    #removeWords(stopwords('en')) %>% 
    removeNumbers() %>% 
    tolower() %>% 
    removePunctuation(preserve_intra_word_dashes = TRUE) %>% 
    stripWhitespace()  %>% 
    strsplit(split=" ") %>%
    extract2(1)
  
  
  load(file="gram_adapt.Rda")
  next_words <-c()
  freq_next_words <-c()
  
  for (i in 1:min(3,length(sentence),length(gram)) ) {
    
    input_words <- sentence %>% 
      tail(n=i) %>%
      paste(collapse = " ")
    
    prediction <- which(gram[[i+1]]$Partial == input_words ) %>%
      sapply(FUN=function(x){sapply(gram[[i+1]], '[[', x)})  %>%
      # t() %>%
      as.data.table() 
    
    if(length(prediction)>0){
      prediction <- prediction[,1:min(10, dim(prediction)[2], length(prediction)) ]
      next_words <- c(next_words, as.character(prediction[4]))
      freq_next_words <- c(freq_next_words, i/9 * as.integer(prediction[2]))   #2+3+4=9
    }
    
  }
  

  require(data.table)
  
  if(length(next_words)>0){
  
    temp <- data.table(next_words, freq_next_words)
    temp <- temp[, sum(freq_next_words), by=next_words]
  
    temp <- as.data.frame(head(temp,4))
  }
  else{
    temp <- data.frame(next_words=c("it", "the", "this"), V1=c(1,1,1))
  }

  return(temp)

}


