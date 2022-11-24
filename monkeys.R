library(quanteda)
library(topicmodels)
library(tidyverse)
library(rtweet)
library(reshape2)
library(ggplot2)

Data.science <- search_tweets(
  q = "monkey", # search for Tweets with "data" AND "science",
  n = 2000 
)
# lets have a look at the dataframe
data = Data.science %>% 
  select(full_text) %>% 
  mutate(doc_id=seq(n())) %>% 
  data.frame()

corpus_sotu_orig <- corpus(data, 
                           docid_field = "doc_id",
                           text_field = "full_text")

corpus_sotu_proc <- tokens(corpus_sotu_orig, 
                           remove_punct = TRUE, # remove punctuation
                           remove_numbers = TRUE, # remove numbers
                           remove_symbols = TRUE) %>% # remove symbols (for social media data, could remove everything except letters)
  tokens_tolower() # remove symbols (for social media data, could remove everything except letters)


lemmaData <- read.csv2("baseform_en.tsv", # downloaded from https://github.com/tm4ss/tm4ss.github.io/tree/master/resources
                       sep="\t", 
                       header=FALSE, 
                       encoding = "UTF-8", 
                       stringsAsFactors = F)

lemmaData = lemmaData %>% 
  filter(!is.na(V1))

corpus_sotu_proc <-  tokens_replace(corpus_sotu_proc, # "Substitute token types based on vectorized one-to-one matching"
                                    lemmaData$V1, 
                                    lemmaData$V2,
                                    valuetype = "fixed") 

corpus_sotu_proc <- corpus_sotu_proc %>%
  tokens_remove(stopwords("english")) %>%
  tokens_ngrams(1) 

DTM <- dfm(corpus_sotu_proc)

# Minimum
minimumFrequency <- 10
DTM <- dfm_trim(DTM, 
                min_docfreq = minimumFrequency,
                max_docfreq = 100)

# keep only letters... brute force
DTM  <- dfm_select(DTM, 
                   pattern = "[a-z]", 
                   valuetype = "regex", 
                   selection = 'keep')
colnames(DTM) <- stringi::stri_replace_all_regex(colnames(DTM), 
                                                 "[^_a-z]","")

DTM <- dfm_compress(DTM, "features")

# We have several rows which do not have any content left. Drop them.

sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- data[sel_idx, ]

#afjust k dependind on the lda model
K <- 20

topicModel <- LDA(DTM, 
                  K, 
                  method="Gibbs", 
                  control=list(iter = 500, 
                               verbose = 25))
tmResult <- posterior(topicModel)
beta <- tmResult$terms
glimpse(beta)   
theta <- tmResult$topics
glimpse(theta)  
terms(topicModel, 10)
top5termsPerTopic <- terms(topicModel, 
                           5)
# For the next steps, we want to give the topics more descriptive names than just numbers. Therefore, we simply concatenate the five most likely terms of each topic to a string that represents a pseudo-name for each topic.
topicNames <- apply(top5termsPerTopic, 
                    2, 
                    paste, 
                    collapse=" ")
topicProportions <- colSums(theta) / nrow(DTM)  # average probability over all paragraphs
names(topicProportions) <- topicNames     # Topic Names
sort(topicProportions, decreasing = TRUE)
attr(topicModel, "alpha") 
topicModel2 <- LDA(DTM, 
                   K, 
                   method="Gibbs", 
                   control=list(iter = 500, 
                                verbose = 25, 
                                alpha = 0.2))#replace alpha

tmResult <- posterior(topicModel2)
theta <- tmResult$topics
beta <- tmResult$terms


topicProportions <- colSums(theta) / nrow(DTM)  # average probability over all paragraphs
names(topicProportions) <- topicNames     # Topic Names 
sort(topicProportions, decreasing = TRUE) 
topicNames <- apply(terms(topicModel2, 5), 2, paste, collapse = " ")
exampleIds <- c(2, 100, 200)
N <- length(exampleIds)

topicProportionExamples <- as.tibble(theta) %>%
  slice(exampleIds)

colnames(topicProportionExamples) <- topicNames

vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), 
                           document = factor(1:N)), 
                     variable.name = "topic", 
                     id.vars = "document")  

ggplot(data = vizDataFrame, 
       aes(topic, value, 
           fill = document), 
       ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, 
             ncol = N)
