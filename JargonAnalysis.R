##Jargon analysis project Summer 2019##
#Building the corpora: 
#English general terms and scraping science papers
library(pdftools)
library(tidyverse)
library(tidytext)
library(tm) 
setwd("~/Desktop/JargonAnalysis-master/JargonAnalysis/Test/PDF")
files <- list.files(pattern = "pdf$")
env_papers <- lapply(files, pdf_text)
length(env_papers)
#Now I have a termdocmatrix, which I'm a little less sure how to use. 
env_test_tib <- tibble(text = env_papers)
corp <- Corpus(URISource(files), readerControl = list(reader = readPDF))
corp <- tm_map(corp, removePunctuation, ucp = TRUE)
env_dtm <- DocumentTermMatrix(corp,
                              control=
                                list(stopwords=TRUE,
                                     tolower=TRUE,
                                     stemming = FALSE,
                                     bounds = list(global = c(1,Inf))))
env_tdm <- TermDocumentMatrix(corp,
                              control=
                                list(stopwords=TRUE,
                                     tolower=TRUE,
                                     stemming = FALSE,
                                     bounds = list(global = c(1,Inf))))
inspect(env_tdm)

###Using blog posts to create general english corpus. Directory must be set to the jargon master, not to test. 
##This results in however many xml documents in the folder being read in as a DTM.
setwd("~/Desktop/JargonAnalysis-master/JargonAnalysis")
blogposts <- Corpus(DirSource("Test/XML"))
blogposts <- tm_map(blogposts, removePunctuation)
blogposts <- tm_map(blogposts, content_transformer(tolower))
blogposts <- tm_map(blogposts, removeNumbers)
blogposts <- tm_map(blogposts, stripWhitespace)
blogDTM <- DocumentTermMatrix(blogposts)
#looking at word frequency:
blog_freq <- colSums(as.matrix(blogDTM))
#total number of terms:
length(blog_freq) 
#posttm <- readXML(type = "node", spec = "XPathExpression") 

###Building the Environmental science corpus###
#### Creating a corpus from PLOS articles 
# Link to full download: https://www.plos.org/text-and-data-mining 
# Basically, I used the tm package to create a VCorpus from the XML files 
# that PLOS downloads as. DirSource() imports files from a directory. I created
# a custom reader (readPLOS) to handle the XML structure of the files 
# DIRECTORY IS TOO BIG FOR R - USE SUBSETS (I used 15 files with zero performance issues
# if you want to test with a more representative sample, I'm sure it could handle 50+)

#create custom reader
readPLOS <- readXML(
  spec = list(front = list("node", "front"),
              content = list("node", "body"),
              back = list("node", "back")),
  doc = PlainTextDocument())

#use DirSource to recursively import documents 
#note that the directory works when set to the main project directory, in my case, jargonanalysis-master.
plos <- VCorpus(DirSource("ploscorpus/plostest", mode = "text"), 
                readerControl = list(reader = readPLOS))

View(plos)
plos[["journal.pbio.0000001.xml"]][["content"]]

# tidy - got rid of all metadata except filename (as identifier?) (which is maybe a bad call, but idk where to store it)
#slice out content
plos[[4]][[1]]

ploscontent <- data.frame(sapply(plos,`[`,1)) %>%
  gather(ploscontent) %>%
  unnest_tokens("words", value)

##### Working on some analysis/creating numbers methods ####
#methods used will be: Jargonness, LSA, lex tightness, Flesch-Kincaid, POS analysis
##can also look at word frequency, topic modeling (should do preprosessing for this)

##Jargonness##
##step one: make sure science (plos) and general corpora are bag of words: 
ploswords <- ploscontent$words #56836 words right now #science corp is now bag of words
generalwords <- blogDTM$dimnames$Terms #51360 words right now #blog/general corp is now bag of words!
##step two: turn abstracts into bag of words (one bag per abstract)
#I'll start with only one abstract to test. 
abs_test <- corp[1] #you just need to change this number to change which abstract you are targeting. (I think...)
abs_test <- tm_map(abs_test, removeNumbers)
length(abs_test)
abs1_jargon <- DocumentTermMatrix(abs_test,
                              control=
                                list(stopwords=TRUE,
                                     tolower=TRUE,
                                     stemming = FALSE,
                                     bounds = list(global = c(1,Inf))))
#note: line above MUST be 1 to inf, not 2 because with stopwords removed, you get 0 words with limit set at 2
abs1_words <- abs1_jargon$dimnames$Terms #973 words right now for Breakup.pdf
##step three: remove stopwords from all corpora (done in read in process, stopwords=TRUE)
##step four: create word frequency tables for both corpora
#this is already done for the document term matrix of the blogs. It is stored in blog_freq
plos <- tm_map(plos, removePunctuation)
plos_dtm <- DocumentTermMatrix(plos,
                               control=
                                 list(stopwords=TRUE,
                                      tolower=TRUE,
                                      stemming = FALSE,
                                      bounds = list(global = c(2,Inf))))
plos_words <- plos_dtm$dimnames$Terms
plos_freq <- colSums(as.matrix(plos_dtm))
##step five: create an empty matrix w/ 4 columns and as many rows as words in abstract
jargon_matrix <- matrix(,nrow = length(abs1_words), ncol=4)
##step six: take first word from abstract bag 
#possibility for better code: use word frequency table for abstract as well, so you don't run the same word over and over..

#ok, to do this with a function, lets assume it will be applied to the vector of words with the pipe opperator. 
#so first arg is word? 
#function needs to make all data in row for single word. 
#the problem is that i need to replace row i in the matrix with the word data. How do I do that?
index <- 0 #maybe I can hack it? 
calc_jargonness <- function(word, index){
 #find if word is in general corpus
  gen_row <- which(word==generalwords)
  index <- index + 1
  jargon_matrix[index,1] <- word
  #if it is, get word freq, calc, save to col 2. If not assign 3. 
  if(gen_row >= 1){
    gen_freq <- blog_freq[gen_row]/length(generalwords)
    jargon_matrix[index,2] <- gen_freq
    gen_freq
  } else{
    jargon_matrix[index,4] <- 3
    jargon_matrix[index,2] <- 0
  }
  #now for the science corpora
  sci_row <- which(word==plos_words)
  if(length(sci_row)==0){
    sci_row <- 0
  }
  #if in science, get word freq, save to 3, get jargon score
  #if not, see if it was in general (is there a non-zero score in col 2?)
  #if it was in general, give col 3+4 = 0. If in neither, all 0. 
  if(sci_row >= 1){
    sci_freq <- plos_freq[sci_row]/length(plos_words)
    jargon_matrix[index,3] <- sci_freq
    jaron_score <- log10((gen_freq/sci_freq))
    jargon_matrix[index,4] <- jaron_score
  } else{
    #hmm I'm not sure this is what we want to do.
    #this means if it isn't in general or science, everything is zero.
    #which ranks it the same as if it was just in the general.
    #I think we need to differentiate the two somehow.
    if(jargon_matrix[index,2]==0){
      jargon_matrix[index,3] <- 0
      jargon_matrix[index,4] <- 0
    }
    else{
      jargon_matrix[index,3] <- 0
      jargon_matrix[index,4] <- 0 
    }
  }
}

calc_jargonness(word="american",1)
View(jargon_matrix)
#Thoughts on how to apply this:
#lapply or similar: a function that takes my function and applies it to a row of data
#a loop of some sort
#the pipe operator %>%. But I don't know how/what to start with. Jargonmatrix? Or does it have to have a product? 



##LSA: latent semantic analysis: 
library(lsa)
library(LSAfun)
#creating the matrixes for the analysis
#working directory is set to jargonAnalysis-master.
source_dir <- "Test/XML"
text_dir <- blogposts #it only wants a path, not an object. So I need to find a way to make this a path.
TDM <- textmatrix(source_dir, stopwords = stopwords_en, stemming = TRUE, removeXML = TRUE, removeNumber = T, minGlobFreq=2)
summary.textmatrix(TDM)
# creating weighted matrix TDM2 out of the original TDM. TDM2 is the term frequency times its inverse document frequency
TDM2 <- lw_tf(TDM) * gw_idf(TDM) 
LSAspace <- lsa(TDM2, dims=dimcalc_share())
as.textmatrix(LSAspace)
#lsa function above makes TDM2 into three matrices. tk (term matrix), dk (document matrix), and sk (singular val matrix)
tk2 <- t(LSAspace$sk *t(LSAspace$tk))
#can plot dimensions and terms.
plot(tk2[,1], y=tk2[,2], col="red", cex=.50, main="TK Plot")
text(tk2[,1], y=tk2[,2], labels=rownames(tk2), cex=.70) #This is a plot of words taken from the blogposts

###Hi Katie! The section below is what I've been working on/struggling with###
##need to do this for the abstracts and sciCorp too. 
##The abstracts are in as a TDM now. 
absDTM2 <- TermDocumentMatrix(corp)
TDM2abs2 <- lw_tf(absDTM2) * gw_idf(absDTM2) #doesn't work, same error: Error in Ops.simple_triplet_matrix((m > 0), 1) : Not implemented.
LSAabs <- lsa(absDTM2, dims = dimcalc_share())
as.textmatrix(LSAabs)
tk2 <- t(LSAabs$sk *t(LSAabs$tk))
#can plot dimensions and terms.
plot(tk2[,1], y=tk2[,2], col="red", cex=.50, main="TK Plot")
text(tk2[,1], y=tk2[,2], labels=rownames(tk2), cex=.70) #This gives a plot of document names. So it isn't getting the words out of the documents? 
#Ok, this kind of works, but it isn't what I expected when I plot it. Also, I cannot interpret this at all. So maybe I need to get that 
#other step to work. 

#Trying LSA on the PLOS corpus
setwd("~/Desktop/JargonAnalysis-master/JargonAnalysis")
source_plos <- "ploscorpus/plostest"
TDM_plos <- textmatrix(source_plos, stopwords = stopwords_en, stemming = TRUE, removeXML = TRUE, removeNumber = T, minGlobFreq=2)
summary.textmatrix(TDM_plos)
# creating weighted matrix TDM2 out of the original TDM. TDM2 is the term frequency times its inverse document frequency
TDM2_plos <- lw_tf(TDM_plos) * gw_idf(TDM_plos) 
LSAspace_plos <- lsa(TDM2_plos, dims=dimcalc_share())
as.textmatrix(LSAspace_plos)
#lsa function above makes TDM2 into three matrices. tk (term matrix), dk (document matrix), and sk (singular val matrix)
tk2_plos <- t(LSAspace_plos$sk *t(LSAspace_plos$tk))
#can plot dimensions and terms.
plot(tk2_plos[,1], y=tk2_plos[,2], col="red", cex=.50, main="TK Plot PLOS Corpus")
text(tk2_plos[,1], y=tk2_plos[,2], labels=rownames(tk2_plos), cex=.70)
##Something I need to do a little research on is how to interpret LSA plots. 

##Flesch-Kincaid: 206.835 - 1.015(totalwords/totalsentences) - 84.6(totalsyllables/totalwords)
#I need a corpus of abstracts that is not tidy. It needs punctuation and spaces. 
#there is actually a function: readability(txt.file, hyphen = NULL, index = "Flesch")
library(koRpus)
library(koRpus.lang.en)
#install.koRpus.lang(lang = "en") #Think I have this on machine now, so I don't need to run line again.
#the readability function takes .txt files. I want to look at my abstracts only, so maybe I save them as txt files...?
setwd("~/Desktop/JargonAnalysis-master/JargonAnalysis/Test/TXT")
readability("Ab1.txt", hyphen = NULL, index = "Flesch.Kincaid", tagger = "tokenize", force.lang = "en")
readability("breakup.txt", hyphen = NULL, index = "Flesch.Kincaid", tagger = "tokenize", force.lang = "en")
readability("coral.txt", hyphen = NULL, index = "Flesch.Kincaid", tagger = "tokenize", force.lang = "en")
readability("carbon.txt", hyphen = NULL, index = "Flesch.Kincaid", tagger = "tokenize", force.lang = "en")
readability("entropySpecies.txt", hyphen = NULL, index = "Flesch.Kincaid", tagger = "tokenize", force.lang = "en")
readability("gutSymbiont.txt", hyphen = NULL, index = "Flesch.Kincaid", tagger = "tokenize", force.lang = "en")
readability("particles.txt", hyphen = NULL, index = "Flesch.Kincaid", tagger = "tokenize", force.lang = "en")
readability("snails.txt", hyphen = NULL, index = "Flesch.Kincaid", tagger = "tokenize", force.lang = "en")
readability("sponge.txt", hyphen = NULL, index = "Flesch.Kincaid", tagger = "tokenize", force.lang = "en")
readability("dengue.txt", hyphen = NULL, index = "Flesch.Kincaid", tagger = "tokenize", force.lang = "en")

#Need to convert XML into txt files, preferably automatically. This will take some research.
#for 1,breakup,coral,carbon, entropy, gut, particles, snails, sponge, dengue I get 
#scores of grade 16.26, 19.23, 18.85, 17.62, 15.63, 18.36, 8.17, 14.25, and 18.38, 14.47 respectively.

##Lexical Tightness: how inter-related words are in normal vs science language.
#is a mean of NPMI and is log2(p(a,b)/p(a)p(b))/-log2(p(a,b))
##OR Word Association Profiles


##Some very standard text analysis on the abstracts: word counts, topic modeling
#sentiment analysis, bigrams (if possible)
#start with word counts

