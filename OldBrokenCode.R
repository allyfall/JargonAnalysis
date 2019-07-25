##to read in the corpus of abstracts. gives bag of words, but only works for a single abstract document. 
library(tm) ## the tibble method only works for one pdf. So don't run this, it is broken. 
env_text <- pdf_text(files)
env_tibble <- tibble(text = env_text)
env_tidy <- env_tibble %>%
  unnest_tokens(word, text)


##Trying to read in standard corpus in txt files:
library(readtext)
#std_corp <- readtext("wlp_fiction_awq/*")
test_corp <- readtext("Test/*",
                      docvarsfrom = "filenames",
                      docvarnames = c("col1", "col2", "col3"),
                      dvsep="")
#this (below) doesn't work 
#test_corp2 <- read.table("Test/wlp_fic_2012.txt", sep = "\t", header = TRUE)
#OK this broke my computer last time, so don't run it.
test_tibble <- tibble(text = test_corp)
test_tidy <- test_tibble %>%
  unnest_tokens(word,text)
#alright, there is some weird stuff going on with the third
#column in the txt files.
#can I read it in delimited by tabs? And then delete the third col?


##Trying the blog corpus which is xml
library(XML)
library(methods)
library(xml2)

##From here to indication below does not work.
list.files(pattern=".xml$")
# create a list from these files
list.filenames<-list.files(pattern=".xml$")
df_list <- lapply(list.filenames, function(f) {
  doc <- read_xml(f)
  setNames(data.frame(
    xml_attr(xml_find_all(doc, "//ns2:opendataField"), "key"),
    xml_attr(xml_find_all(doc, "//ns2:opendataField"), "value")
  ), c("key", f))
  
})
blog_corpus <- xmlParse(file = "Test/7596.male.26.Internet.Scorpio.xml")
blog_dataframe <- xmlToDataFrame("Test/7596.male.26.Internet.Scorpio.xml")
blog_list <- xmlToList(blog_corpus)
blog_tibble <- tibble(blog_dataframe)
blog_char <- as.character(blog_tibble)
blog_tidy <- blog_dataframe %>%
  unnest_tokens(word, text, format = "xml")

#another method?
posts <- xpathApply(blog_corpus, "//post",xmlValue)
postWords <- lapply(posts,strsplit,"[[:space:]]")
#postWords <- tibble(postWords)