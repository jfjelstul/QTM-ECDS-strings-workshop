##########################################################################
# Josh Fjelstul
# Emory University
# QTM-ECDS Fall 2018 Workshops, Coding in R Series
# Intermediate R: Strings
##########################################################################

# review: data structures in R
# unit 1: reading in text data
# unit 2: cleaning and manipulating text data
# unit 3: using regular expressions to make a dataset
# unit 4: preparing data for text analysis

##########################################################################
# setup
##########################################################################

# install packages
# install.packages("stringr")
# install.packages("rvest")
# install.packages("XML")
# install.packages("rvest")
# install.packages("dplyr")
# install.packages("tm")
# install.packages("quanteda")

# load packages
library(stringr)
library(rvest)
library(XML)
library(rvest)
library(dplyr)
library(tm)
library(quanteda)

# set working directory
setwd("~/Desktop/") # Mac
setwd("C:/Desktop/") # PC
# or use the menu: Session > Set Working Directory > Choose Directory...
#set the working directory to the root folder for this workshop

# clean workspace
rm(list = ls())
# or use the button in the environment panel

# clear console
cat("\014")
# or use the button in the console panel

##########################################################################
##########################################################################
# review: data structures in R
##########################################################################
##########################################################################

# assign values to objects
x <- 9
x
x <- "text"
x

# objects have classes
x <- 9
class(x)
x <- "text"
class(x)

# vectors
vector <- c(1, 2, 3)
vector <- c("element1", "element2", "element3")

# indexing 
vector[1]
vector[1:2]
index <- 1:2
vector[index]

# length of a vector
length(vector)

# length of unique elements
vector <- c("element1", "element1", "element2")
length(unique(vector))

# lists
list1 <- list(vector1 = c("element1", "element2", "element3"), 
              vector2 = c("element4", "element5", "element6"), 
              vector3 = c("element7", "element8"))

# viewing information in lists
list1
list1[1]
list1[[1]]
list1$vector1

# flaten a list
x <- unlist(list1)
names(x)
names(x) <- NULL

# data frames
# a data frame is really a structured list
var1 <- c(1, 2, 3)
data <- data.frame(var1 = var1, var2 = c(4, 5, 6))

# call a variable
data$var1

# change a variable
data$var1 <- c(7, 8, 9)
data$var1 <- c("element1", "element2", "element3")

# check class
class(data)
class(data$var1)
class(data$var2)

##########################################################################
##########################################################################
# unit 1: reading text data
##########################################################################
##########################################################################

# clean workspace
rm(list = ls())

# clear console
cat("\014")

##########################################################################
# 1.1: CSV files
##########################################################################

# read in data
data <- read.csv("data/inauguration.csv")

# see variable names
names(data)

# check class
class(data$event)
# this is a factor
# a factor is a variable that takes a finite number of discrete values

# store text as strings
data <- read.csv("data/inauguration.csv", stringsAsFactors = FALSE)

# check class again
class(data$event)
# now it's a string

##########################################################################
# 1.2: TXT files
##########################################################################

# load a .txt file
text <- readLines("text/harry-potter.txt")
# don't worry about this warning

# make a data frame
data <- data.frame(id = 1:length(text), text = text, stringsAsFactors = FALSE)

##########################################################################
# 1.3: HTML using the "XML" package
##########################################################################

# URL address
url <- "https://en.wikipedia.org/wiki/Harry_Potter"

# new file name
file <- "harry-potter-temp.html"

# download HTML page to a folder
download.file(url = url, destfile = file)

# read HTML into R from website
html <- readLines(url)

# read HTML into R from file
file <- "text/harry-potter.html"
html <- readLines(file)

# preview contents
head(html) # that's no good

# we can use tools from the XML package to extract the text we're interested in from the HTML code

# the first step is to parse the HTML code
text <- htmlParse(html)

# then we can use HTML tags to extract what we're interested in
# check the code to see what the right tags are
text <- xpathSApply(text, "//p", xmlValue)

# make a data frame
data <- data.frame(text = text)

##########################################################################
# 1.4: HTML using the "rvest" package
##########################################################################

# read HTML into R
html <- read_html("text/harry-potter.html")

# extract text in paragraph tags
text <- html %>% html_nodes("p") %>% html_text()

# make a data frame
data <- data.frame(text = text)

##########################################################################
# 1.5: HTML tables
##########################################################################

# URL address
url <- "https://en.wikipedia.org/wiki/United_States_presidential_inauguration"

# download file to working directory
table <- readLines(url)

# collapse vector of HTML code to a single string
table <- str_c(table, collapse = " ")

# extract tables
table <- readHTMLTable(table)

# get a summary of the object
summary(table)

# we want the 4th element in the list
table <- table[[4]]

# view variable names
names(table)

# replace variable names
names(table) <- c("number", "date", "event", "location", "oath", "length")

# remove the first row
table <- table[-1,]

# save as a csv file
write.csv(table, "inauguration-temp.csv", row.names = FALSE, fileEncoding = "UTF-8")

##########################################################################
##########################################################################
# unit 2: cleaning and manipulating text data
##########################################################################
##########################################################################

# clean workspace
rm(list = ls())

# clear console
cat("\014")

##########################################################################
# 2.1: the stringr package
##########################################################################

# R has built-in functions but these are better
# stringr creates wrapers for built-in functions
# note that all functions are vectorized

# sample text
# read HTML into R
full.text <- read_html("text/harry-potter.html") %>% html_nodes("p") %>% html_text()
text <- full.text[3]

# trim white space around string
text <- str_trim(text)

# replace single occurence
str_replace(text, "Hogwarts School of Witchcraft and Wizardry", "Hogwarts")

# replace multiple occurences
str_replace_all(text, "Harry Potter", "HARRY POTTER")

# detect expression
str_detect(text, "wizard")

# make a dummy variable
as.numeric(str_detect(text, "wizard"))

# count the number of occurences
str_count(text, "wizard")

##########################################################################
# 2.2: regular expressions
##########################################################################

# a syntax to match patterns in text

################################################
# 2.2.1: metacharacters
################################################

# spaces
# \n line break
# \t tab
# \r carriage return (rare)

# ? * + . ^ $ | \ ( ) [ ] { }
# if you want the character, escape the metacharacter using \
# in R, you have to escape again 
# to match a period: \\.

# ? match the preceding item at most once
# * match the preceding item zero or more times
# + match the preceding item one or more times
# . match any single character
# ^ matches the empty character at the beginning of a string
# $ matches the empty character at the end of a string
# | means or

# move to a new paragraph
text <- str_trim(full.text[4])

# using ^
# match the first character in the string
str_replace(text, "^.", "")

# using $
# match the last character in the string
str_replace(text, ".$", "")

# using \\
# extract the word vote followed by a period
str_extract_all(text, "worldwide")[[1]] # this isn't what we want
str_extract_all(text, "worldwide\\.")[[1]]
# note that str_extract_all returns a list

# using \\
# remove a left parenthesis
str_replace_all(text, "[", "") # this doesn't work
str_replace_all(text, "\\]", "") # this does

# remove left and right brackets
str_replace_all(text, "\\[|\\]", "")

# remove brackets and contents
str_replace_all(text, "\\[.+\\]", "")
str_replace_all(text, "\\[.*?\\]", "")

# this expression is especially useful
# .*?x matches any character any number of times until x occurs
str_extract(text, "Harry.*?Stone")

################################################
# 2.2.2: sub-expressions
################################################

# use parentheses to create sub-expressions
# use \\# to copy expressions
# if the regular expression is "example (text)", "\\1" gives you "text"

# example
str_replace(text, "(copies) worldwide", "\\1")

################################################
# 2.2.3: character classes
################################################

# character classes
# a set of characters to match
# list characters inside brackets
# [abc] the lower letters a, b, and c
# [^abc] any characters but the lower letters a, b, and c
# [A-Z] all capital letters 
# [A-Za-z] all letters 
# [0-9] numbers
# note that only ^ - \ are special inside a character class
# if you want - as a character, put it first or last

# extract years
str_extract_all(text, "[0-9][0-9][0-9][0-9]") # we'll see a better way in a second

# remove all characters that aren't letters or spaces
str_replace_all(text, "[^A-Za-z ]", "")

# remove capitalized words
str_replace_all(text, "[A-Z][a-z]+", "")

################################################
# 2.2.4: predefined character classes
################################################

# note that these must go inside another set of brackets
# [:alnum:] alphanumeric characters
# same as [0-9A-Za-z] but also includes diacritics
# [:alpha:] alphabetic characters
# same as [A-Za-z] but also includes diacritics
# [:blank:] blank characters
# [:digit:] numbers
# same as [0-9]
# [:lower:] lower case letters
# [:upper:] upper case letters
# [:print:] printable characters and space
# [:punct:] punctuation characters 
# ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
# [:space:] space, tab, new line, carriage return, etc.

# the nice thing about [:alnum:] and [:alpha:] is that 
# they'll match characters with diacritic marks

# remove punctuation
str_replace_all(text, "[[:punct:]]", " ") 

# remove numbers
str_replace_all(text, "[[:digit:]]", " ") # leaves a lot of white space 
str_replace_all(text, "[[:space:]]*[[:digit:]]+[[:space:]]*", "") # better 

################################################
# 2.2.5: repetition quantifiers
################################################

# use metacharacters to address repetition
# {n} match the preceding item exactly n times
# {n,} match the preceding item n or more times
# {n,m} match the preceding item between n and m times

# extract years
str_extract_all(text, "[0-9]+")[[1]] # not what we want
str_extract_all(text, "[0-9]{4}")[[1]] # use custom character set
str_extract_all(text, "[[:digit:]]{4}")[[1]] # use predefined character set

##########################################################################
# 2.3: example: using regular expressions to get cleaned text
##########################################################################

# duplicate text
clean <- text

# remove numbers
clean <- str_replace_all(clean, "[0-9]+", " ")

# remove punctuation
clean <- str_replace_all(clean, "[-.,\\[\\]]", " ")

# clean spaces
clean <- str_replace_all(clean, " +", " ")

# trim string
clean <- str_trim(clean)

# lower case
clean <- tolower(clean)

##########################################################################
# 2.4: example: using predefined character classes to get cleaned text
##########################################################################

# duplicate text
clean <- text

# remove numbers
clean <- str_replace_all(clean, "[[:digit:]]", " ")

# remove punctuation
clean <- str_replace_all(clean, "[[:punct:]]", " ")

# clean spaces
clean <- str_replace_all(clean, "[[:space:]]+", " ")

# trim string
clean <- str_trim(clean)

# lower case
clean <- tolower(clean)

##########################################################################
# 2.5: example: using regular expressions to extract words
##########################################################################

# make a list of words
list_words <- function(x) {
  
  # collapse vector of paragraphs into a single string
  words <- str_c(x, collapse = " ")
  
  # remove punctuation
  words <- str_replace_all(words, "[[:punct:]]", " ")
  
  # remove numbers
  words <- str_replace_all(words, "[[:digit:]]", " ")
  
  # remove space
  words <- str_replace_all(words, "[[:space:]]+", " ")
  
  # convert to lower case
  words <- tolower(words)
  
  # split string into a vector of words
  words <- str_split(words, " ")[[1]]
  
  # remove empty strings
  words <- words[words != ""]
  
  # return a vector of words
  return(words)
}

# run function
words <- list_words(text)

# view results
head(words, 15)

# number of words
length(words)

# number of unique words
length(unique(words))

##########################################################################
# 2.6: example: using regular expressions to clean a dataset
##########################################################################

# read in data
data <- read.csv("data/inauguration.csv", stringsAsFactors = FALSE)

# 1. number of the inauguration (regular only)
# 2. day
# 3. month
# 4. year
# 5. day of the week
# 6. whether it was regular or extraordinary
# 7. name of the president
# 8. name of the person administering the oath
# 9. length of the speech (word count)

# number
data$number <- str_extract(data$number, "[0-9]+")
data$number <- as.numeric(data$number)
data$number[is.na(data$number)] <- 0

# month 
data$day <- str_extract(data$date, "[0-9]+")
data$day <- as.numeric(data$day)

# day
data$month <- str_extract(data$date, "[A-Z][a-z]+")

# year
data$year <- str_extract(data$date, "[0-9]{4}")
data$year <- as.numeric(data$year)

# weekday
data$weekday <- str_extract(data$date, "\\(.*?\\)")
data$weekday <- str_replace_all(data$weekday, "\\(|\\)", "")

# extraordinary
data$extraordinary <- as.numeric(str_detect(data$event, "Extraordinary"))

# president
data$event <- str_replace(data$event, "\\(.*?\\)", "")
data$president <- str_extract(data$event, "[A-Z][a-z]+$")

# oath
data$oath <- str_replace(data$oath, "([a-z])([A-Z])", "\\1, \\2")
data$oath <- str_replace(data$oath, "([a-z])([A-Z])", "\\1, \\2")
data$oath <- str_replace(data$oath, ",.*", "")
data$oath <- str_extract(data$oath, "[A-Z][a-z]+$")

# length
data$length <- str_extract(data$length, "[0-9]+")
data$length <- as.numeric(data$length)

# choose variables
data <- select(data, number, weekday, month, day, year, president, oath, extraordinary, length)

##########################################################################
##########################################################################
# unit 3: using regular expressions to make a dataset
##########################################################################
##########################################################################

# clean workspace
rm(list = ls())

# clear console
cat("\014")

# read in data
dat <- read.csv("data/judges-example.csv", stringsAsFactors = FALSE)
names <- read.csv("data/judge-names.csv", stringsAsFactors = FALSE)

# we want to make the following variables
# 1. day of document
# 2. month of document
# 3. year of document
# 4. year of case
# 5. case number (which we can get two ways)
# 6. whether the case was heard in English
# 7. the name of the rapporteur (the judge that writes the opinion)
# 8. a clean list of the judges in the chamber
# 9. the number of judges in the chamber
# 10. a chamber ID variable

# we can use regular expressions to get all of this data

# this is a good example because the text includes non-standard characters
# this is when predefined character classes are really useful

##########################################################################
# 3.1: extracting numbers from strings
##########################################################################

# day of document
dat$document_day <- str_extract(dat$document_date, "[0-9]{2}$")
dat$document_day <- as.numeric(dat$document_day)

# month of document
dat$document_month <- str_extract(dat$document_date[1], "-[0-9]{2}-")
dat$document_month <- str_replace_all(dat$document_month, "-", "")
dat$document_month <- as.numeric(dat$document_month)

# year of document
dat$document_year <- str_extract(dat$document_date, "^[0-9]{4}")
dat$document_year <- as.numeric(dat$document_year)

# year of case
dat$case_year <- str_extract(dat$CELEX_number, "^6[0-9]{4}")
dat$case_year <- str_extract(dat$case_year, "[0-9]{4}$")
dat$case_year <- as.numeric(dat$case_year)

# case number

# option 1
dat$number <- str_extract(dat$CELEX_number, "[0-9]{4}$")
dat$number <- as.numeric(dat$number)

# option 2
dat$number <- str_extract(dat$case_number, "[0-9]+")
dat$number <- as.numeric(dat$number)

##########################################################################
# 3.2: dummy variables from text data
##########################################################################

# whether the case was heard in English

# option 1: coercing a logical statement to an integer
dat$english <- as.numeric(dat$language_text == "English")

# option 2: by detection
dat$english <- str_detect(dat$language_text, "English")
dat$english <- as.numeric(dat$english)

##########################################################################
# 3.3: constructing string variables (by concatenation)
##########################################################################

# what if we wanted to reconstruct the CELEX number?
dat$CELEX_number_2 <- str_c(6, dat$case_year, "TJ", str_pad(dat$number, width = 4, side = "left", pad = "0"), sep = "")

# check
sum(dat$CELEX_number == dat$CELEX_number) == nrow(dat)

# what if we wanted to reconstruct the case number? 
dat$case_number_2 <- str_c("T-", dat$number, "/", str_extract(dat$case_year, "[0-9]{2}$"), sep = "")

# check
sum(dat$case_number == dat$case_number_2) == nrow(dat)

##########################################################################
# 3.4: cleaning a string variable (by reduction)
##########################################################################

# the name of the rapporteur
# first we need to make sure all of the white space is clean (always do this first)
dat$judges_text <- str_replace_all(dat$judges_text, "[[:space:]]+", " ")
dat$rapporteur <- str_extract(dat$judges_text, "\\.[[:alpha:] ]+ \\(rapporteur\\)")
dat$rapporteur <- str_replace(dat$rapporteur, ".rapporteur.", "") # or "\\(rapporteur\\)"
dat$rapporteur <- str_replace(dat$rapporteur, "\\.", "") # or "^."
dat$rapporteur <- str_trim(dat$rapporteur)

# a clean list of the judges in the chamber
dat$judges <- dat$judges_text
dat$judges <- str_replace(dat$judges, ", juges", "")
dat$judges <- str_replace(dat$judges, "composé de ", "")
dat$judges <- str_replace(dat$judges, "président,", "")
dat$judges <- str_replace(dat$judges, "\\(rapporteur\\)", "")
dat$judges <- str_replace_all(dat$judges, "[A-Z]\\.", "")
dat$judges <- str_replace_all(dat$judges, "Mme|M ", "")
dat$judges <- str_replace_all(dat$judges, " et ", ", ")
dat$judges <- str_trim(dat$judges)
dat$judges <- str_replace_all(dat$judges, " *, *", ", ")

# now we need to put them in alphabetical order
# that's so we can tell if a chamber is unique or not

# make an example string
x <- "van der Woude, Wiszniewska-Białecka, Ulloa Rubio"

# write a function
clean <- function(x) {
  
  # split the string by commas
  # this returns a list
  x <- str_split(x, ", ")
  
  # turn the list into a vector
  x <- unlist(x)
  
  # or just do this: x <- str_split(x)[[1]]
  
  # put the vector in alphabetical order
  x <- x[order(x)]
  
  # collapse the vector into a single string
  
  x <- str_c(x, collapse = ", ")
  
  # return the string
  return(x)
}

# apply the function to the list
for(i in 1:nrow(dat)) {
  dat$judges[i] <- clean(dat$judges[i])
}

##########################################################################
# 3.5: counting items in a string list
##########################################################################

# the number of judges in the chamber
dat$size_chamber <- str_count(dat$judges, ",") + 1

##########################################################################
# 3.6: creating an ID variable based on a categorial string variable
##########################################################################

# the number of judges in the chamber
dat$chamber_ID <- as.numeric(as.factor(dat$judges))

##########################################################################
# 3.7: creating a string list by detection (instead of reduction)
##########################################################################

# here's another way to get the list of judge names
# this only works if we have a list of the names ahead of time

# make a new empty variable
dat$judges_2 <- NA

# make a vector of judge names
names <- names$name

# for each row in our data frame...
for(i in 1:nrow(dat)) {
  
  # make a temporary vector to store the judge names
  temp <- NULL
  
  # for each name...
  for(j in 1:length(names)) {
    
    # check to see if the name occurs in the raw text
    if(str_detect(dat$judges_text[i], names[j])) {
      
      # if it does, add it to our vector
      temp <- c(temp, names[j])
    }
  }
  
  # order the judge names
  temp <- temp[order(temp)]
  
  # collapse the names into a single string
  temp <- str_c(temp, collapse = ", ")
  
  # store the string in the correct row in our data frame
  dat$judges_2[i] <- temp
}

# check
sum(dat$judges == dat$judges_2) == nrow(dat)

##########################################################################
##########################################################################
# unit 4: preparing data for text analysis
##########################################################################
##########################################################################

# clean workspace
rm(list = ls())

# clear console
cat("\014")

# the goal is to make a document term matrix (DTM)
# a DTM is the input for most text analysis functions

##########################################################################
# 4.1: prepare some test data
##########################################################################

# let's make some data to analyze
text <- read_html("text/harry-potter.html") %>% html_nodes("p") %>% html_text()

# make a data frame
data <- data.frame(text = text)

# drop paragraphs with no text
data$text <- str_trim(data$text)
data <- filter(data, text != "")

# length of each document
data$length <- nchar(data$text)

##########################################################################
# 4.2: the "tm" package
##########################################################################

# make a text corpus
corpus <- Corpus(VectorSource(data$text))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus <- tm_map(corpus, stripWhitespace)
# don't worry about these warnings

# make a document term matrix
# rows are documents
# columns are words
# cells are word frequencies 
dtm <- DocumentTermMatrix(corpus)

# make a term document matrix
# rows are words
# columns are documents
# cells are word frequencies
tdm <- TermDocumentMatrix(corpus)

# check dimensions
dim(dtm)
dim(tdm)

# convert to a matrix
dtm_matrix <- as.matrix(dtm)
tdm_matrix <- as.matrix(tdm)

# make a version with only the most common words
# sparsity is the percent of documents a term does NOT appear in
# larger maximum allowed sparsity = more terms retained
dtm_trimmed <- removeSparseTerms(dtm, sparse = 0.95)
# this keeps words that appear in at least 5% of the documents

# check dimensions
dim(dtm_trimmed)

##########################################################################
# 4.3: the "quanteda" package
##########################################################################

# make a text corpus
corpus <- corpus(data$text, docvars = data)

# select texts
corpus <- corpus_subset(corpus, length > 500)

# extract the original texts from the corpus
texts <- texts(corpus)
texts[1]

# tokenize texts
tokens <- tokens(corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, split_hyphens = TRUE)

# remove stop words
tokens <- tokens_remove(tokens, stopwords("english"))

# stem tokens
tokens <- tokens_wordstem(tokens, "english")

# make a data term matrix
dtm <- dfm(tokens)

# check dimensions
dim(dtm)

# keep words that occur in at least 5 documents
dtm_trimmed <- dfm_trim(dtm, min_docfreq = 5)
dim(dtm_trimmed)

# keep words that occur in at least 5% of documents
dtm_trimmed <- dfm_trim(dtm, min_docfreq = 0.05)
dim(dtm_trimmed)

# keep words that occur at least twice
dtm_trimmed <- dfm_trim(dtm, min_termfreq = 2)
dim(dtm_trimmed)

##########################################################################
# end R script
##########################################################################
