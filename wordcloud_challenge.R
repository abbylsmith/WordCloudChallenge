###WORDCLOUDZZZ


##PART 1: parsing the data file (or URL etc.)

library(XML)

# assign input:
#option 1: text file
#option 2: url

input.example<-'0068412.txt'
input.example2 <- 'http://www.cnn.com/2014/04/16/world/asia/south-korea-sinking-ship-students/index.html?hpt=hp_t1'


parse_File <- function(input, ...) {
  #load packages
  require(RCurl)
  require(XML)
  
  evaluate_input <- function(input) {    
    # if input is a file
    if(file.exists(input)) {
      char.vec <- readLines(input, warn = FALSE)
      return(paste(char.vec, collapse = ""))
    }
    
    # if input is just plain text
    if(grepl("</html>", input, fixed = TRUE)) return(input)
    
    # if input is a URL
    if(!grepl(" ", input)) {
      # download html
      html <- getURL(input, followlocation = TRUE)
      # parse html
      doc = htmlParse(html, asText=TRUE)
      plain.text <- xpathSApply(doc, "//p", xmlValue)
      return(cat(paste(plain.text, collapse = "\n")))
    }
    
    # return NULL if input is not one of above 
    return(NULL)
  }
  
  # convert HTML to plain text
  html_to_text <- function(html) {
    html<-lapply(input.example1,)
    text <- htmlParse(html, asText = TRUE)
    return(text)
  }
  
  # text vector into one character string
  text_to_string <- function(txt) {
    return(paste(txt, collapse = " "))
  }
  
  # determine what the file being inputting
  html.list <- lapply(input, evaluate_input)
  
  # extract text from files (if inported as list)
  text.list <- lapply(html.list, html_to_text)
  
  # text->string
  text.vector <- sapply(text.list, text_to_string)
  return(text.vector)
}


# evaluate input and convert to text
txt <- parse_File(input.example)
txt


##PART 2: Getting distinct words

library(stringr)
tokenize <- function(text.input){
  text.input<-gsub("[^[:alnum:][:space:]']", "", text.input) #removing punctation, numbers, etc. expect apostrephes
  strsplit(text.input, split= "\\s+")[[1]] #input=string, will return a list. We will split using                                           '\\s+', which allows for multiple spaces
}

to_make_unique<-tokenize(txt)

library(tm) #tm_map is from this package "Transformations on Corpora"

uniquify <- function(tokenize.output){
  unique_txt<-unique(tokenize.output)
}

output.unique<-uniquify(to_make_unique)


filter_stopwords<-function(text.vector){
  stopwords<-stopwords("english")  #gets dictionary of common English words
  lowercase<-tolower(text.vector) #lowercase so it matches
  removeWords(lowercase, stopwords) #filtering stopwords
}

filtered<-filter_stopwords(output.unique)

length(summary(filtered,maxsum=50000))

##PART 3: WEIGHTING WORDS


weight_by_counts <- function(text){
  #takes in the orginal text
  sum<-c()
  text<-input.example ##TODO take out
  #text<-parse_File(text)
  #tokenize, uniqueify, and then filter the stopwords
  vector.words<- filter_stopwords(uniquify(tokenize(text)))
  for (i in 1:length(vector.words)){
    sum<-c(sum, sum(str_count(text, i)))
  }
}

weighted.words<-weight_by_counts(input.example)



###determining a bonding box. Text=words

bounding_box <-function(text, weights, scale=0.4, font=1, random.order=TRUE, dimensions = unit(c(1, 1), "npc")){
  
  width= convertWidth(dimensions[1], "cm", TRUE)
  height= convertHeight(dimensions[1], "cm", TRUE)
  
  df<- data.frame(words, weights, font, stringsAsFactors = FALSE)
  
  #order the words based on weights
  ord <- ord(-df$weights)
  df <- df[ord, ]
  
  #randomize order
  if(random.order==TRUE){
    df <-df[sample(1:nrow(df),)]
  }
  
  #calculating word sizes
  sizes= list()
  for(i in 1:nrow(df)){
    sizes[[i]] <- textGrob(df$words[i,], gp = gpar(fontface= df$font[i])) #textGrob creates and draws text
  }
  
  #creating of box based on word sizes
  w <- unit(rep(1, nrow=(df)), "grobwidth", sizes)
  df$Width <- convertWidth(w, "cm", TRUE)/ width 
  h <- unit(rep(1, nrow=(df)), "grobheight", sizes)
  df$Heigth <- convertHeight(h, "cm", TRUE)/ width
  
  asc.desc <- grep1("g|j|p|q|y|b|", df$words)
  
  
  
}