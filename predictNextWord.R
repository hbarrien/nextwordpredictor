# predictNextWord.R
#
# CREATION DATE
# 2016-09-22
#
# AUTHOR
# Herbert Barrientos
#
# DESCRIPTION
# Program that predicts the "next word" for a given input string, which may consist of a single word 
# or a phrase. The program first checks input validity. Next, the program determines what n-gram it 
# should first use to start performing the prediction process. If the input string is one word, the 
# program will use bigrams; two words, trigrams; three words, quadgrams; four word, pentagrams; five 
# or more words, sextagrams.
#
# Once the n-gram to begin with has been determined, the program will load the corresponding data file.
# A search is conducted to find matches for the input string. For every match, the last word is extracted 
# and used to predict its probability using the Chain Rule of Conditional Probability approach.
#
# The program is organized as a finite state machine, which processes the input based on its current state. 
# For instance, if the input string consists of seven words, an initial state will convert that input to a 
# five-word string by removing the first two words and keeping the last five words. The resulting five-word 
# input string is processed using sextagrams. If no match is found, a "truncating state" will then remove 
# the first word from the input, thus converting it to a four-word string. At this point, the program backs 
# off and another state will begin another search process using pentagrams. Again, if no match is found, the 
# "truncating state" will again remove the first word from the input and converts it to a three-word string. 
# The program backs off again and another state will begin a search process using quadgrams. If no matches 
# are found, the process repeats for trigrams and finally bigrams. If matches are found at any given state, 
# the words with the highest probabilities are returned. If the "bigram state" is reached and no matches are 
# retrieved, the program returns an empty string.
#
# The program has to consider time and resource constraints, as one assumption is that it should execute on 
# a mobile device --which has less hardware and processing capabilities than a desktop or server computer. 
# To address this problem, the process operates in one of two memory usage modes: a) full dataset in RAM, or 
# b) load data on demand and discard when done. Mode a) is lazy in that it only loads the n-gram data file 
# that it currently needs (e.g., if the input string consists of three words, then pentagram and sextagram 
# data files will not be loaded to memory). However, once loaded, the data file remains in memory. This tactic 
# makes searches very fast, but uses a large amount of memory. On the other hand, mode b) will load the 
# needed n-gram file, use it and then it discards it. With this approach memory usage is kept to a minimum,
# but at the expense of processing time, due to the costly disk I/O operations. 
#
# It is worth mentioning that, regardless of the mode used, the program will randomly select an n-gram subset 
# with a predefined number of terms to work with. The reason for this action is that, even if successfully 
# loaded in memory, the number of n-grams may reach the millions, and the current search method used (grep) 
# tends to drastically slow down. Nevertheless, the adavantage to this approach is that, processing the same
# input string several times will yield different results each time.
#
# RETURNS
#   NULL       - If an error occurred.
#   "BAD DATA" - If the input string is invalid.
#   One or more possible "next words", up to a maximum of NUM_SELECTED_WORDS.
#
# REVISION HISTORY
#


# #####################
# ##### LIBRARIES #####
# #####################
library(data.table)


# #####################
# ##### CONSTANTS #####
# #####################

# Debug modes used for printing tracking information to the console
DEBUG_STATE_MODE <- FALSE
DEBUG_INPUT_MODE <- FALSE
DEBUG_NGRAM_MODE <- FALSE

# Constants used for input/output 
BAD_INPUT    <- "BAD INPUT"
BLANK_SPACE  <- " "
EMPTY_STRING <- ""

# Regular expressions used in n-gram searches
CHAR_FILTER    <- "[^a-zA-Z0-9';:!?,\\. ]"
ALLOWED_CHARS  <- "[^a-zA-Z0-9' ]"
NUMBER_AS_WORD <- "\\s*(?<!\\B|-)\\d+(?!\\B|-)\\s*"

# Input/Output constraint values
REPEAT_WHITESPACE <- "\\s+"
WORD_DELIMITER    <- BLANK_SPACE

# Maximum number of words to start working with, taken from the user input
MAX_INPUT_LENGTH   <- 5

# n-gram sample size, after data has been read into memory
NGRAM_SAMPLE_SIZE  <- 800000

# Maximun number of predicted output words
NUM_SELECTED_WORDS <- 5

# When a search retrieves many results, a sample of size PROC_NGRAM_SAMPLE_SIZE 
# is taken before performing prediction calculations
PROC_NGRAM_SAMPLE_SIZE <- 150

# Determines if data should continue to reside in memory after read (TRUE), or
# if data should be read, used, and then discarded (FALSE)
FULL_DATASET_IN_RAM <- FALSE

# Probability calculation method: single term frequency or paired term frequency
SINGLE_FREQ <- 1
PAIRED_FREQ <- 2
PROB_CALC_METHOD <- SINGLE_FREQ

# n-gram index values
BIGRAM    <- 1
TRIGRAM   <- 2
QUADGRAM  <- 3
PENTAGRAM <- 4
SEXTAGRAM <- MAX_INPUT_LENGTH
DEPLOY    <- TRUE

# File paths and names to be used in development or production environments
if (DEPLOY) {
  
  BIGRAM_FNAME    <- paste0(getwd(), "/bigram.txt")
  TRIGRAM_FNAME   <- paste0(getwd(), "/trigram.txt")
  QUADGRAM_FNAME  <- paste0(getwd(), "/quadgram.txt")
  PENTAGRAM_FNAME <- paste0(getwd(), "/pentagram.txt")
  SEXTAGRAM_FNAME <- paste0(getwd(), "/sextagram.txt")
  
  TERM_FREQ_VECTOR_FNAME <- paste0(getwd(), "/term_freq_vector.txt")
  TERM_FREQ_NAMES_FNAME  <- paste0(getwd(), "/term_freq_names.txt")
  
} else {

  BIGRAM_FNAME    <- paste0(getwd(), "/capstone/wordpredictor/bigram.txt")
  TRIGRAM_FNAME   <- paste0(getwd(), "/capstone/wordpredictor/trigram.txt")
  QUADGRAM_FNAME  <- paste0(getwd(), "/capstone/wordpredictor/quadgram.txt")
  PENTAGRAM_FNAME <- paste0(getwd(), "/capstone/wordpredictor/pentagram.txt")
  SEXTAGRAM_FNAME <- paste0(getwd(), "/capstone/wordpredictor/sextagram.txt")
  
  TERM_FREQ_VECTOR_FNAME <- paste0(getwd(), "/capstone/wordpredictor/term_freq_vector.txt")
  TERM_FREQ_NAMES_FNAME  <- paste0(getwd(), "/capstone/wordpredictor/term_freq_names.txt")
    
}  # END if

# #############################
# ##### N-GRAM MODEL DATA #####
# #############################

# These variables contain the data loaded when FULL_DATASET_IN_RAM == TRUE. This means data 
# is read only once for each n-gram
origBigrams    <- NULL
origTrigrams   <- NULL
origQuadgrams  <- NULL
origPentagrams <- NULL
origSextagrams <- NULL

# Data loaded when FULL_DATASET_IN_RAM == FALSE. These variables are also used to hold
# the samples obtained from the orig* variables (i.e., when FULL_DATASET_IN_RAM == TRUE)
bigrams    <- NULL
trigrams   <- NULL
quadgrams  <- NULL
pentagrams <- NULL
sextagrams <- NULL

term.frequency.vector <- NULL
original.input <- NULL

# ##################################
# ##### N-GRAM MODEL FUNCTIONS #####
# ##################################

# The term frequency vector contains individual words and their frequencies in the Corpus
loadTermFrequencyVector <- function()
{
  if (DEBUG_NGRAM_MODE) print("loadTermFrequencyVector")
  
  term.frequency.vector <<- fread(TERM_FREQ_VECTOR_FNAME, col.names = c("term"), header = FALSE)
  term.frequency.vector <<- as.character(term.frequency.vector[[1]])
  
  if (is.null(term.frequency.vector) || (length(term.frequency.vector) == 0))
    return(NULL)
  
  term.frequency.names <- fread(TERM_FREQ_NAMES_FNAME, col.names = c("term"), header = FALSE)
  term.frequency.names <- as.character(term.frequency.names[[1]])
  
  if (is.null(term.frequency.names) || (length(term.frequency.names) == 0))
    return(NULL)
  
  if (length(term.frequency.vector) != length(term.frequency.names))
    return(NULL)
  
  names(term.frequency.vector) <<- term.frequency.names
  
  return(term.frequency.vector)
  
} # END loadTermFrequencyVector
  
loadNGram <- function(nGram)
{
  if (DEBUG_NGRAM_MODE) print("loadNGram")
  
  fname <- switch(nGram, BIGRAM_FNAME, TRIGRAM_FNAME, QUADGRAM_FNAME, PENTAGRAM_FNAME, SEXTAGRAM_FNAME)
  
  nGramData <- fread(fname, col.names = c("nGram"))
  nGramData <- as.character(nGramData[[1]])
  
  if (is.null(nGramData) || (length(nGramData) == 0)) return(NULL)
  
  return(nGramData)
  
}  # END loadNGram

loadBigram <- function()
{
  if (DEBUG_NGRAM_MODE) print("loadBigram")
  
  # Load data according to the preset memory usage mode
  if (FULL_DATASET_IN_RAM) {
    
    # Lazy load: read data only once
    if (is.null(origBigrams))
      origBigrams <<- loadNGram(BIGRAM)
    
    # Obtain a samle of bigrams of size NGRAM_SAMPLE_SIZE
    if (!is.null(origBigrams))
      bigrams <<- sample(origBigrams, size = NGRAM_SAMPLE_SIZE)
      
  } else {
    
    # Load data on demand
    bigrams <<- loadNGram(BIGRAM)
    
    # Obtain a samle of bigrams of size NGRAM_SAMPLE_SIZE
    if (!is.null(bigrams))
      bigrams <<- sample(bigrams, size = NGRAM_SAMPLE_SIZE)
      
  }  # END if
  
  return(bigrams)
  
}  # END loadBigram

loadTrigram <- function()
{
  if (DEBUG_NGRAM_MODE) print("loadTrigram")
  
  # Load data according to the preset memory usage mode
  if (FULL_DATASET_IN_RAM) {
    
    # Lazy load: read data only once
    if (is.null(origTrigrams))
      origTrigrams <<- loadNGram(TRIGRAM)
      
    # Obtain a samle of trigrams of size NGRAM_SAMPLE_SIZE
    if (!is.null(origTrigrams))
      trigrams <<- sample(origTrigrams, size = NGRAM_SAMPLE_SIZE)
      
  } else {
      
    # Load data on demand
    trigrams <<- loadNGram(TRIGRAM)
      
    # Obtain a samle of trigrams of size NGRAM_SAMPLE_SIZE
    if (!is.null(trigrams))
      trigrams <<- sample(trigrams, size = NGRAM_SAMPLE_SIZE)
      
  }  # END if
  
  return(trigrams)
  
}  # END loadTrigram

loadQuadgram <- function()
{
  if (DEBUG_NGRAM_MODE) print("loadQuadgram")
  
  # Load data according to the preset memory usage mode
  if (FULL_DATASET_IN_RAM) {
    
    # Lazy load: read data only once
    if (is.null(origQuadgrams))
      origQuadgrams <<- loadNGram(QUADGRAM)
      
    # Obtain a samle of quadgrams of size NGRAM_SAMPLE_SIZE
    if (!is.null(origQuadgrams))
      quadgrams <<- sample(origQuadgrams, size = NGRAM_SAMPLE_SIZE)
      
  } else {
      
    # Load data on demand
    quadgrams <<- loadNGram(QUADGRAM)
      
    # Obtain a samle of quadgrams of size NGRAM_SAMPLE_SIZE
    if (!is.null(quadgrams))
      quadgrams <<- sample(quadgrams, size = NGRAM_SAMPLE_SIZE)
      
  }  # END if
  
  return(quadgrams)
  
}  # END loadQuadgram

loadPentagram <- function()
{
  if (DEBUG_NGRAM_MODE) print("loadPentagram")
  
  # Load data according to the preset memory usage mode
  if (FULL_DATASET_IN_RAM) {
    
    # Lazy load: read data only once
    if (is.null(origPentagrams))
      origPentagrams <<- loadNGram(PENTAGRAM)
    
    # Obtain a samle of pentagrams of size NGRAM_SAMPLE_SIZE
    if (!is.null(origPentagrams))
      pentagrams <<- sample(origPentagrams, size = NGRAM_SAMPLE_SIZE)
    
  } else {
    
    # Load data on demand
    pentagrams <<- loadNGram(PENTAGRAM)
    
    # Obtain a samle of pentagrams of size NGRAM_SAMPLE_SIZE
    if (!is.null(pentagrams))
      pentagrams <<- sample(pentagrams, size = NGRAM_SAMPLE_SIZE)
    
  }  # END if
  
  return(pentagrams)
  
}  # END loadPentagram

loadSextagram <- function()
{
  if (DEBUG_NGRAM_MODE) print("loadSextagram")
  
  # Load data according to the preset memory usage mode
  if (FULL_DATASET_IN_RAM) {
    
    # Lazy load: read data only once
    if (is.null(origSextagrams))
      origSextagrams <<- loadNGram(SEXTAGRAM)
    
    # Obtain a samle of sextagrams of size NGRAM_SAMPLE_SIZE
    if (!is.null(origSextagrams))
      sextagrams <<- sample(origSextagrams, size = NGRAM_SAMPLE_SIZE)
    
  } else {
    
    # Load data on demand
    sextagrams <<- loadNGram(SEXTAGRAM)
    
    # Obtain a samle of sextagrams of size NGRAM_SAMPLE_SIZE
    if (!is.null(sextagrams))
      sextagrams <<- sample(sextagrams, size = NGRAM_SAMPLE_SIZE)
    
  }  # END if
  
  return(sextagrams)
  
}  # END loadSextagram

releaseNGram <- function(nGram)
{
  if (DEBUG_NGRAM_MODE) print("releaseNGram")
  
  # Remove the n-gram, indicated by argument nGram, from memory
  switch(nGram, bigrams <<- NULL, trigrams <<- NULL, quadgrams <<- NULL, pentagrams <<- NULL, sextagrams <<- NULL)
  
  # Remove the "read-once" n-gram kept in memory, as indicated by argument nGram. This action is taken
  # to ensure that no nGram remains in memory, thus bloating it
  if (!FULL_DATASET_IN_RAM)
    switch(nGram, origBigrams <<- NULL, origTrigrams <<- NULL, origQuadgrams <<- NULL, origPentagrams <<- NULL, origSextagrams <<- NULL)
  
}  # END releaseNGram

releaseBigram <- function()
{
  if (DEBUG_NGRAM_MODE) print("releaseBigram")
  
  releaseNGram(BIGRAM)
  
}  # END releaseBigram

releaseTrigram <- function()
{
  if (DEBUG_NGRAM_MODE) print("releaseTrigram")
  
  releaseNGram(TRIGRAM)
  
}  # END releaseTrigram

releaseQuadgram <- function()
{
  if (DEBUG_NGRAM_MODE) print("releaseQuadgram")
  
  releaseNGram(QUADGRAM)
  
}  # END releaseQuadgram

releasePentagram <- function()
{
  if (DEBUG_NGRAM_MODE) print("releasePentagram")
  
  releaseNGram(PENTAGRAM)
  
}  # END releasePentagram

releaseSextagram <- function()
{
  if (DEBUG_NGRAM_MODE) print("releaseSextagram")
  
  releaseNGram(SEXTAGRAM)
  
}  # END releaseSextagram

# ###############################
# ##### AUXILIARY FUNCTIONS #####
# ###############################
splitString <- function(s)
{
  return(as.vector(strsplit(s, WORD_DELIMITER)))
  
}  # END splitString


splitStringLength <- function(s)
{
  return(length(s[[1]]))
  
}  # END splitStringLength


reduceStringLength <- function(s, desiredLength)
{
  s1    <- splitString(s)
  s1Len <- splitStringLength(s1)
  
  if (s1Len <= desiredLength) return(s)
  
  s2 <- EMPTY_STRING
  
  idx <- (s1Len - desiredLength) + 1
  for (i in idx:s1Len)
    s2 <- paste(s2, s1[[1]][i])
  
  return(trimws(s2, which = c("both")))
  
}  # END reduceStringLength


# ######################################
# ##### INPUT PROCESSING FUNCTIONS #####
# ######################################
checkInput <- function(s)
{
  if (DEBUG_INPUT_MODE) print("checkInput")
  
  # Return BAD_INPUT: if argument s is NULL or invalid, not a textual object, or the empty string
  if (is.null(s) || is.na(s) || !is.character(s) || (s == EMPTY_STRING)) return(BAD_INPUT)

  # Return BAD_INPUT: if argument s contains chars not included in the character filter
  if (length(grep(CHAR_FILTER, s, perl = TRUE)) > 0) return(BAD_INPUT)  
  
  # Remove all non [alphanumeric, single quote, and blankspace] characters before continuing
  s1 <- gsub(ALLOWED_CHARS, BLANK_SPACE, s, perl = TRUE)
  
  # Remove duplicated whitespace and replace it with a blank space
  s1 <- trimws(s1, which = c("both"))
  s1 <- gsub(REPEAT_WHITESPACE, BLANK_SPACE, s1, perl = TRUE)
  
  # Return BAD_INPUT: if the resulting string was reduced to a blank space or the empty string
  if ((s1 == BLANK_SPACE) || (s1 == EMPTY_STRING)) return(BAD_INPUT)
  
  # Return BAD_INPUT: if cleaned argument s contains numbers as words
  if (length(grep(NUMBER_AS_WORD, s1, perl = TRUE)) > 0) return(BAD_INPUT)
  
  return(s)
  
}  # END checkInput

formatInput <- function(s)
{
  if (DEBUG_INPUT_MODE) print("formatInput")
  
  # Remove all non [alphanumeric, single quote, and blankspace] characters
  s1 <- gsub(ALLOWED_CHARS, BLANK_SPACE, s, perl = TRUE)
  
  # Remove unnecessary whitespace and replace it with the word delimiter
  s1 <- trimws(s1, which = c("both"))
  s1 <- gsub(REPEAT_WHITESPACE, WORD_DELIMITER, s1, perl = TRUE)
  
  # Convert to lower case
  s1 <- tolower(s1)
  
  return(s1)
  
}  # END formatInput

setOriginalInput <- function(s)
{
  original.input <<- s
  
}  # END setOriginalInput

getOriginalInput <- function()
{
  return(original.input)
  
}  # END getOriginalInput


# ##########################################
# ##### FINITE STATE MACHINE FUNCTIONS #####
# ##########################################
prepareInput <- function(s)
{
  if (DEBUG_STATE_MODE) print("STATE: prepareInput")
  
  # Validate input string
  s1 <- checkInput(s)
  
  if (s1 == BAD_INPUT) 
    return(BAD_INPUT)
  
  # Format input string for adequate n-gram processing
  s1 <- formatInput(s1)
  
  # Handle potentially long input string
  s1 <- reduceStringLength(s1, ifelse((MAX_INPUT_LENGTH < 1), 1, MAX_INPUT_LENGTH))
  
  # Markov assumption: short history. Save original input sub-string used for processing
  setOriginalInput(s1)
  
  # Proceed to the next state
  return(initialize(s1))
  
}  # END prepareInput

initialize <- function(s)
{
  nGram <- NULL
  
  if (DEBUG_STATE_MODE) print("STATE: initialize")
  
  s1 <- splitString(s)
  inputLen <- splitStringLength(s1)
  
  if ((inputLen < 1) || (inputLen > MAX_INPUT_LENGTH))
    return(NULL)
  
  if (is.null(term.frequency.vector)) {
    
    if (is.null(loadTermFrequencyVector()))
        return(NULL)
    
  }  # END if
  
  # Free up memory
  releaseBigram(); releaseTrigram(); releaseQuadgram(); releasePentagram(); releaseSextagram();
  
  switch(inputLen, nGram <- loadBigram(), nGram <- loadTrigram(), nGram <- loadQuadgram(), nGram <- loadPentagram(), nGram <- loadSextagram())
  
  if (is.null(nGram)) return(NULL)
  
  # Proceed to the next state
  return(findNextWord(s))
  
}  # END initialize

findNextWord <- function(s)
{
  if (DEBUG_STATE_MODE) print("STATE: findNextWord")
  
  s1 <- splitString(s)
  inputLen <- splitStringLength(s1)
  
  # Proceed to the next state
  if (inputLen == BIGRAM)    return(handleBigrams(s))
  if (inputLen == TRIGRAM)   return(handleTrigrams(s))
  if (inputLen == QUADGRAM)  return(handleQuadgrams(s))
  if (inputLen == PENTAGRAM) return(handlePentagrams(s))
  if (inputLen == SEXTAGRAM) return(handleSextagrams(s))
  
  return(NULL)
  
}  # END findNextWord

handleBigrams <- function(s)
{
  if (DEBUG_STATE_MODE) print("STATE: handleBigrams")
  
  selectedBigrams <- searchNGrams(s)
  
  if (is.null(selectedBigrams)) return(NULL)
  if (length(selectedBigrams) == 0) return(EMPTY_STRING)
  
  # Predict the "next words"
  nextWord <- selectWord(selectedBigrams)
  if (length(nextWord) == 0) return(EMPTY_STRING)
  
  # Return output
  return(nextWord[1:NUM_SELECTED_WORDS])
  
}  # END handleBigrams

handleTrigrams <- function(s)
{
  if (DEBUG_STATE_MODE) print("STATE: handleTrigrams")
  
  selectedTrigrams <- searchNGrams(s)
  
  if (is.null(selectedTrigrams)) return(NULL)
  
  # If no trigrams were found, back off
  if (length(selectedTrigrams) == 0) {
    
    s1 <- backoff(s)
    return(initialize(s1))
    
  }  # END if
  
  # Predict the "next words"
  nextWord <- selectWord(selectedTrigrams)
  if (length(nextWord) == 0) return(EMPTY_STRING)
  
  # Return output
  return(nextWord[1:NUM_SELECTED_WORDS])
  
}  # END handleTrigrams

handleQuadgrams <- function(s)
{
  if (DEBUG_STATE_MODE) print("STATE: handleQuadgrams")
  
  selectedQuadgrams <- searchNGrams(s)
  
  # If no quadgrams were found, back off
  if (is.null(selectedQuadgrams)) return(NULL)
  
  if (length(selectedQuadgrams) == 0) {
    
    s1 <- backoff(s)
    return(initialize(s1))
    
  }  # END if
  
  # Predict the "next words"
  nextWord <- selectWord(selectedQuadgrams)
  if (length(nextWord) == 0) return(EMPTY_STRING)
  
  # Return output
  return(nextWord[1:NUM_SELECTED_WORDS])
  
}  # END handleQuadgrams

handlePentagrams <- function(s)
{
  if (DEBUG_STATE_MODE) print("STATE: handlePentagrams")
  
  selectedPentagrams <- searchNGrams(s)
  
  if (is.null(selectedPentagrams)) return(NULL)
  
  # If no pentagrams were found, back off
  if (length(selectedPentagrams) == 0) {
    
    s1 <- backoff(s)
    return(initialize(s1))
    
  }  # END if
  
  # Predict the "next words"
  nextWord <- selectWord(selectedPentagrams)
  if (length(nextWord) == 0) return(EMPTY_STRING)
  
  # Return output
  return(nextWord[1:NUM_SELECTED_WORDS])
  
}  # END handlePentagrams

handleSextagrams <- function(s)
{
  if (DEBUG_STATE_MODE) print("STATE: handleSextagrams")
  
  selectedSextagrams <- searchNGrams(s)
  
  if (is.null(selectedSextagrams)) return(NULL)
  
  # If no sextagrams were found, back off
  if (length(selectedSextagrams) == 0) {
    
    s1 <- backoff(s)
    return(initialize(s1))
    
  }  # END if
  
  # Predict the "next words"
  nextWord <- selectWord(selectedSextagrams)
  if (length(nextWord) == 0) return(EMPTY_STRING)
  
  # Return output
  return(nextWord[1:NUM_SELECTED_WORDS])
  
}  # END handleSextagrams

searchNGrams <- function(s)
{
  nGrams <- NULL
  
  if (DEBUG_STATE_MODE) print("STATE: searchNGrams")
  
  s1 <- splitString(s)
  inputLen <- splitStringLength(s1)
  
  switch(inputLen, nGrams <- bigrams, nGrams <- trigrams, nGrams <- quadgrams, nGrams <- pentagrams, nGrams <- sextagrams)
  
  if (is.null(nGrams)) return(NULL)
  
  searchTerm <- paste0("^", s, " .")
  selectedNGrams <- nGrams[grep(searchTerm, nGrams)]
  
  if (DEBUG_STATE_MODE) print(selectedNGrams)
  
  return(selectedNGrams)
  
}  # END searchNGrams

selectWord <- function(nGrams)
{
  if (DEBUG_STATE_MODE) print("STATE: selectWord")
  
  # calculate "next word" probabilities using the predefined method
  switch(PROB_CALC_METHOD, probs <- calcProbIndividualFreq(nGrams), probs <- calcProbPairedFreq(nGrams))
  
  return(probs)
  
}  # END selectWord

calcProbIndividualFreq <- function(nGrams)
{
  if (DEBUG_STATE_MODE) print("calcProbIndividualFreq")
  
  if (length(nGrams) > PROC_NGRAM_SAMPLE_SIZE)
    nGrams <- sample(nGrams, size = PROC_NGRAM_SAMPLE_SIZE)
  
  words <- vector()
  probs <- vector()
  
  for (i in 1:length(nGrams)) {
    
    nGram <- splitString(nGrams[i])
    
    s1    <- splitString(paste0(getOriginalInput(), BLANK_SPACE, nGram[[1]][length(nGram[[1]])]))
    s1Len <- inputLen <- splitStringLength(s1)
    
    p        <- 1
    freq     <- 0
    currWord <- EMPTY_STRING
    
    for( j in s1Len:1) {
      
      currWord <- s1[[1]][j]
      
      # If current word not found in frequency vector, smooth it
      if (is.na(term.frequency.vector[currWord])) freq <- 1 
      else freq <- as.numeric(term.frequency.vector[[currWord]])
      
      p <- (p * (freq / NGRAM_SAMPLE_SIZE))
      
    }  # END for
    
    words <- c(words, s1[[1]][s1Len])
    probs <- c(probs, p)
    
  }  # END for
  
  names(probs) <- words
  probs <- sort(probs, decreasing = TRUE)
  
  return(probs)
  
}  # END calcProbIndividualFreq

calcProbPairedFreq <- function(nGrams)
{
  if (DEBUG_STATE_MODE) print("calcProbPairedFreq")
  
  if (length(nGrams) > PROC_NGRAM_SAMPLE_SIZE)
    nGrams <- sample(nGrams, size = PROC_NGRAM_SAMPLE_SIZE)
  
  words <- vector()
  probs <- vector()
  
  for (i in 1:length(nGrams)) {
    
    nGram <- splitString(nGrams[i])
    
    s1    <- splitString(paste0(getOriginalInput(), BLANK_SPACE, nGram[[1]][length(nGram[[1]])]))
    s1Len <- inputLen <- splitStringLength(s1)
    
    p        <- 1
    freq     <- 0
    freqPrev <- 0
    
    currWord <- EMPTY_STRING
    prevWord <- EMPTY_STRING
    
    for( j in s1Len:1) {
      
      currWord <- s1[[1]][j]
      
      # If current word not found in frequency vector, smooth it
      if (is.na(term.frequency.vector[currWord])) freq <- 1 
      else freq <- as.numeric(term.frequency.vector[[currWord]])
      
      if (j > 1) {
        
        prevWord <- s1[[1]][j-1]
        
        # If previous word not found in frequency vector, smooth it
        if (is.na(term.frequency.vector[prevWord])) freqPrev <- 1 
        else freqPrev <- as.numeric(term.frequency.vector[[prevWord]])
        
      } # END if
      
      p <- (p * ((freq + freqPrev) / NGRAM_SAMPLE_SIZE))
      
    }  # END for
    
    words <- c(words, s1[[1]][s1Len])
    probs <- c(probs, p)
    
  }  # END for
  
  names(probs) <- words
  probs <- sort(probs, decreasing = TRUE)
  
  return(probs)
  
}  # END calcProbPairedFreq

backoff <- function(s)
{
  if (DEBUG_STATE_MODE) print("STATE: backoff")
  
  s1 <- splitString(s)
  sLength <- splitStringLength(s1)
  
  if (DEBUG_STATE_MODE) str(s1)
  
  s2 <- EMPTY_STRING
  for (i in 2:sLength)
    s2 <- paste0(s2, s1[[1]][i], BLANK_SPACE)
  
  return(trimws(s2, which = c("both")))
  
}  # END backoff


# #######################################
# ##### MAIN STATE MACHINE FUNCTION #####
# #######################################
predictNextWord <- function(s)
{
  if (DEBUG_STATE_MODE) print("STATE: predictNextWord")
  
  # Start the process by calling the first state
  return(prepareInput(s))
  
}  # END predictNextWord