# capstone_DataProcessing.R
#
# CREATION DATE
# 2016-09-01

# AUTHOR
# Herbert Barrientos

# DESCRIPTION
# Process that captures input data from various sources (blogs, news tweets), cleans it, 
# and uses it to create six n-grams (i.e., bigrams, trigrams, quadgrams, pentagrams, and 
# sextagrams) that are saved to disk. This input data is also used to create a word frequency 
# vector that is also saved to disk. 
#
# All saved files will be used by the word-predicting program.
# 
# DATA PROCESSING DECISIONS
# 1. Set the character encoding of all Corpus texts to LATIN1.
#
# 2. The single quote character is represented in various ways, so the first step is to detect 
#    it using the SINGLE_QUOTE_LATIN1_0x patterns and to replace the located character sequences 
#    with a mask. Not performing this action upfront will result in losing the single quote 
#    character patterns to other data cleaning processes and tokenizing functions, thus leaving 
#    truncated verbs and other quoted words. The mask is replaced with the SINGLE_QUOTE_STRAIGHT 
#    character once all data cleaning activity has completed.
#
# 3. All texts are converted to lowercase.
#
# 4. All characters not forming part of FILTER_PATTERN_01 are removed. The blank space is 
#    included in the pattern just so that it is ignored.
#
# 5. Upon completion of steps 1 - 4, any remaining punctuation is completely removed.
# 
# 6. FILTER_PATTERN_02: Numbers are removed only when they appear as single words (e.g., 2000). 
#    Numbers embedded within alphabetical characters are left (e.g., r2d2, c3po, u2) as they 
#    might be used in certain communication contexts.
#
# 7. Stop words are not removed as they may be necessary to complete a given phrase.
#
# 8. Profanity words are not removed (for now...).
#
# 9. All single characters not forming part of SINGLE_CHARACTER_PATTERN are removed.
#
# 10. REPEAT_CHARACTER_PATTERN: All repeating character words, such as "zzz", or "aaaaaaaaaa" 
#     are removed.
#
# 11. White space is stripped so that there is only one space between words.
#
# 12. Text samples. The idea is that, given that this project is to build a model (as opposed 
#     to a production application), the purpose is to test the word prediction algorithm with 
#     a subset of the data provided. Thus, text samples of TERM_SAMPLE_SIZE elements were 
#     obtained from the Corpus files.
#
# 13. New Corpus. The clean text sample is saved to disk, and used to create a new Corpus.
#
# 14. Term frequency vector. Created from the newly created Corpus. This way, both this vector 
#     and the n-grams share the same data.
#
# 15. Memory issues. To handle memory issues and prolonged processing time (some runs took 
#     approximately 24 hs. using the entire dataset), the aforementioned text samples were 
#     created. Additionally, data structures are deleted from memory as soon as they are no 
#     longer necessary, and the garbage collection function is executed immediately after.
# 
# HISTORY REVISION
#   2016-09-12 : H. Barrientos
#     a) Included functionality for the creation of pentagrams and sextagrams. The purpose of 
#        this inclusion is to expand the predicition algorithm's knowledge base, so that the
#        algorithm can handle longer sentence fragments of up to five words.
#
#     b) Included functionality for processing the single quote character.
#


# #######################
# ###### LIBRARIES ######
# #######################
library(clue)
library(tm)
library(RWeka)


# #######################
# ###### CONSTANTS ######
# #######################

# Set Corpus document indices
EN_BLOG    <- 1
EN_NEWS    <- 2
EN_TWITTER <- 3

# Set textual patterns
BLANK_SPACE <- " "
TERM_SAMPLE_SIZE <- 40000

FILTER_PATTERN_01 <- "[^a-zA-Z 0-9]"
FILTER_PATTERN_02 <- "\\s*(?<!\\B|-)\\d+(?!\\B|-)\\s*"

SINGLE_CHARACTER_PATTERN <- "^([^aiI] )|( [^aiI] )|( [^aiI])$"
REPEAT_CHARACTER_PATTERN <- " (.)\\1{1,} "

LATIN1                 <- "latin1"
SINGLE_QUOTE_STRAIGHT  <- "'" 
SINGLE_QUOTE_LATIN1_01 <- "(Ã¢â,¬â"¢)"
SINGLE_QUOTE_LATIN1_02 <- "(Ã,â???T)"
SINGLE_QUOTE_LATIN1_03 <- "(['])"
SINGLE_QUOTE_MASK      <- "hbcpecsbexbe"

# #######################
# ###### FUNCTIONS ######
# #######################

# Create the toSpace content transformer
toSpace <- content_transformer( 
  function(x, pattern) { return (gsub(pattern, BLANK_SPACE, x, perl = TRUE)) } )

# Abstract the NGramTokenizer function 
ngramTokenizer <- function(x, n, m) NGramTokenizer(x, Weka_control(min = n, max = m))

# Create interfaces for bigrams and trigrams
bigramTokenizer    <- function(x) ngramTokenizer(x, 2, 2)
trigramTokenizer   <- function(x) ngramTokenizer(x, 3, 3)
quadgramTokenizer  <- function(x) ngramTokenizer(x, 4, 4)
pentagramTokenizer <- function(x) ngramTokenizer(x, 5, 5)
sextagramTokenizer <- function(x) ngramTokenizer(x, 6, 6)


# #######################
# ##### DIRECTORIES #####
# #######################

# Set directory of text files in EN
wd     <- paste0(getwd(), "/capstone")
dir_en <- paste0(wd, "/final/en_US/")

dir_en_processed <- paste0(wd, "/final/en_US_processed/")
word_predictor   <- paste0(wd, "/wordpredictor/")


# #############################
# ##### OUTPUT FILE NAMES #####
# #############################

BIGRAM_FNAME    <- paste0(word_predictor, "bigram.txt")
TRIGRAM_FNAME   <- paste0(word_predictor, "trigram.txt")
QUADGRAM_FNAME  <- paste0(word_predictor, "quadgram.txt")
PENTAGRAM_FNAME <- paste0(word_predictor, "pentagram.txt")
SEXTAGRAM_FNAME <- paste0(word_predictor, "sextagram.txt")

PROCESSED_CORPUS_FNAME <- paste0(dir_en_processed, "processed_corpus.txt")
TERM_FREQ_VECTOR_FNAME <- paste0(dir_en_processed, "term_freq_vector.txt")
TERM_FREQ_NAMES_FNAME  <- paste0(dir_en_processed, "term_freq_names.txt")
TERM_FREQ_ORDERED_VECTOR_FNAME <- paste0(dir_en_processed, "term_freq_ord_vector.txt")


# #######################
# ### CORPUS CREATION ###
# #######################

# Create the Corpus for text files in EN
cname_en <- dir_en
docs_en  <- Corpus(DirSource(cname_en), readerControl = list(reader = readPlain)) 

# Inspect the Corpus
str(docs_en)
summary(docs_en)


# #######################
# #### DATA CLEANING ####
# #######################

# Set the character encoding to LATIN1
Encoding(docs_en[[EN_BLOG]]$content) <- LATIN1
Encoding(docs_en[[EN_NEWS]]$content) <- LATIN1
Encoding(docs_en[[EN_TWITTER]]$content) <- LATIN1

# Replace the various representations of single quote with the defined mask
for (i in EN_BLOG:EN_TWITTER) {
  
  docs_en[[i]]$content <- gsub(SINGLE_QUOTE_LATIN1_01, SINGLE_QUOTE_MASK, docs_en[[i]]$content, perl = TRUE)
  docs_en[[i]]$content <- gsub(SINGLE_QUOTE_LATIN1_02, SINGLE_QUOTE_MASK, docs_en[[i]]$content, perl = TRUE)
  docs_en[[i]]$content <- gsub(SINGLE_QUOTE_LATIN1_03, SINGLE_QUOTE_MASK, docs_en[[i]]$content, perl = TRUE)
  
}  # END for

# Set all Corpus text to lower case
docs_en <- tm_map(docs_en,content_transformer(tolower))
str(docs_en)

# Replace all characters not forming part of FILTER_PATTERN_01 with a blank space
docs_en <- tm_map(docs_en, toSpace, FILTER_PATTERN_01)
str(docs_en)

# Remove any remaining punctuation
docs_en <- tm_map(docs_en, removePunctuation)
str(docs_en)

# Replace all numbers appearing as words (FILTER_PATTERN_02) with a blank space
docs_en <- tm_map(docs_en, toSpace, FILTER_PATTERN_02)
str(docs_en)

# Replace all single character words not forming of SINGLE_CHARACTER_PATTERN with a blank space
docs_en <- tm_map(docs_en, toSpace, SINGLE_CHARACTER_PATTERN)
str(docs_en)

# Replace all repeating character words with a blank space
docs_en <- tm_map(docs_en, toSpace, REPEAT_CHARACTER_PATTERN)
str(docs_en)

# Strip white space
docs_en <- tm_map(docs_en, stripWhitespace)
str(docs_en)


# ###############################
# #### N-GRAM MODEL CREATION ####
# ###############################

# Make the sampling reproducible
set.seed(1234)

# Get a sample from the BLOG text file in the Corpus
t1 <- sample(docs_en[[EN_BLOG]]$content, TERM_SAMPLE_SIZE)

# Get a sample from the NEWS text file in the Corpus
t2 <- sample(docs_en[[EN_NEWS]]$content, TERM_SAMPLE_SIZE)

# Get a sample from the TWITTER text file in the Corpus
t3 <- sample(docs_en[[EN_TWITTER]]$content, TERM_SAMPLE_SIZE)

# Join all samples into one vector and free up memory
tsamples <- c(t1, t2, t3)
rm(t1); rm(t2); rm(t3); gc()

# Create a bigram, remove repetitions, substitute the single quote mask, save it 
# to disk, and free up memory
bigram <- bigramTokenizer(tsamples)
bigram <- unique(bigram)
bigram <- gsub(SINGLE_QUOTE_MASK, SINGLE_QUOTE_STRAIGHT, bigram, perl = TRUE)
write.table(bigram, BIGRAM_FNAME, row.names = FALSE, col.names = FALSE)
rm(bigram); gc()

# Create a trigram, remove repetitions, substitute the single quote mask, save it 
# to disk, and free up memory
trigram <- trigramTokenizer(tsamples)
trigram <- unique(trigram)
trigram <- gsub(SINGLE_QUOTE_MASK, SINGLE_QUOTE_STRAIGHT, trigram, perl = TRUE)
write.table(trigram, TRIGRAM_FNAME, row.names = FALSE, col.names = FALSE)
rm(trigram); gc()

# Create a quadgram, remove repetitions, substitute the single quote mask, save it 
# to disk, and free up memory
quadgram <- quadgramTokenizer(tsamples)
quadgram <- unique(quadgram)
quadgram <- gsub(SINGLE_QUOTE_MASK, SINGLE_QUOTE_STRAIGHT, quadgram, perl = TRUE)
write.table(quadgram, QUADGRAM_FNAME, row.names = FALSE, col.names = FALSE)
rm(quadgram); gc()

# Create a pentagram, remove repetitions, substitute the single quote mask, save it 
# to disk, and free up memory
pentagram <- pentagramTokenizer(tsamples)
pentagram <- unique(pentagram)
pentagram <- gsub(SINGLE_QUOTE_MASK, SINGLE_QUOTE_STRAIGHT, pentagram, perl = TRUE)
write.table(pentagram, PENTAGRAM_FNAME, row.names = FALSE, col.names = FALSE)
rm(pentagram); gc()

# Create a sextagram, remove repetitions, substitute the single quote mask, save it 
# to disk, and free up memory
sextagram <- sextagramTokenizer(tsamples)
sextagram <- unique(sextagram)
sextagram <- gsub(SINGLE_QUOTE_MASK, SINGLE_QUOTE_STRAIGHT, sextagram, perl = TRUE)
write.table(sextagram, SEXTAGRAM_FNAME, row.names = FALSE, col.names = FALSE)
rm(sextagram); gc()

# Save tsamples in order to create a new, refined Corpus
write.table(tsamples, PROCESSED_CORPUS_FNAME, row.names = FALSE, col.names = FALSE, quote = FALSE)

# Free up memory
rm(tsamples); rm(docs_en); gc()


# ########################################
# #### TERM FREQUENCY VECTOR CREATION ####
# ########################################

# Create a new Corpus with clean data for text files in EN
cname_en <- dir_en_processed
docs_en  <- Corpus(DirSource(cname_en), readerControl = list(reader = readPlain)) 

# Remove single quote mask and replace it with the straight single quote
docs_en[[1]]$content <- gsub(SINGLE_QUOTE_MASK, SINGLE_QUOTE_STRAIGHT, docs_en[[1]]$content, perl = TRUE)

# Inspect the Corpus
str(docs_en)
summary(docs_en)

# Create the Document Term Matrix (DTM)
dtm <- DocumentTermMatrix(docs_en)

# Inspect the DTM
dtm

# Free up memory by removing the Corpus. Not needed anymore
rm(docs_en); gc()

# IMPORTANT: Remove the least frequent terms. There are many sparse terms 
# (mostly with frequencies between 1 - 9) that are useless
dtm_withoutSparseTerms <- removeSparseTerms(dtm, 0.34)

# Calculate the cumulative frequencies of words across documents [FREQUENCY VECTOR]
freqr <- colSums(as.matrix(dtm_withoutSparseTerms))

# Free up memory
rm(dtm); rm(dtm_withoutSparseTerms); gc()

# Create sort vector in descending order of frequency
ordr <- order(freqr, decreasing=TRUE)

# Write vectors to disk
freqr_names <- names(freqr)

write.table(freqr, TERM_FREQ_VECTOR_FNAME, row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(freqr_names, TERM_FREQ_NAMES_FNAME, row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(ordr,  TERM_FREQ_ORDERED_VECTOR_FNAME, row.names = FALSE, col.names = FALSE, quote = FALSE)
