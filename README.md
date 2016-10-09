# Next Word Predictor
A Natural Language Processing R&D project   
Developed by Herbert Barrientos   

## **DESCRIPTION**   
The project is composed of three parts:   
  1. An R program that captures raw source data from three sources, performs data cleaning, and transforms the resulting tidy data into n-gram data files and term frequency data files.   
  
  2. An R, next-word prediction program that uses the produced n-gram and term frequency data files for textual input processing.   
  
  3. An R, Shiny Apps application used to test the next-word prediction program online.   

## **SOURCE CODE**   
  * **capstone_DataProcessing_v3.R**   
    Process that captures input data from various sources (blogs, news tweets), cleans it, and uses it to create six n-grams (i.e., bigrams, trigrams, quadgrams, pentagrams, and sextagrams) that are saved to disk. This input data is also used to create a word frequency vector that is also saved to disk. All saved files will be used by the word-predicting program.   
    
    For more implementation details, please read the program's internal documentation.   
    
  * **predictNextWord.R**   
    Program that predicts the "next word" for a given input string, which may consist of a single word or a phrase. The program first checks input validity. Next, the program determines what n-gram it should first use to start performing the prediction process. If the input string is one word, the program will use bigrams; two words, trigrams; three words, quadgrams; four word, pentagrams; five or more words, sextagrams.   
    
    Once the n-gram to begin with has been determined, the program will load the corresponding data file. A search is conducted to find matches for the input string. For every match, the last word is extracted and used to predict its probability using the Chain Rule of Conditional Probability approach.

    The program is organized as a finite state machine, which processes the input based on its current state. For instance, if the input string consists of seven words, an initial state will convert that input to a five-word string by removing the first two words and keeping the last five words. The resulting five-word input string is processed using sextagrams. If no match is found, a "truncating state" will then remove the first word from the input, thus converting it to a four-word string. At this point, the program backs off and another state will begin another search process using pentagrams. Again, if no match is found, the "truncating state" will again remove the first word from the input and converts it to a three-word string. The program backs off again and another state will begin a search process using quadgrams. of no matches are found, the process repeats for trigrams and finally bigrams. If matches are found at any given state, the words with the highest probabilities are returned. If the "bigram state" is reached and no matches are retrieved, the program returns an empty string.
    
    For more implementation details, please read the program's internal documentation.   
 
  * **ui.R and server.R**   
      Shiny Apps programs.

## **DATA FILES**   
The three raw source data files were provided by Swiftkey, comprising BLOGS, NEWS, and TWEETS. Although versions in several languages were provided, the English version was used for this project.   

Provided that the data owners don't delete them, the raw source data files may be found at this URL:   
https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip   

## **DIRECTORY STRUCTURE**   
For development and execution purposes on a local-machine, the directory structure should be as follows:
  + getwd()   
    + capstone   
      + scripts   
        + capstone_DataProcessing_v3.R   
      + final   
        + en_US   
          + en_US.blogs.txt   
          en_US.news.txt   
          en_US.twitter.txt   
        + en_US_processed   
          + processed_corpus.txt   
          term_freq_names.txt   
          term_freq_ord_vector.txt   
          term_freq_vector.txt   
          bigram.txt   
          pentagram.txt   
          quadgram.txt   
          sextagram.txt   
          trigram.txt   
      + wordpredictor   
         + predictNextWord.R   
         server.R   
         ui.R   
         bigram.txt (copied from the _en_US_processed_ directory)   
         pentagram.txt (copied from the _en_US_processed_ directory)   
         quadgram.txt (copied from the _en_US_processed_ directory)   
         sextagram.txt (copied from the _en_US_processed_ directory)   
         trigram.txt (copied from the _en_US_processed_ directory)   
         term_freq_names.txt (copied from the _en_US_processed_ directory)   
         term_freq_vector.txt (copied from the _en_US_processed_ directory)   

## **SHINY APPS DEMO**   
The demo may be tested at this URL:   
https://hpbarr.shinyapps.io/wordpredictor/   
   
