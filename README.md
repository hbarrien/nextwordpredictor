# Next Word Predictor
A Natural Language Processing R&D project   
Developed by Herbert Barrientos   

## **DESCRIPTION**   
The project is composed of three parts:   
  1. An R program that captures raw source data from three sources, performs data cleaning, and transforms the resulting tidy data into n-gram data files.   
  2. A next-word prediction algorithm that uses the produced n-gram data files for textual input processing.   
  3. A Shiny Apps application used to test the prediction algorithm online.   

## **SOURCE CODE**   
**capstone_DataProcessing_v3.R**   

  * **predictNextWord.R**   
    Process that captures input data from various sources (blogs, news tweets), cleans it, and uses it to create six n-grams (i.e., bigrams, trigrams, quadgrams, pentagrams, and sextagrams) that are saved to disk. This input data is also used to create a word frequency vector that is also saved to disk. All saved files will be used by the word-predicting algorithm.   
    
    For more implementation details, please read the program's internal documentation.   
    
  * **ui.R**   

  * **server.R**   

## **DATA FILES**   
