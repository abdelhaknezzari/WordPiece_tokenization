library(tidyverse)
library(Rcpp)


corpus = c("This is the Hugging Face Course.",
           "This chapter is about tokenization.",
           "This section shows several tokenizer algorithms.",
           "Hopefully, you will be able to understand how they are trained and generate tokens.")

Rcpp::sourceCpp("rcppFunctions.cpp")

## Test
splitByDelimiter("Wx##fc##df##mffg","##") %>%
testthat::expect_equal(c("Wx","fc","df","mffg"),
                        "function splitByDelimiter is fine!")

splitTokensByDelimiter("##fc##df##mffg","##") %>%
  testthat::expect_equal(c("##fc" ,  "##df" ,  "##mffg"))

splitTokensByDelimiter("wx##fc##df##mffg","##") %>%
   testthat::expect_equal(c("wx","##fc" ,  "##df" ,  "##mffg"))

testthat::expect_equal(splitCorpusToWords(corpus),
                       c("This",
                         "is" ,
                         "the"  ,
                         "Hugging",
                         "Face",
                         "Course",
                         "." ,"",
                         "This" ,
                         "chapter",
                         "is",
                         "about",
                         "tokenization" ,
                         "."  ,
                         "",
                         "This",
                         "section" ,
                         "shows",
                         "several" ,
                         "tokenizer",
                         "algorithms" ,
                         "." ,
                         ""           ,
                         "Hopefully"  ,
                         ",",  ""   ,
                         "you"       ,
                         "will"       ,
                         "be"       ,
                         "able",
                         "to"       ,
                         "understand" ,
                         "how"       ,
                         "they"     ,
                         "are",
                         "trained"  ,
                         "and",
                         "generate"  ,
                         "tokens"   ,
                         ".",
                         ""))


list(
This= c("T" ,  "##h", "##i" ,"##s"),
is=c("i"  , "##s"),
the=c("t"  , "##h" ,"##e"),
Hugging=c("H",   "##u", "##g" ,"##g", "##i" ,"##n" ,"##g"),
Face = c("F",   "##a" ,"##c" ,"##e"),
Course=c("C" ,  "##o" ,"##u", "##r", "##s" ,"##e"),
.=c("."),
`[SEP]`=c(""),
chapter= c("c"   ,"##h", "##a", "##p" ,"##t", "##e", "##r"),
about=c("a"  , "##b", "##o", "##u" ,"##t"),
tokenization=c("t" ,  "##o" ,"##k", "##e", "##n", "##i", "##z", "##a" ,"##t" ,"##i", "##o" ,"##n"),
section=c("s"  , "##e" ,"##c", "##t" ,"##i", "##o", "##n"),
shows=c("s"  , "##h", "##o", "##w" ,"##s"),
several=c("s"  , "##e", "##v", "##e", "##r", "##a" ,"##l"),
tokenizer =c("t" ,  "##o", "##k" ,"##e", "##n", "##i", "##z", "##e", "##r"),
algorithms=c("a" ,  "##l", "##g", "##o" ,"##r" ,"##i", "##t", "##h" ,"##m", "##s"),
Hopefully=c("H" ,  "##o", "##p", "##e", "##f" ,"##u", "##l", "##l", "##y"),
`,` = c(','),
you =c('y',"##o", "##u"),
will =c("w",  "##i" ,"##l" ,"##l"),
be=c("b"  , "##e"),
able =c("a",  "##b" ,"##l", "##e"),
to = c("t" ,  "##o"),
understand = c("u"  , "##n", "##d" ,"##e" ,"##r" ,"##s", "##t" ,"##a" ,"##n", "##d"),
how = c("h"  , "##o" ,"##w"),
they = c("t" ,  "##h" ,"##e" ,"##y"),
are = c("a" ,  "##r" ,"##e"),
trained = c("t"   ,"##r", "##a", "##i", "##n", "##e" ,"##d"),
and = c("a"   ,"##n", "##d"),
generate = c( "g"  , "##e", "##n", "##e" ,"##r" ,"##a" ,"##t", "##e"),
tokens = c("t"  , "##o", "##k", "##e", "##n" ,"##s")
) %>%
  testthat::expect_equal(splitWords(splitCorpusToWords(corpus)),.,,"splitWords Is OK")

check_computePairScores <- function() {
  wordOccurences = splitCorpusToWords(corpus)
  splits = splitWords(wordOccurences)
  tibble(
    token= c("##f##u", "a##b", "F##a", "##h##m", "T##h", "c##h", "##m##s",
             "H##u", "i##s", "##i##z", "u##n", "w##i", "##o##k", "##o##w",
             "C##o", "h##o", "y##o", "##a##c", "##a##p", "##b##l", "##c##t",
             "##l##y", "##p##t", "##z##a", "##n##d", "##g##g", "##u##g",
             "##w##s", "##e##f", "##e##v", "##k##e", "##r##a", "##v##e", "b##e",
             "g##e", "##o##u", "t##o", "s##h", "##a##t", "##l##l", "##b##o", "##o##p",
             "H##o", "##l##g", "t##h", "##h##i", "s##e", "##u##l", "##u##t",
             "a##l", "##i##s", "##e##r", "##t##i", "##c##e", "##e##c", "##e##y",
             "##p##e", "##z##e", "##g##i", "##n##g", "##r##s", "##u##r", "a##r", "##a##l",
             "##t##a", "##g##o", "a##n", "##h##a", "##t##h", "##e##n", "##i##n",
             "##n##i", "t##r", "##s##t", "##i##o", "##o##n", "##t##e", "##a##i",
             "##a##n", "##i##l", "##i##t", "##d##e", "##e##d", "##h##e", "##r##i",
             "##h##o", "##n##s", "##n##e", "##o##r", "##l##e", "##r##e", "##s##e"),
    score= c(0.2, 0.2, 0.142857142857143, 0.125, 0.125, 0.125, 0.1, 0.1, 0.1, 0.0909090909090909, 0.0909090909090909,
             0.0909090909090909, 0.0769230769230769, 0.0769230769230769, 0.0769230769230769, 0.0769230769230769,
             0.0769230769230769, 0.0714285714285714, 0.0714285714285714, 0.0714285714285714, 0.0714285714285714,
             0.0714285714285714, 0.0714285714285714, 0.0714285714285714, 0.0681818181818182,
             0.0625, 0.05, 0.05, 0.0476190476190476, 0.0476190476190476, 0.0476190476190476, 0.0476190476190476,
             0.0476190476190476, 0.0476190476190476, 0.0476190476190476, 0.0461538461538462, 0.043956043956044,
             0.0416666666666667, 0.0408163265306122, 0.0408163265306122, 0.0384615384615385, 0.0384615384615385,
             0.0384615384615385, 0.0357142857142857, 0.0357142857142857, 0.0340909090909091, 0.0317460317460317, 0.0285714285714286,
             0.0285714285714286, 0.0285714285714286, 0.0272727272727273, 0.0264550264550265, 0.025974025974026, 0.0238095238095238,
             0.0238095238095238, 0.0238095238095238, 0.0238095238095238, 0.0238095238095238, 0.0227272727272727, 0.0227272727272727,
             0.0222222222222222, 0.0222222222222222, 0.0222222222222222, 0.0204081632653061, 0.0204081632653061, 0.0192307692307692,
             0.0181818181818182, 0.0178571428571429, 0.0178571428571429, 0.0173160173160173, 0.0165289256198347, 0.0165289256198347,
             0.0158730158730159, 0.0142857142857143, 0.013986013986014, 0.013986013986014, 0.0136054421768707, 0.012987012987013,
             0.012987012987013, 0.012987012987013, 0.012987012987013, 0.0119047619047619, 0.0119047619047619, 0.0119047619047619,
             0.0101010101010101, 0.00961538461538462, 0.00909090909090909, 0.00865800865800866, 0.00854700854700855, 0.00680272108843537,
             0.00529100529100529, 0.00476190476190476)%>% as.numeric()  ) -> vv1

  tibble(
    token = computePairScores(wordOccurences, splits) %>% sort(decreasing = T) %>% names(),
    score = computePairScores(wordOccurences, splits) %>% sort(decreasing = T) %>% as.numeric() ) -> vv2

  (vv1 %>% as.matrix() == vv2 %>% as.matrix()) %>%
    as.vector() %>%
    every(rlang::is_true) %>%
  testthat::expect_true("computePairScores Is OK")
}

check_computePairScores()

check_vocabGeneration <- function(){


  wordOccurences = splitCorpusToWords(corpus)
  splits = splitWords(wordOccurences)

  token = getMaxScoreToken(wordOccurences, splits)
  pairL = splitTokensByDelimiter(token,"##")
  pairL
  splits = mergePair(splits, pairL[1], pairL[2])
  vocab = vocabFromSplits(splits)
  vocab
  length(vocab)

  token = getMaxScoreToken(wordOccurences, splits)
  pairL = splitTokensByDelimiter(token,"##")
  splits = mergePair(splits, pairL[1], pairL[2])
  vocab = vocabFromSplits(splits)
  length(vocab)


  token = getMaxScoreToken(wordOccurences, splits)
  pairL = splitTokensByDelimiter(token,"##")
  splits = mergePair(splits, pairL[1], pairL[2])
  vocab = vocabFromSplits(splits)
  length(vocab)




  token = getMaxScoreToken(wordOccurences, splits)
  pairL = splitTokensByDelimiter(token,"##")
  splits = mergePair(splits, pairL[1], pairL[2])
  vocab = vocabFromSplits(splits)
  length(vocab)





}


###


corpus %>% splitCorpusToWords() %>% getWordsFrequency()

corpus %>% splitCorpusToWords() -> wordOccurences
splits = splitWords(wordOccurences)
getPairFrequencies(splits,wordOccurences)

wordOccurences %>%getLetterFrequencies(splits)
wordOccurences %>%getWordsFrequency()

wordOccurences %>% computePairScores(splits) %>% sort(decreasing = T)

wordOccurences %>% computePairScores2() %>% bind_rows() %>% View()
mergePair(splits, "##o","##k")-> vv
mergePair(splits, "##ok","##e") -> vv
mergePair(splits, "##oke","##n") -> vv
mergePair(splits, "##oken","##s") -> vv
mergePair(splits,"t", "##okens") -> vv


setupSignalHandler <- function() {
  sig <- signal(SIGTRAP, function(signum) {
    stop(paste("Received signal", signum))
  })
  on.exit(signal(SIGTRAP, sig))
}
setupSignalHandler()

corpus %>% generateVocab()



generateVocab1<- function (corpus, vocab_size = 70)
{
   vocab = c()
   wordOccurences = splitCorpusToWords(corpus)
   currentVocabSize = 0

  splits = splitWords(wordOccurences)
while (currentVocabSize < vocab_size)
{

  #############
  getLetterFrequencies(wordOccurences, splits)

   letterFreqs = getLetterFrequencies(wordOccurences, splits)
   pairFeqs = getPairFrequencies(splits, wordOccurences)
   words =  pairFeqs %>% names()
  index =1
  for (freq in pairFeqs)
  {
    freq =  pairFeqs[index]
    token = words[index]
    pair = c()
    if(token %>% substr(1, 2) == "##") {
      pair = c(token %>% substr(0, 3),token %>% substr(4, 6))
    } else {
      pair = c(token %>% substr(1, 1), token %>% substr(2, 4))
    }
      score = freq %>% as.numeric() / (letterFreqs[pair[1]] %>% as.numeric()* letterFreqs[2] %>% as.numeric())
      pairScores[token] = score;
  }

  ##########@

    scores = computePairScores(wordOccurences, splits)
    maxEntry = which(scores == (scores %>% max())) %>% names() %>% .[1]
    token = maxEntry

   pair = c()
   if(token %>% substr(0, 2) == "##") {
     pair = c(token %>% substr(0, 3),token %>% substr(4, 6))
   } else {
     pair = c(token %>% substr(0, 1), token %>% substr(1, 4))
   }
  splits = mergePair(splits, pair[1], pair[2])
    vocab = vocabFromSplits(splits)

   currentVocabSize = currentVocabSize + 1

  }

}