library(tidyverse)
library(magrittr)
library(tokenizers)

read_corpus <- \(path= "/Users/I023373/Downloads/ITAB_20231005_TRANSP_.CSV" ){
  path %>%
  read_lines() %>%
    lapply( \(x){
      x %>%
        str_split("#") %>% unlist() %>%
        .[5] }  ) %>%
    unlist()
}

corpus <- read_corpus()

read_vocab <- \(corpus) {
  corpus %>%
    lapply( \(x){
      x %>%
        tokenize_character_shingles( n = 1, n_min = 1, strip_non_alphanum = FALSE ) }  ) %>%
    unlist() %>%
    unique()
}


corpus %>% read_vocab() -> vocab


get_token_stats <- \(corpus) {

read_token_stats <- \(corpus) {
  corpus %>%
    lapply( \(x){
      x %>%
        tokenize_character_shingles( n = 1, n_min = 1, strip_non_alphanum = FALSE ) }  ) %>%
    unlist() %>%
    as_tibble() %>%
    magrittr::set_names(c("token")) %>%
    count(token)
}

corpus %>% read_token_stats() -> token_stats

read_start_token_stats <- \(corpus) {
  corpus%>%
    lapply( \(x){
      x %>%
        tokenize_character_shingles( n = 1, n_min = 1, strip_non_alphanum = FALSE ) %>%
        unlist()  -> seq
      seq[1]
    }  ) %>%
    unlist() %>%
    as_tibble() %>%
    magrittr::set_names(c("token")) %>%
    count(token)

}

corpus %>% read_start_token_stats() -> token_start_stats


token_stats_list <- \(df_) {
  dfList = df_$n
  names(dfList) = df_$token
  dfList
}


token_stats %>%
  left_join( token_start_stats, by = "token" ) %>%
  filter( token != "NA") %>%
  mutate( n = ifelse( is.na( n.y ),n.x,n.x - n.y), token = paste0("##",token) ) %>%
  select(token, n) -> token_not_start_stats

rbind(token_start_stats,token_not_start_stats) -> token_stats

  token_stats %>% token_stats_list()

}

corpus %>% get_token_stats() -> tokenStatList


ngram_statistics <- \(corpus) {
  corpus %>% get_token_stats() -> tokenStatList

  score_two_gram <- \(x) {
    if(str_length(x) < 4 ) {
      return(NA)
    }
    if(str_starts(x,"##")) {
      t1 =   substr(x,1,3)
      t2 =   substr(x,4,6)
    } else {
      t1 =   substr(x,1,1)
      t2 =   substr(x,2,4)
    }
    as.integer(tokenStatList[t1]) * as.integer(tokenStatList[t2])
  }

  score_3_gram <- \(x) {
    if(str_length(x) < 7 ) {
      return(NA)
    }
    if(str_starts(x,"##")) {
      t1 =   substr(x,1,3)
      t2 =   substr(x,4,6)
      t3 =   substr(x,7,9)
    } else {
      t1 =   substr(x,1,1)
      t2 =   substr(x,2,4)
      t3 =   substr(x,5,7)
    }
    as.numeric(tokenStatList[t1]) * as.numeric(tokenStatList[t2]) * as.numeric(tokenStatList[t3])
  }

  score_4_gram <- \(x) {
    if(str_length(x) < 10) {
      return(NA)
    }
    if(str_starts(x,"##")) {
      t1 =   substr(x,1,3)
      t2 =   substr(x,4,6)
      t3 =   substr(x,7,9)
      t4 =   substr(x,10,12)
    } else {
      t1 =   substr(x,1,1)
      t2 =   substr(x,2,4)
      t3 =   substr(x,5,7)
      t4 =   substr(x,8,10)
    }
    as.numeric(tokenStatList[t1]) *
      as.numeric(tokenStatList[t2]) *
      as.numeric(tokenStatList[t3]) *
      as.numeric(tokenStatList[t4])
  }

  score_5_gram <- \(x) {
    if(str_length(x) < 13) {
      return(NA)
    }
    if(str_starts(x,"##")) {
      t1 =   substr(x,1,3)
      t2 =   substr(x,4,6)
      t3 =   substr(x,7,9)
      t4 =   substr(x,10,12)
      t5 =   substr(x,13,15)
    } else {
      t1 =   substr(x,1,1)
      t2 =   substr(x,2,4)
      t3 =   substr(x,5,7)
      t4 =   substr(x,8,10)
      t5 =   substr(x,11,13)
    }

      as.numeric(tokenStatList[t1]) *
      as.numeric(tokenStatList[t2]) *
      as.numeric(tokenStatList[t3]) *
      as.numeric(tokenStatList[t4]) *
      as.numeric(tokenStatList[t5])
  }

   corpus %>%
    lapply(\(x) {
      x %>% tolower()
    }) %>%
    unlist() %>%
    unique() %>%
    as_tibble() %>%
    magrittr::set_names(c("object")) %>%
    rowwise() %>%
    mutate( token =  object %>%
      (\(x){
        tks = x %>%
          str_split("") %>%
          unlist()
        if(length(tks)>1 ){
          tks[2:length(tks)] %>%
            unlist() %>%
            paste0("##",.) %>%
            paste( collapse = ",") %>%
            paste(tks[1],. ,sep=",")
        } else {
          tks[1]
        }
      } ) ) %>%
    separate_rows(token, sep = ",") %>%
    group_by(object ) %>%
    reframe(
      token    = token,
      token_l1 = lead(token),
      token_l2 = lead(lead(token)),
      token_l3 = lead(lead(lead(token))) ,
      token_l4 = lead(lead(lead(lead(token)))),
      token_l5 = lead(lead(lead(lead(lead(token))))) ,
      token_l6 = lead(lead(lead(lead(lead(lead(token)))))),
      two_gram  = sprintf("%s%s", token, token_l1),
      three_gram  = sprintf("%s%s%s", token, token_l1, token_l2),
      four_gram   = sprintf("%s%s%s%s", token, token_l1, token_l2, token_l3),
      five_gram   = sprintf("%s%s%s%s%s", token, token_l1, token_l2, token_l3, token_l4)
    ) %>%
    ungroup() -> df_

    df_  %>%
    filter( !is.na(token_l1) , token_l1 != "NA" ) %>%
    count( two_gram ) %>%
      rowwise() %>%
      mutate( score_ = ( n/score_two_gram(two_gram) )  ) ->
      gram_2_stats



  df_ %>%
  filter(!is.na(token_l2) ) %>%
  count(three_gram) %>%
   rowwise( ) %>%
    mutate(score_ = n/score_3_gram(three_gram) ) -> gram_3_stats


   df_  %>%
  filter(!is.na(token_l3) ) %>%
  count(four_gram) %>%
     rowwise( ) %>%
     mutate(score_ = n / score_4_gram(four_gram) ) -> gram_4_stats


  df_ %>%
  filter(!is.na(token_l4) ) %>%
  count(five_gram) %>%
    rowwise( ) %>%
    mutate(score_ = n / score_5_gram(five_gram) ) -> gram_5_stats

  list( gram_2_stats = gram_2_stats,
        gram_3_stats = gram_3_stats,
        gram_4_stats=gram_4_stats,
        gram_5_stats=gram_5_stats)
}

corpus %>% ngram_statistics()





corpus = c("huggingface","hugging","face","hug","hugger","learning","learning","learners","learn")
corpus = c("low_","lowest_","newer_","wider_","new_")
corpus %>% ngram_statistics() -> vv




##########@
corpus = c("This is the Hugging Face Course.",
           "This chapter is about tokenization.",
           "This section shows several tokenizer algorithms.",
           "Hopefully, you will be able to understand how they are trained and generate tokens.")
get_words_count <- \(corpus) {
  new_words <- corpus %>%
    lapply(\(x){ x %>%
      tokenizers::tokenize_words(strip_punct = F) }) %>%
    unlist()
  new_words %>% unique() -> dict
  dict  %>% lapply(\(x) { which( new_words == x) %>% length()   }) %>% unlist() -> words_count
  names(words_count) = dict
  words_count
}

corpus %>% get_words_count() -> word_counts



alphabet = c()
for(word in names(word_counts)) {
  word %>% tokenize_character_shingles( n = 1, n_min = 1, strip_non_alphanum = FALSE )
  alphabet %>% rbind()

}


