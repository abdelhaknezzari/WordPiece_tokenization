#include <Rcpp.h>
#include <map>
#include <regex>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
std::vector<std::string> splitString(const std::string &inputString)
{
  std::vector<std::string> characters;
  for (char c : inputString)
  {
    characters.push_back(string(1, c));
  }

  return characters;
}

// Function to split sentence into words
// and handle punctuation marks
// [[Rcpp::export]]
vector<string> splitSentence(const std::string &sentence)
{
  std::vector<std::string> tokens;

  std::stringstream ss(sentence);
  std::string token;

  while (ss >> token)
  {
    size_t start = 0, end = 0;
    while ((end = token.find_first_of(",.;?!", start)) != std::string::npos)
    {
      tokens.push_back(token.substr(start, end - start));
      tokens.push_back(token.substr(end, 1));
      start = end + 1;
    }
    tokens.push_back(token.substr(start));
  }

  return tokens;
}

// [[Rcpp::export]]
CharacterVector splitByDelimiter(const string &input, const string &delimiter)
{
  CharacterVector characters;
  string myToken = "";
  for (int i = 0; i < input.size(); ++i)
  {
    if (input[i] != delimiter[0])
    {
      myToken += input[i];
    }

    if (i == input.size() || (input[i] == delimiter[0] && input[i + 1] == delimiter[1]  && myToken != ""))
    {
      characters.push_back(myToken);
      myToken = "";
    }
  }
  characters.push_back(myToken);

  return characters;
}


// [[Rcpp::export]]
CharacterVector splitTokensByDelimiter(const string &input, const string &delimiter)
{
  CharacterVector characters;
  string myToken = "";
  for (int i = 0; i < input.size()-1; ++i)
  {

    if ( input[i] == delimiter[0] &&
         input[i+1] == delimiter[1] &&
         myToken != "" )
    {
      characters.push_back(myToken);
      myToken = "";
    }
    myToken += input[i];
  }
  characters.push_back(myToken+ input[input.size()-1]);

  return characters;
}

// Function to split sentence into words
// and handle punctuation marks
// [[Rcpp::export]]
vector<string> splitCorpusToWords(const vector<string> &corpus)
{
  vector<string> tokens;

  for (int i = 0; i < corpus.size(); i++)
  {
    std::vector<std::string> tokensSentence = splitSentence(corpus[i]);
    for (int j = 0; j < tokensSentence.size(); j++)
    {
      tokens.push_back(tokensSentence[j]);
    }
  }
  return tokens;
}

// Function to split sentence into words
// and handle punctuation marks
// [[Rcpp::export]]
vector<string> splitCorpusToChars(const vector<string> &corpus)
{
  vector<string> tokens;

  vector<string> words = splitCorpusToWords(corpus);

  for (string word : words)
  {
    vector<string> tokenChars = splitString(word);
    for (int i = 0; i < tokenChars.size(); i++)
    {
      string myToken = i == 0 ? tokenChars[i] : ("##" + tokenChars[i]);
      tokens.push_back(myToken);
    }
  }
  return tokens;
}

// Function to split sentence into words
// and handle punctuation marks
// [[Rcpp::export]]
map<string, vector<string>> getWordChars(const vector<string> &words)
{
  map<string, vector<string>> wordChars;
  for (string word : words)
  {
    vector<string> tokenChars = splitString(word);
    vector<string> tokens;
    for (int i = 0; i < tokenChars.size(); i++)
    {
      string myToken = i == 0 ? tokenChars[i] : ("##" + tokenChars[i]);
      tokens.push_back(myToken);
    }
    wordChars[word] = tokens;
  }
  return wordChars;
}

// [[Rcpp::export]]
List splitWords(const vector<string> &words)
{
  List wordChars = List::create();
  for (string word : words)
  {
    vector<string> tokenChars = splitString(word);
    vector<string> tokens;
    for (int i = 0; i < tokenChars.size(); i++)
    {
      string myToken = i == 0 ? tokenChars[i] : ("##" + tokenChars[i]);
      tokens.push_back(myToken);
    }
    auto currentWord = word == "" ? "[SEP]" : word;
    if (word == "")
    {
      tokens.push_back("");
    }
    wordChars[currentWord] = tokens;
  }
  return wordChars;
}

// [[Rcpp::export]]
set<string> getCorpusChars(const vector<string> &corpus)
{
  set<string> tokens;

  vector<string> words = splitCorpusToWords(corpus);

  for (string word : words)
  {
    vector<string> tokenChars = splitString(word);

    for (int i = 0; i < tokenChars.size(); i++)
    {
      string myToken = i == 0 ? tokenChars[i] : ("##" + tokenChars[i]);
      if (tokens.find(myToken) == tokens.end())
      {
        tokens.insert(myToken);
      }
    }
  }
  set<string> specialTokens = {"[PAD]", "[UNK]", "[CLS]", "[SEP]", "[MASK]"};
  tokens.insert(specialTokens.begin(), specialTokens.end());
  return tokens;
}

// [[Rcpp::export]]
double getListValue(const List &myList, const string &myKey)
{
  try
  {
    return myList[myKey];
  }
  catch (std::exception &ex)
  {
  }
  catch (...)
  {
  }
  return NA_REAL;
}

// [[Rcpp::export]]
bool test1(string test)
{
  return test == "";
}

// Function to split sentence into words
// and handle punctuation marks
// vector<string> words = splitCorpusToWords(corpus);
// [[Rcpp::export]]
List getWordsFrequency(const vector<string> &words)
{
  List wordFreq = List::create();
  for (string word : words)
  {
    string key = word;
    if (word == "")
    {
      key = "[SEP]";
    }
    auto currentCount = getListValue(wordFreq, key);
    if (::isnan(currentCount))
    {
      wordFreq[key] = 1;
    }
    else
    {
      wordFreq[key] = currentCount + 1;
    }
  }
  return wordFreq;
}

//    map<string,vector<string>> splits = getWordChars(corpus);
// [[Rcpp::export]]
List getLetterFrequencies(const vector<string> &wordOccurences, const List &splits)
{
  List letterFreqs = List::create();
  List wordFreqs = getWordsFrequency(wordOccurences);
  CharacterVector words = as<CharacterVector>(splits.names());

  for (int j = 0; j < splits.size(); j++)
  {
    vector<string> split = splits[j];
    for (int i = 0; i < split.size(); i++)
    {
      auto currentWord = as<string>(words[j]);
      auto wordFreq = as<double>(wordFreqs[currentWord]);
      auto currentCount = getListValue(letterFreqs, split[i]);
      //  Rcout << "word:" << currentWord << " -- value:" << wordFreq << " -- count:" << currentCount << endl;
      letterFreqs[split[i]] = (::isnan(currentCount) ? 0 : currentCount) + wordFreq;
    }
  }
  return letterFreqs;
}

// [[Rcpp::export]]
List getPairFrequencies(const List &splits, const vector<string> &wordOccurences)
{
  List pairFreqs = List::create();
  CharacterVector words = as<CharacterVector>(splits.names());
  List wordFreqs = getWordsFrequency(wordOccurences);

  for (int i = 0; i < splits.size(); ++i)
  {
    vector<string> split = splits[i];
    auto myWord = as<string>(words[i]);
    if (myWord == "")
    {
      myWord = "[SEP]";
    }
    auto wordFreq = as<double>(wordFreqs[myWord]);
    if (split.size() > 1)
    {
      for (int i = 0; i < split.size() - 1; i++)
      {
        string myPair = split[i] + split[i + 1];
        auto currentFreq = getListValue(pairFreqs, myPair);
        pairFreqs[myPair] = (!::isnan(currentFreq) ? currentFreq : 0) + wordFreq;
      }
    }
  }
  return pairFreqs;
}

// // [[Rcpp::export]]
// string tokenToPair(string token){
//    CharacterVector pair;
//    string startTk = token.substr(0, 2) == "##" ;

//    if(startTk == "##" ){
//      return token.substr(3, token.size() - 2 );
//    } else {

//    }
// }

// [[Rcpp::export]]
map<string, double> computePairScores(const vector<string> &wordOccurences, const List &wordSplits)
{
  map<string, double> pairScores;
  List letterFreqs = getLetterFrequencies(wordOccurences, wordSplits);
  List pairFeqs = getPairFrequencies(wordSplits, wordOccurences);
  CharacterVector words = as<CharacterVector>(pairFeqs.names());
  for (int i = 0; i < pairFeqs.size(); ++i)
  {
    double freq = pairFeqs[i];
    string token = as<string>(words[i]);
    CharacterVector pairL = splitTokensByDelimiter(token,"##");
    double score = freq / (as<double>(letterFreqs[as<string>(pairL[0])]) *
                           as<double>(letterFreqs[as<string>(pairL[1])]));
    pairScores[(as<string>(pairL[0]) + as<string>(pairL[1]))] = score;
  }
  return pairScores;
}

// [[Rcpp::export]]
map<string, map<string, string>> computePairScores2(const vector<string> &wordOccurences)
{
  map<string, map<string, string>> pairScores;
  List splits = splitWords(wordOccurences);
  List pairFeqs = getPairFrequencies(splits, wordOccurences);
  CharacterVector words = as<CharacterVector>(pairFeqs.names());
  List letterFreqs = getLetterFrequencies(wordOccurences, splits);

  for (int i = 0; i < pairFeqs.size(); ++i)
  {
    double freq = pairFeqs[i];
    string token = as<string>(words[i]);
   
   CharacterVector pairL = splitTokensByDelimiter(token,"##");
   double score = freq / (as<double>(letterFreqs[as<string>(pairL[0])]) *
                           as<double>(letterFreqs[as<string>(pairL[1])]));
    map<string, string> map1;
    map1["score"] = to_string(score);
    map1["freql1"] = to_string( as<double>(letterFreqs[as<string>(pairL[0])] ) );
    map1["freql2"] = to_string( as<double>(letterFreqs[as<string>(pairL[1]) ] ) );
    map1["freq"] = to_string(freq);
    map1["pair"] = as<string>(pairL[0])  +  as<string>(pairL[1]);
    pairScores[as<string>(pairL[0])  +  as<string>(pairL[1])] = map1;
  }
  return pairScores;
}

//   map<string,vector<string>> splits = getWordChars(corpus);

template <typename T, typename F>
auto operator|(T &&arg, F &&func)
{
  return forward<F>(func)(forward<T>(arg));
}

// [[Rcpp::export]]
List vv1(List wordSplits, string field)
{
  wordSplits[field] = as<double>(wordSplits[field]) + 1;
  return wordSplits;
}

bool strStartWith(const string &str, const string &prefix)
{
  return str.compare(0, prefix.length(), prefix) == 0;
}

// [[Rcpp::export]]
List mergePair( List &wordSplits, const string &l1, const string &l2)
{
  // List mergedPairs = List::create();
  CharacterVector fields = as<CharacterVector>(wordSplits.names());
  for (int i = 0; i < wordSplits.size(); i++)
  {
    CharacterVector wordChars = as<CharacterVector>(wordSplits[i]);
    if (wordChars.size() > 1)
    {
      for (int i = 0; i < wordChars.size() - 1; ++i)
      {
        if ((wordChars(i) == l1) && (wordChars(i + 1) == l2))
        {

          auto l2merge = strStartWith(l2, "##") ? l2.substr(2, l2.size()) : l2;
          wordChars(i) = l1 + l2merge;
          wordChars.erase(wordChars.begin() + i + 1);
        }
      }
      wordSplits[as<string>(fields[i])] = wordChars;
    }
  }
  return wordSplits;
}

// [[Rcpp::export]]
CharacterVector vocabFromSplits(List &wordSplits)
{
  set<string> uniqueTokens;
  for (int i = 0; i < wordSplits.size(); ++i)
  {
    auto tokens = as<vector<string>>(wordSplits[i]);
    for (int j = 0; j < tokens.size(); ++j)
    {
      uniqueTokens.insert(tokens[j]);
    }
  }

  return CharacterVector(uniqueTokens.begin(), uniqueTokens.end());
}

// [[Rcpp::export]]
string getMaxScoreToken( const vector<string> &wordOccurences, const List &splits )
{
    map<string, double> scores = computePairScores(wordOccurences, splits);
    auto maxEntry = max_element(scores.begin(), scores.end(),
                                [](const pair<string, double> &entry1, const pair<string, double> &entry2)
                                {
                                  return (entry1.second < entry2.second);
                                });
    return maxEntry->first;
 }

// [[Rcpp::export]]
CharacterVector generateVocab(const vector<string> &corpus, const int vocab_size = 70)
{
  CharacterVector vocab;
  vector<string> wordOccurences = splitCorpusToWords(corpus);
  int currentVocabSize = 0;
  List splits = splitWords(wordOccurences);

  while (currentVocabSize < vocab_size)
  {
    auto token = getMaxScoreToken(wordOccurences, splits);
    const auto pairL = splitTokensByDelimiter(token,"##");
    if(pairL.size() != 2) {
     break;
    }
    splits = mergePair(splits, as<string>(pairL[0]), as<string>(pairL[1]));
    vocab = vocabFromSplits(splits);
    currentVocabSize++;
  }
  return vocab;
}

map<string, double> sortMap(const map<string, double> &myMap)
{
  vector<pair<string, double>> vec;
  for (const auto &[key, value] : myMap)
  {
    vec.push_back({key, value});
  }

  // Sort the vector of pairs by the second element.
  sort(vec.begin(), vec.end(), [](const auto &a, const auto &b)
       { return a.second < b.second; });

  // Create a new map from the sorted vector of pairs.
  map<string, double> sortedMap;
  for (const auto &[key, value] : vec)
  {
    sortedMap[key] = value;
  }
  return sortedMap;
}

// [[Rcpp::export]]
map<string, double> getUsedCharsStatistics(const vector<string> &corpus)
{
  map<string, double> myMap;

  vector<string> corpusChars = splitCorpusToChars(corpus);

  for (string line : corpusChars)
  {
    auto it = myMap.find(line);
    if (it != myMap.end())
    {
      myMap[line] += 1;
    }
    else
    {
      myMap[line] = 1;
    }
  }

  return sortMap(myMap);
}

// Function to split sentence into words
// and handle punctuation marks
// [[Rcpp::export]]
std::string convToLowerCase(const std::string &sentence)
{
  std::string input = sentence;
  std::transform(input.begin(), input.end(), input.begin(), [](unsigned char c)
                 { return std::tolower(c); });
  return input;
}
