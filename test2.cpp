#include <Rcpp.h>
#include <map>
#include <regex>

using namespace Rcpp;
using namespace std;

class TrieNode {
 public:
  char char_;
  unordered_map<char, TrieNode*> children_;
  vector<TrieNode*> F_;
  TrieNode* f_;
  string chiv;
  bool isEndOfWord;

  TrieNode(char c) : char_(c), children_({{}}), F_(), f_(nullptr), chiv(""), isEndOfWord(false) {}
};

class Trie {
 public:
  TrieNode* root_;

  Trie() : root_(new TrieNode(' ')) {}

  void Build(const unordered_set<string>& vocab) {
    TrieNode* r = root_;
    TrieNode* r_hash = nullptr;

    for (const string& key : vocab) {
      TrieNode* start = root_;
      for (const char c : key) {
        bool isFound = false;

        if (start->children_.count(c) > 0) {
          start = start->children_[c];
          isFound = true;
          continue;
        }

        if (!isFound) {
          string parent_chiv = start->chiv;
          start->children_[c] = new TrieNode(c);
          start = start->children_[c];
          start->chiv = parent_chiv + c;
          if (c == '#')  r_hash = start;

        }
      }

      start->isEndOfWord = true;
    }
  }
};




// [[Rcpp::export]]
bool checkClass( )
{
    std::unordered_set<std::string> vocab = {"hello", "world", "!"};

    Trie trie;
    trie.Build(vocab);

    // Search for a word in the trie.
    bool isFound = trie.Search("hello");

    // Check if the trie contains a prefix.
    return trie.HasPrefix("wor");
 }




