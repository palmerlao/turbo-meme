#include <iostream>
#include <string>
#include <unordered_map>

using namespace std;

class TrieNode {
  int count;
  unordered_map<char, TrieNode*> nexts;
public:
  void add(string str, int i) {
    if (str.empty()) {
      return;
    } else if (nexts.find(str[0]) == nexts.end()) {
      nexts[str[0]] = new TrieNode;
      nexts[str[0]]->count = 1;
    } else {
      nexts[str[0]]->count += 1;
    }
    nexts[str[0]]->add(str.substr(1),nexts[str[0]]->count);
  }

  int find(string str) {
    if (str.empty())
      return count;
    else if ( nexts.find(str[0]) == nexts.end() )
      return 0;
    else {
      return nexts[str[0]]->find(str.substr(1));
    }
  }
};

int main() {
  TrieNode * t = new TrieNode;
  int n;
  cin >> n;
  string instruction, arg;
  for (int i=0; i<n; i++) {
    cin >> instruction >> arg;
    if (instruction == "add") {
      t->add(arg,0);
    }
    if (instruction == "find") {
      cout << t->find(arg) << endl;
    }
  }
  return 0;
}
