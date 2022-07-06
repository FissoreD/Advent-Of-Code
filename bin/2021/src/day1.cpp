#include <iostream>
#include <fstream>
#include <vector>
#include <numeric>
#include <string>
#include <algorithm>
#include <functional>
using namespace std;

const string fileName = "inp/day1.txt";

int day1_1()
{
  ifstream MyReadFile(fileName);
  string myText;
  unsigned int a = 0, b = 0, acc = 0;
  while (MyReadFile >> b)
  {
    if (b > a)
      acc++;
    a = b;
  }
  MyReadFile.close();
  return --acc;
}

int day1_2()
{
  ifstream MyReadFile(fileName);
  string myText;
  vector<int> L;
  unsigned int old_value, acc;
  for (int i = 0; i < 3; ++i)
  {
    MyReadFile >> old_value;
    L.push_back(old_value);
  }
  sort(L.begin(), L.end());
  old_value = accumulate(L.begin(), L.end(), 0, [](int a, int b)
                         { return a + b; });
  cout << old_value << '\n';
  while (MyReadFile >> acc)
  {
    if (acc < )
  }
  MyReadFile.close();
  return --acc;
}

int main(int argc, char const *argv[])
{
  cout << day1_1() << '\n'
       << day1_2();
}
