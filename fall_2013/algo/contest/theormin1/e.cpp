#pragma comment(linker,"/STACK:256000000")
#define _CRT_SECURE_NO_DEPRECATE

#include <cstdio>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <list>
#include <map>
#include <set>
#include <queue>
#include <deque>
#include <stack>
#include <algorithm>
#include <functional>
#include <cmath>
#include <sstream>
#include <utility>
#include <ctime>
#include <memory.h>
#include<bitset>
#define forn(i,n) for (int i = 0; i < (int)(n); i++)
#define forv(i, a) for(int i=0; i<(int)a.size(); i++)
#define mset(a, val) memset(a, val, sizeof(a))
#define all(a) a.begin(),a.end()
#define mp make_pair
#define pb push_back
#define VI vector< int >
#define PII pair< int,int >
#define sqr(a) ((a)*(a))
#define cube(a) ((a)*(a)*(a))
#define pi 3.1415926535897932384626433832795
#define PI pi
#define iinf 1000000000
#define linf 1000000000000000000LL
#define sinf 10000
#define eps 1e-9
#define lng long long
#define ulng unsigned long long
#define uint unsigned int
#define X first
#define Y second
using namespace std;
#define prev asdprev
#define exit(a) do{ if (a) cerr<<"oops "<<a<<endl; exit(a); }while(0)

int n, k;
int a[1005000];
int median;

void printAns(int left, int right)
{
  for (int i = left; i <= right; i++)
    printf("%d ", a[i]);
  printf("\n");
}

bool cmp(int first, int second)
{
  if(abs(first-median) == abs(second - median))
    return first < second;
  else
    return abs(first-median) < abs(second - median);
}

void solve()
{
  int m = (n-1)/2; //median index
  int left = (n-k)/2, right = (n+k)/2-1;

  nth_element(a, a+m, a+n);
  median = a[m];

  // by position
  nth_element(a, a+left, a+m+1);
  nth_element(a+m, a+right, a+n);
  printAns(left, right);

  // by value
  nth_element(a, a+k-1, a+n, cmp);
  printAns(0, k-1);
}


#define taska "casting"
int main() {
#ifdef __ASD__
    freopen("input.txt", "r", stdin);freopen("output.txt", "w", stdout);
#else
    //freopen(taska".in", "r", stdin);freopen(taska".out", "w", stdout);
    //freopen("input.txt", "r", stdin);freopen("output.txt", "w", stdout);
#endif
    //ios_base::sync_with_stdio(false); cin.tie(0);
  
  scanf("%d", &n);
  forn(i, n) scanf("%d", a+i);
  scanf("%d", &k);

  if(k==0)
  {
    cout<<endl;
    return 0;
  }

  solve();
    return 0;
}