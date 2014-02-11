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

int n, len;
int a[100500];
int b[100500];
int p[100500];

bool cmp(int i, int j)
{
	return a[i] < a[j];
}

void solve()
{
	forn(i, n)
	{
		int to = upper_bound(b, b+len, i, cmp) - b;
		
		if(to > 0 && a[b[to-1]] == a[i])
			continue;

		b[to] = i;
		if(to == len)
			++len;

		if(to!=0)
			p[i] = b[to-1];
	}
}

void printAns()
{
	vector<int> ans;

	int i = b[len-1];
	while(i!=-1)
	{
		ans.push_back(a[i]);
		i=p[i];
	}


	reverse(all(ans));

	cout<<len<<endl;
	forv(i, ans)
		cout<<ans[i]<<" ";
	cout<<endl;
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

	mset(p, -1);

	cin>>n;
	forn(i, n)
		cin>>a[i];

	solve();
	printAns();


    return 0;
}