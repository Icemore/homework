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

struct mstack
{
	void push(int num)
	{
		int cmax = num;
		if(!st.empty())
			cmax = max(cmax, st.top().second);

		st.push(mp(num, cmax));
	}

	int pop()
	{
		int res = st.top().first;
		st.pop();
		return res;
	}

	int getMax()
	{
		return st.top().second;
	}

	int top()
	{
		return st.top().first;
	}

	bool empty()
	{
		return st.empty();
	}

	stack< pair<int, int> > st;
};

struct mqueue
{
	void push(int num)
	{
		st1.push(num);
	}

	int pop()
	{
		rearrange();
		return st2.pop();
	}

	int top()
	{
		rearrange();
		return st2.top();
	}

	int getMax()
	{
		int res=-1;

		if(!st1.empty()) res=max(res, st1.getMax());
		if(!st2.empty()) res=max(res, st2.getMax());

		return res;
	}

	void rearrange()
	{
		if(st2.empty())
		{
			while(!st1.empty())
				st2.push(st1.pop());
		}
	}

	mstack st1, st2;
};

string str;
int n, k;

void solve()
{
	mqueue q;

	forn(i, k+1)
		q.push(str[i]-'0');

	forn(i, n-k)
	{
		int cmax = q.getMax();
		while(q.top() != cmax)
			q.pop();

		cout<<q.pop();
		if(i+1 != n-k)
			q.push(str[k+i+1]-'0');
	}
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

	cin>>str;
	cin>>k;

	n=str.size();
	solve();

    return 0;
}

