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
#define forn(i,n) for(int i = 0; i < (int)(n); i++)
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

int n;
char str[300];
bool pal[300][300];
int dp[300][300];
int pr[300][300];

void calcPalindromes()
{
	mset(pal, 0);

	forn(r, n)
	{
		for(int len = 0; len<=r+1; ++len)
		{
			if(len<=1)
			{
				pal[r][len]=true;
			}
			else
			{
				if(str[r]==str[r-len+1])
					pal[r][len] = pal[r-1][len-2];
			}
		}
	}
}

void solveForShift(int shift)
{
	calcPalindromes();

	forn(i, n) 
		dp[shift][i] = iinf;

	forn(i, n)
	{
		for(int len=1; len<=i+1; ++len)
		{
			if(!pal[i][len]) continue;
			int cur = 1;
			if(i>0) cur+=dp[shift][i-len];

			if(cur < dp[shift][i])
			{
				dp[shift][i] = cur;
				pr[shift][i] = len;
			}
		}
	}
}

void shiftStr()
{
	char first = str[0];
	for(int i=0; i<n-1; ++i)
		str[i] = str[i+1];
	str[n-1] = first;
}

void restoreAns(int shift)
{
	forn(i, shift)
		shiftStr();

	vector<string> ans;
	int pos = n-1;
	while(pos>=0)
	{
		int len = pr[shift][pos];
		int newpos = pos-len;

		ans.push_back(string(str+newpos+1, str+pos+1));
		pos=newpos;
	}

	reverse(all(ans));
	forv(i, ans)
		cout << ans[i] <<" ";
	cout << endl;
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

	cin>>n>>str;

	int ans = -1;

	forn(shift, n)
	{
		solveForShift(shift);
		shiftStr();

		if(ans==-1 || dp[ans][n-1] > dp[shift][n-1])
			ans=shift;
	}

	cout << ans << " " << dp[ans][n-1] << endl;
	restoreAns(ans);

    return 0;
}