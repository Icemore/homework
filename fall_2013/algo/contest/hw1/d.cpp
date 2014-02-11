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

int n, m;
vector< vector<int> > a;
vector<lng> rows;
vector<lng> cols;

lng findMin(vector<lng>& vec, int &idx)
{
	lng cur=0, pre=0, post=0;

	forv(i, vec)
	{
		cur+=i*vec[i];
		post+=vec[i];
	}

	pre=vec[0];
	post-=vec[0];

	lng ans=cur;
	idx=0;
	for(size_t i = 1; i < vec.size(); ++i)
	{
		cur-=post;
		cur+=pre;

		post-=vec[i];
		pre+=vec[i];

		if(cur<ans)
		{
			ans=cur;
			idx=i;
		}
	}

	return ans;
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

	cin>>n>>m;
	
	a.resize(n);
	forn(i, n)
		a[i].resize(m);
	rows.assign(n, 0);
	cols.assign(m, 0);

	forn(i, n)forn(j, m)
		cin>>a[i][j];

	lng ans=0;
	forn(i, n)forn(j, m)
	{
		ans+=2*a[i][j];

		cols[j]+=a[i][j];
		rows[i]+=a[i][j];
	}

	int x, y;

	ans+=2*findMin(cols, y);
	ans+=2*findMin(rows, x);

	cout<<x+1<<" "<<y+1<<endl;
	cout<<ans<<endl;

    return 0;
}

