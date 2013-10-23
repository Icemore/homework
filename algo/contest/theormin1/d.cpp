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
vector<int> grav[200000];
vector<int> gravT[200000];
vector<int> topsort;
bool used[200000];
int color[200000];
set< pair<int, int> > condEdges;

void dfs1(int v)
{
	used[v]=true;

	forv(i, grav[v])
	{
		int to=grav[v][i];
		if(!used[to])
			dfs1(to);
	}

	topsort.push_back(v);
}

void dfs2(int v, int c)
{
	color[v]=c;

	forv(i, gravT[v])
	{
		int to=gravT[v][i];
		if(color[to]==0)
			dfs2(to, c);
		else
		{
			if(color[to]!=c)
				condEdges.insert(mp(color[to], c));
		}
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

	cin>>n>>m;
	
	forn(i, m)
	{
		int v, u;

		cin>>v>>u;
		--v; --u;
		grav[v].push_back(u);
		gravT[u].push_back(v);
	}

	forn(i, n)
		if(!used[i])
			dfs1(i);
	
	reverse(all(topsort));

	int c=0;
	forn(i, n)
		if(color[topsort[i]]==0)
			dfs2(topsort[i], ++c);

	cout<<condEdges.size()<<endl;

    return 0;
}