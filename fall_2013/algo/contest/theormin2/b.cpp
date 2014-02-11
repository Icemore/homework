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

int n, m;
vector< PII > grav[50000];
vector<int> cutPoints;

int curTime;
int tin[50000], d[50000];
bool used[50000];

int dfs(int v, int p)
{
	used[v]=true;
	tin[v] = curTime++;
	d[v] = tin[v];

	bool isCutPoint = false;
	int children = 0;

	forv(i, grav[v])
	{
		int to = grav[v][i].first;
		if(to == p) continue;

		if(used[to])
		{
			d[v] = min(d[v], tin[to]);
		}
		else
		{
			++children;
			d[v] = min(d[v], dfs(to, v));
			if(d[to] >= tin[v] && p!=-1)
				isCutPoint = true;
		}
	}

	if(p==-1 && children>1)
		isCutPoint = true;

	if(isCutPoint)
		cutPoints.push_back(v);

	return d[v];
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
		int u, v;

		cin>>u>>v;
		--u; --v;

		grav[u].pb(mp(v, i));
		grav[v].pb(mp(u, i));
	}

	curTime=0;
	forn(i, n)
		if(!used[i])
			dfs(i, -1);

	sort(all(cutPoints));

	cout<<cutPoints.size()<<endl;
	forv(i, cutPoints)
		cout<<cutPoints[i]+1<<" ";
	cout<<endl;

    return 0;
}