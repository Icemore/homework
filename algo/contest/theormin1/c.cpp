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
vector<int> topsort;
int used[200000];

bool dfs(int v)
{
	if(used[v]!=0)
		return used[v]==2;

	used[v]=1;

	forv(i, grav[v])
	{
		if(!dfs(grav[v][i]))
			return false;
	}

	used[v]=2;

	topsort.push_back(v);
	return true;
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
	}

	bool failed=false;
	forn(i, n)
		if(used[i]==0)
		{
			if(!dfs(i))
			{
				failed=true;
				break;
			}
		}

	if(failed)
		cout<<-1<<endl;
	else
	{
		reverse(all(topsort));
		forv(i, topsort)
			cout<<topsort[i]+1<<" ";
	}

    return 0;
}

