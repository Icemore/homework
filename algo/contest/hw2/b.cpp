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
lng d[200][10][1<<6];
lng mod = 1000000000+9;

void update(int mask, int i, int j, int tomask)
{
	int toi = i+1;
	int toj = j;

	if(toi>n) 
	{
		tomask<<=1;
		toi = 1;
		toj = toj+1;
	}

	tomask&=(1<<(n+1))-1;

	d[toj][toi][tomask] += d[j][i][mask];
	d[toj][toi][tomask] %= mod;
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

	cin >> n >> m;
	
	d[0][1][0] = 1;

	for(int j=0; j<m; ++j)
	{
		for(int i=1; i<=n; ++i)
		{
			for(int mask=0; mask<(1<<(n+1)); ++mask)
			{
				if(d[j][i][mask]==0) continue;

				if(mask&(1<<i))
				{
					update(mask, i, j, mask^(1<<i));
				}
				else
				{
					if(!(mask&(1<<(i-1))))
						update(mask, i, j, mask|(1<<(i-1)));

					if(i<n && !(mask&(1<<(i+1))))
						update(mask, i, j, mask|(1<<(i+1)));
				}
			}
		}
	}

	cout << d[m][1][0] << endl;


    return 0;
}