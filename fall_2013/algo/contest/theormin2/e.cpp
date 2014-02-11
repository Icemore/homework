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

int n, t;
int usedRooms;
int d[100500];
pair<int, pair<int, int> > events[200500];


void solve()
{
	sort(events, events+t);

	queue<int> freeRooms;

	forn(i, t)
	{
		int id = events[i].second.second;
		int type = events[i].second.first;

		if(type == 0)
		{
			freeRooms.push(d[id]);
		}
		else
		{
			int room = -1;

			if(freeRooms.empty())
			{
				room = ++usedRooms;
			}
			else
			{
				room = freeRooms.front();
				freeRooms.pop();
			}

			d[id] = room;
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

	cin>>n;
	forn(i, n)
	{
		int b, e;
		cin>>b>>e;

		events[t++] = mp(b, mp(1, i));
		events[t++] = mp(e, mp(0, i));
	}

	solve();

	cout<<usedRooms<<endl;
	forn(i, n)
		cout<<d[i]<<" ";
	cout<<endl;

    return 0;
}