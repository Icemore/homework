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

int tCnt;
int up[1000200][20];
int height[1000200];

void init() {
    tCnt = 1;
    height[0] = 0;
}

void add(int v) {
    int u = tCnt++;

    up[u][0] = v;
    for(int i=1; i<20; ++i) {
        up[u][i] = up[up[u][i-1]][i-1];
    }
    
    height[u] = height[v] + 1;
}

int jump(int v, int l) {
    for(int i=0; i<20 && l; ++i) {
        if(l&1) {
            v = up[v][i];
        }

        l>>=1;
    }

    return v;
}

void level(int &u, int &v) {
    if(height[u] > height[v]) {
        u = jump(u, height[u] - height[v]);
    }
    else if(height[u] < height[v]) {
        v = jump(v, height[v] - height[u]);
    }
}

int getLCA(int u, int v) {
    level(u, v);
    
    if(u==v) return v;

    for(int i = 19; i>=0; --i) {
        if(up[u][i] != up[v][i]) {
            u = up[u][i];
            v = up[v][i];
        }
    }

    return up[u][0];
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

   int n;
   char str[5];
   int u, v;

   init();

   scanf("%d", &n);

   forn(i, n) {
       scanf("%s", str);

       if(str[0] == 'a') {
           scanf("%d", &v);
           add(v-1);
       }
       else {
           scanf("%d%d", &v, &u);
           printf("%d\n", getLCA(v-1, u-1) + 1 );
       }
   }

    return 0;
}