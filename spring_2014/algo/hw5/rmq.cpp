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

struct node {
    lng maxVal;
    lng delta;

    node(lng val = 0, lng add = 0) 
        : maxVal(val), delta(add)
    {}
};

const int maxn = 100500;
int n, q;
int a[maxn];
node tree[4*maxn];

void build(int i=1, int l=0, int r=n-1) {
    if(l==r) {
        tree[i].maxVal = a[l];
        return;
    }

    int c = (l+r)/2;

    build(i*2, l, c);
    build(i*2+1, c+1, r);

    tree[i].maxVal = max(tree[i*2].maxVal, tree[i*2+1].maxVal);
}

void add(int delta, int tl, int tr, int i=1, int l=0, int r=n-1) {
    if(tl == l && tr == r) {
        tree[i].delta += delta;
        return;
    }

    int c = (l+r)/2;

    if(tr <= c) {
        add(delta, tl, tr, i*2, l, c);
    }
    else if(tl > c) {
        add(delta, tl, tr, i*2+1, c+1, r);
    } 
    else {
        add(delta, tl, c, i*2, l, c);
        add(delta, c+1, tr, i*2+1, c+1, r);
    }

    tree[i].maxVal = max(tree[i*2].maxVal + tree[i*2].delta, tree[i*2+1].maxVal + tree[i*2+1].delta);
}

lng getMax(int tl, int tr, int i=1, int l=0, int r=n-1) {
    if(tl == l && tr == r) {
        return tree[i].maxVal + tree[i].delta;
    }

    int c = (l+r)/2;

    if(tr <= c) {
        return tree[i].delta + getMax(tl, tr, i*2, l, c);
    }
    else if(tl > c) {
        return tree[i].delta + getMax(tl, tr, i*2+1, c+1, r);
    }
    else {
        return tree[i].delta + 
            max(getMax(tl, c, i*2, l, c), getMax(c+1, tr, i*2+1, c+1, r));
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
    ios_base::sync_with_stdio(false); cin.tie(0);

    cin >> n >> q;

    forn(i, n) {
        cin >> a[i];
    }

    build();

    forn(i, q) {
        string str;
        int l, r, v;

        cin >> str;

        if(str[0] == 'm') {
            cin >> l >> r;
            cout << getMax(l-1, r-1) << '\n';
        }
        else {
            cin >> l >> r >> v;
            add(v, l-1, r-1);
        }
    }

    return 0;
}