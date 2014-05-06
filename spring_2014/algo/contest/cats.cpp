#define _CRT_SECURE_NO_DEPRECATE
#define _SCL_SECURE_NO_DEPRECATE
#include <iostream>
#include <vector>
#include <complex>
#include <algorithm>
#include <cmath>
#include <numeric>
#include <memory.h>

using namespace std;

#define forn(i, n) for(int i = 0; i <(n); ++i)
#define forv(i, a) forn(i, (int)a.size())
const double eps = 1e-9;
const double pi = 3.1415926535897932384626433832795;
const int inf = 1000 * 1000 * 1000;

struct edge{
	int to;
	bool alive;

	edge(int to)
		: to(to), alive(true)
	{}
};

const int maxn = 100000 + 100;
vector<edge> grav[maxn];
vector<int> topsort;
bool used[maxn];
int comp[maxn];
int compsCnt;
int n, m;

void dfs1(int v) {
	used[v] = true;

	forv(i, grav[v]){
		edge & cur = grav[v][i];

		if (!used[cur.to]) {
			cur.alive = false;
			dfs1(cur.to);
		}
	}

	topsort.push_back(v);
}

void dfs2(int v) {
	comp[v] = compsCnt;

	forv(i, grav[v]){
		edge & cur = grav[v][i];

		if (comp[cur.to] != -1 || !cur.alive) {
			continue;
		}

		dfs2(cur.to);
	}
}

void findComps() {
	memset(used, 0, sizeof(used));
	memset(comp, -1, sizeof(comp));

	dfs1(0);
	reverse(topsort.begin(), topsort.end());

	compsCnt = 0;
	forv(i, topsort) {
		if (comp[topsort[i]] == -1) {
			dfs2(topsort[i]);
			compsCnt++;
		}
	}
}

const int maxl = 18;
int up[maxn][18];
int height[maxn];
vector<int> tree[maxn];
int finish;

void dfs3(int v, int h, int p) {
	height[v] = h;

	up[v][0] = p;
	for (int i = 1; i < maxl; ++i) {
		up[v][i] = up[up[v][i - 1]][i - 1];
	}

	forv(i, tree[v]) {
		int to = tree[v][i];

		if (to != p) {
			dfs3(to, h + 1, v);
		}
	}
}

void buildTree() {
	forn(from, n) {
		forv(i, grav[from]) {
			int to = grav[from][i].to;

			if (comp[to] != comp[from]){
				tree[comp[from]].push_back(comp[to]);
			}
		}
	}

	dfs3(comp[finish], 0, comp[finish]);
}

int jump(int v, int h) {
	for (int i = 0; i < maxl && h; ++i){
		if (h & 1) {
			v = up[v][i];
		}

		h >>= 1;
	}

	return v;
}

void level(int & u, int & v) {
	if (height[u] > height[v]) {
		u = jump(u, height[u] - height[v]);
	}
	else {
		v = jump(v, height[v] - height[u]);
	}
}

int getLCA(int u, int v) {
	level(u, v);

	if (u == v) return v;

	for (int i = maxl - 1; i >= 0; --i) {
		if (up[u][i] != up[v][i]) {
			u = up[u][i];
			v = up[v][i];
		}
	}

	return up[u][0];
}

int main() {
#ifdef __ASD__
	freopen("input.txt", "rt", stdin);
	freopen("output.txt", "wt", stdout);
#endif

	cin >> n >> m;
	cin >> finish;
	--finish;

	forn(i, m){
		int u, v;
		cin >> u >> v;
		--u; --v;

		grav[u].push_back(edge(v));
		grav[v].push_back(edge(u));
	}

	findComps();
	buildTree();

	int k;
	cin >> k;
	forn(i, k) {
		int u, v;
		cin >> u >> v;
		--u; --v;

		int t = getLCA(comp[u], comp[v]);
		cout << height[t] << "\n";
	}

	return 0;
}