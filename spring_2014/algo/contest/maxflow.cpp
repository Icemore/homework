#define _CRT_SECURE_NO_DEPRECATE
#define _SCL_SECURE_NO_DEPRECATE
#include <iostream>
#include <vector>
#include <complex>
#include <algorithm>
#include <cmath>
#include <numeric>

using namespace std;

#define forn(i, n) for(int i = 0; i <(n); ++i)
#define forv(i, a) forn(i, (int)a.size())
const double eps = 1e-9;
const double pi = 3.1415926535897932384626433832795;
const int inf = 1000 * 1000 * 1000;

struct edge {
	int to;
	int c, f;
	int w;
	size_t back;

	edge(int to, int c, int w, size_t back)
		: to(to), c(c), f(0), w(w), back(back)
	{}

	int maxAdd() {
		return c - f;
	}

	bool full() {
		return maxAdd() <= 0;
	}
};

const int maxn = 1000;
vector<edge> grav[maxn];
int s, t;
int n;

void addEdge(int from, int to, int c, int w) {
	size_t bf = grav[to].size();
	size_t bt = grav[from].size();

	grav[from].push_back(edge(to, c, w, bf));
	grav[to].push_back(edge(from, 0, -w, bt));
}

pair<int, int> pred[maxn];
int dist[maxn];
bool findPath() {
	fill(dist, dist + n, inf);
	dist[s] = 0;
	pred[s] = make_pair(-1, -1);

	forn(i, n) {
		forn(from, n){
			forv(j, grav[from]){
				edge & cur = grav[from][j];

				if (cur.full()) continue;

				if (dist[from] + cur.w < dist[cur.to]) {
					dist[cur.to] = dist[from] + cur.w;
					pred[cur.to] = make_pair(from, j);
				}
			}
		}
	}

	return dist[t] != inf;
}

int getFlow() {
	int flow = inf;
	auto p = pred[t];

	while (p.first != -1) {
		edge & e = grav[p.first][p.second];

		flow = min(flow, e.maxAdd());
		p = pred[p.first];
	}

	return flow;
}

void pushFlow(int flow) {
	auto p = pred[t];

	while (p.first != -1) {
		edge & e = grav[p.first][p.second];

		e.f += flow;
		grav[e.to][e.back].f -= flow;

		p = pred[p.first];
	}
}

int maxflow() {
	int flow = 0;

	while (findPath()) {
		int curFlow = getFlow();
		pushFlow(curFlow);
		flow += curFlow;
	}

	return flow;
}

int m;
int w[50];
int r[50];
int a[50][50];

void read() {
	cin >> m;
	forn(i, m) {
		cin >> w[i];
	}
	
	forn(i, m) {
		cin >> r[i];
	}

	forn(i, m) forn(j, m) {
		cin >> a[i][j];
	}
}

bool construct() {
	s = 0;
	t = 1;

	int pshift = 2;
	int tshift = pshift + (m*m);
	n = tshift + m;
	
	int firstWin = w[0] + r[0];

	for (int i = 1; i < m; ++i){
		for (int j = 1; j < i; ++j) {
			int cur = pshift + i*m + j;
			addEdge(s, cur, a[i][j], 1);

			addEdge(cur, tshift + i, inf, 1);
			addEdge(cur, tshift + j, inf, 1);
		}


		if (firstWin - w[i] < 0) return false;
		addEdge(tshift + i, t, firstWin - w[i], 1);
	}

	return true;
}

bool solve() {
	read();

	if (!construct()) return false;

	int need = 0;
	forv(i, grav[s]) {
		need += grav[s][i].c;
	}

	return maxflow() == need;
}

int main() {
#ifdef __ASD__
	freopen("input.txt", "rt", stdin);
	freopen("output.txt", "wt", stdout);
#endif

	
	if (solve()) {
		cout << "YES" << endl;
	}
	else {
		cout << "NO" << endl;
	}

	return 0;
}