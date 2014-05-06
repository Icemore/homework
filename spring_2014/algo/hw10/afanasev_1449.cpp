// problem 1449 run 5648495

#define _CRT_SECURE_NO_DEPRECATE
#define _SCL_SECURE_NO_DEPRECATE
#include <iostream>
#include <vector>
#include <complex>
#include <algorithm>
#include <cmath>
#include <numeric>
#include <memory.h>
#include <queue>

using namespace std;

#define forn(i, n) for(int i = 0; i <(n); ++i)
#define forv(i, a) forn(i, (int)a.size())
const double eps = 1e-9;
const double pi = 3.1415926535897932384626433832795;
const int inf = 1000 * 1000 * 1000;

struct edge{
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

const int maxn = 500;
vector<edge> grav[maxn];
int s, t;
int n;

void addEdge(int from, int to, int c, int w) {
	size_t bf = grav[to].size();
	size_t bt = grav[from].size();

	w = -w;

	grav[from].push_back(edge(to, c, w, bf));
	grav[to].push_back(edge(from, 0, -w, bt));
}

pair<int, int> pred[maxn];
int dist[maxn];
int mark[maxn];

bool findPath() {
	fill(dist, dist + n, inf);
	fill(mark, mark + n, 0);
	dist[s] = 0;

	deque<int> q;
	q.push_back(s);
	mark[s] = 1;
	pred[s] = make_pair(-1, -1);

	while (!q.empty()) {
		int from = q.front();
		q.pop_front();

		mark[from] = 2;

		forv(i, grav[from]) {
			edge & cur = grav[from][i];

			if (cur.full()) continue;

			int to = cur.to;
			int len = cur.w;

			if (dist[to] > dist[from] + len) {
				dist[to] = dist[from] + len;

				if (mark[to] == 0) {
					q.push_back(to);
				}
				else if (mark[to] == 2) {
					q.push_front(to);
				}

				pred[to] = make_pair(from, i);
				mark[to] = 1;
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
int a[maxn][maxn];
int b[maxn][maxn];

void constructFlow() {
	s = 0;
	t = 2 * m + 1;
	n = t + 1;

	forn(i, m){
		addEdge(s, i + 1, 1, 0);
		addEdge(m + 1 + i, t, 1, 0);
	}

	forn(i, m) forn(j, m){
		addEdge(i + 1, m + 1 + j, 1, a[i][j]);
	}
}

int keyCells[maxn];
int back[maxn];

void findDivision() {
	forn(i, m) {
		forn(j, m) {
			b[keyCells[i]][j] = a[i][j];
		}
	}

	forn(j, m) forn(i, m){
		if (i != j)
			b[i][j] = b[j][j] - b[i][j];
	}

	int n = m + 1;
	forn(i, m) {
		b[m][i] = b[i][i];
		b[i][i] = 0;
		b[i][m] = 0;
	}
	b[m][m] = 0;

	forn(k, n)forn(i, n)forn(j, n) {
		b[i][j] = min(b[i][j], b[i][k] + b[k][j]);
	}
}

int rows[maxn], cols[maxn];

void solve(){
	constructFlow();
	maxflow();

	forn(i, m){
		forv(j, grav[i + 1]){
			edge & cur = grav[i + 1][j];

			if (!cur.full()){
				continue;
			}

			keyCells[i] = cur.to - m - 1;
		}
	}

	forn(i, m) {
		back[keyCells[i]] = i;
	}

	findDivision();
	forn(i, m) {
		rows[back[i]] = b[m][i];
	}
	forn(i, m) {
		cols[keyCells[i]] = a[i][keyCells[i]] - rows[i];
	}
}

int main() {
	cin >> m;
	forn(i, m)forn(j, m){
		cin >> a[i][j];
	}

	solve();

	forn(i, m) {
		cout << rows[i] << " ";
	}
	cout << endl;

	forn(i, m) {
		cout << cols[i] << " ";
	}
	cout << endl;

	return 0;
}