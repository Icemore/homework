#include <iostream>
#include <vector>
#include <cstdlib>
#include <algorithm>
using namespace std;

vector< vector<int> > graph;
vector<int> topsort;
vector<int> used;

bool dfs(int v)
{
	if(used[v] != 0)
		return used[v] == 2;

	used[v] = 1;

	for(size_t i = 0; i < graph[v].size(); ++i)
	{
		if(!dfs(graph[v][i]))
			return false;
	}

	used[v] = 2;

	topsort.push_back(v);
	return true;
}

int main() 
{
	size_t n, m;
	cin >> n >> m;

	used.assign(n, 0);
	graph.resize(n);

	for(size_t i = 0; i < m; ++i)
	{
		int from, to;
		cin >> from >> to;
		--from; --to;

		graph[from].push_back(to);
	}
	
	for(size_t v = 0; v < n; ++v)
	{
		if(used[v] != 0) continue;
		if(!dfs(v)) break;
	}

	if(topsort.size() < n)
	{
		cout << -1 << endl;
	}
	else
	{
		reverse(topsort.begin(), topsort.end());
		for(size_t i = 0; i < topsort.size(); ++i)
			cout << topsort[i] + 1 << " ";
	}

    return 0;
}