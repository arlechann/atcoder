#include <algorithm>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iostream>
#include <limits>
#include <list>
#include <numeric>
#include <queue>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < i##_MACRO; i++)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) (a).begin(), (a).end()
#define AALL(a, n) (a), ((a) + (n))
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define MODNUM (static_cast<int>(1e9 + 7))
#define MOD(x) ((x) % MODNUM)

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

const int INF = 2e9;
const double EPS = 1e-10;
const double PI = acos(-1.0);

const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, -1, 0, 1};

inline int toInt(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

using Weight = int;

struct Edge {
	size_t from;
	size_t to;
	Weight cost;
	Edge(size_t t, Weight c) : to(t), cost(c) {}
	Edge(size_t f, size_t t, Weight c) : from(f), to(t), cost(c) {}
	bool operator<(const Edge& rhs) const { return this->cost > rhs.cost; }
};

struct Graph {
	size_t node;
	std::vector<std::vector<Edge>> edges;

	Graph(size_t n) : node(n), edges(n) {}
};

std::vector<Weight> dijkstra(const Graph& graph, const size_t s) {
	size_t n = graph.node;
	std::vector<bool> used(n, false);
	std::vector<Weight> distances(n, INF);

	distances[s] = 0;
	std::priority_queue<Edge> pq;
	pq.push(Edge(s, 0));
	while(!pq.empty()) {
		Edge edge = pq.top();
		pq.pop();
		if(used[edge.to]) {
			continue;
		}
		used[edge.to] = true;
		for(auto&& e : graph.edges[edge.to]) {
			Weight alt = edge.cost + e.cost;
			if(alt < distances[e.to]) {
				distances[e.to] = alt;
				pq.push(Edge(e.to, alt));
			}
		}
	}

	return distances;
}

// dijkstra ここまで

std::pair<Weight, Graph> prim(const Graph& graph, size_t s) {
	size_t n = graph.node;
	std::vector<bool> used(n, false);
	Weight total = 0;
	Graph mst(n);

	std::priority_queue<Edge> pq;
	pq.push(Edge(-1, s, 0));
	while(!pq.empty()) {
		Edge edge = pq.top();
		pq.pop();
		if(used[edge.to]) {
			continue;
		}
		used[edge.to] = true;
		total += edge.cost;
		if(edge.from != -1) {
			mst.edges[edge.from].push_back(edge);
		}
		for(auto&& e : graph.edges[edge.to]) {
			if(!used[e.to]) {
				pq.push(e);
			}
		}
	}

	return std::pair<Weight, Graph>(total, mst);
}

int main() {
	int v, e;
	scanf("%d %d", &v, &e);
	int s, t, w;
	Graph graph(v);
	REP(i, e) {
		scanf("%d %d %d", &s, &t, &w);
		graph.edges[s].push_back(Edge(s, t, w));
		graph.edges[t].push_back(Edge(t, s, w));
	}

	pair<Weight, Graph> mst = prim(graph, 0);
	printf("%d\n", mst.first);
	return 0;
}