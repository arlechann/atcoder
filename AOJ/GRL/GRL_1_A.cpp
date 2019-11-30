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

using Distance = int;

struct Edge {
	size_t to;
	Distance cost;
	Edge(size_t t, Distance c) : to(t), cost(c) {}
	bool operator<(const Edge& rhs) const { return this->cost > rhs.cost; }
};

using Graph = std::vector<std::vector<Edge>>;

std::vector<Distance> dijkstra(const Graph& graph, const size_t s) {
	size_t n = graph.size();
	std::vector<bool> used(n, false);
	std::vector<Distance> distances(n, INF);

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
		for(auto&& e : graph[edge.to]) {
			Distance alt = edge.cost + e.cost;
			if(alt < distances[e.to]) {
				distances[e.to] = alt;
				pq.push(Edge(e.to, alt));
			}
		}
	}

	return distances;
}

int main() {
	int v, e, r;
	scanf("%d %d %d", &v, &e, &r);
	Graph graph(v);
	REP(i, e) {
		int s, t, d;
		scanf("%d %d %d", &s, &t, &d);
		graph[s].push_back(Edge(t, d));
	}
	VI dists = dijkstra(graph, r);

	EACH(e, dists) {
		if(e == INF) {
			puts("INF");
		} else {
			printf("%d\n", e);
		}
	}

	return 0;
}