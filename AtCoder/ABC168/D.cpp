#include <algorithm>
#include <boost/optional.hpp>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <numeric>
#include <queue>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < i##_MACRO; i++)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) std::begin(a), std::end(a)
#define RALL(a) std::rbegin(a), std::rend(a)
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define INT(x) (static_cast<int>(x))

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

constexpr int INF = 2e9;
constexpr double EPS = 1e-10;
constexpr double PI = acos(-1.0);

constexpr int dx[] = {-1, 0, 1, 0};
constexpr int dy[] = {0, -1, 0, 1};

template <typename T>
constexpr int sign(T x) {
	return x < 0 ? -1 : x > 0 ? 1 : 0;
}

template <>
constexpr int sign(double x) {
	return x < -EPS ? -1 : x > EPS ? 1 : 0;
}

template <typename T, typename U>
constexpr void chmax(T& m, U x) {
	m = max(m, x);
}

template <typename T, typename U>
constexpr void chmin(T& m, U x) {
	m = min(m, x);
}

template <typename T>
constexpr T square(T x) {
	return x * x;
}

using Weight = int;

// 辺
struct Edge {
	size_t from;
	size_t to;
	Weight cost;
	Edge(size_t t, Weight c) : to(t), cost(c) {}
	Edge(size_t f, size_t t, Weight c) : from(f), to(t), cost(c) {}
	bool operator<(const Edge& rhs) const { return this->cost > rhs.cost; }
};

// グラフ G=(V,E)
struct Graph {
	size_t node;
	std::vector<std::vector<Edge>> edges;

	Graph(size_t n) : node(n), edges(n) {}
};

// 最短経路探索(非負閉路)
std::vector<int> dijkstra(const Graph& graph, const size_t s) {
	size_t n = graph.node;
	std::vector<int> used(n, -1);
	std::vector<Weight> distances(n, INF);

	distances[s] = 0;
	std::priority_queue<Edge> pq;
	pq.push(Edge(0, s, 0));
	while(!pq.empty()) {
		Edge edge = pq.top();
		pq.pop();
		if(used[edge.to] != -1) {
			continue;
		}
		used[edge.to] = edge.from;
		for(auto&& e : graph.edges[edge.to]) {
			Weight alt = edge.cost + e.cost;
			if(alt < distances[e.to]) {
				distances[e.to] = alt;
				pq.push(Edge(edge.to, e.to, alt));
			}
		}
	}
	return used;
}

int main() {
	int n, m;
	cin >> n >> m;
	VI a(m);
	VI b(m);
	Graph g(n);
	REP(i, m) {
		cin >> a[i] >> b[i];
		g.edges[a[i] - 1].push_back(Edge(a[i] - 1, b[i] - 1, 1));
		g.edges[b[i] - 1].push_back(Edge(b[i] - 1, a[i] - 1, 1));
	}
	vector<int> result = dijkstra(g, 0);
	bool is_enable = true;
	REP(i, n) {
		if(result[i] == -1) {
			is_enable = false;
			break;
		}
	}
	if(is_enable) {
		cout << "Yes" << endl;
		RANGE(i, 1, n) { cout << result[i] + 1 << endl; }
	} else {
		cout << "No" << endl;
	}
	return 0;
}
