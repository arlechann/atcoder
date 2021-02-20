#include <algorithm>
#include <boost/optional.hpp>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iomanip>
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
#define PRECISION(x) std::fixed << std::setprecision(x)

using namespace std;

using ll = long long;
using VI = std::vector<int>;
using VI2D = std::vector<vector<int>>;
using VLL = std::vector<long long>;
using VLL2D = std::vector<vector<long long>>;

constexpr ll INF = 2e18;
constexpr double EPS = 1e-10;
constexpr double PI = acos(-1.0);

constexpr int dx[] = {-1, 0, 1, 0};
constexpr int dy[] = {0, -1, 0, 1};

template <typename T, std::size_t N>
struct make_vector_type {
	using type =
		typename std::vector<typename make_vector_type<T, (N - 1)>::type>;
};

template <typename T>
struct make_vector_type<T, 0> {
	using type = typename std::vector<T>;
};

template <typename T, size_t N>
auto make_vector_impl(const std::vector<std::size_t>& ls, T init_value) {
	if constexpr(N == 0) {
		return std::vector<T>(ls[N], init_value);
	} else {
		return typename make_vector_type<T, N>::type(
			ls[N], make_vector_impl<T, (N - 1)>(ls, init_value));
	}
}

template <typename T, std::size_t N>
auto make_vector(const std::size_t (&ls)[N], T init_value) {
	std::vector<std::size_t> dimensions(N);
	for(int i = 0; i < N; i++) {
		dimensions[N - i - 1] = ls[i];
	}
	return make_vector_impl<T, N - 1>(dimensions, init_value);
}

template <typename T>
std::vector<T> make_vector(std::size_t size, T init_value) {
	return std::vector<T>(size, init_value);
}

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
	m = max<T>(m, x);
}

template <typename T, typename U>
constexpr void chmin(T& m, U x) {
	m = min<T>(m, x);
}

template <typename T>
constexpr T square(T x) {
	return x * x;
}

using Weight = ll;

// 辺
struct Edge {
	size_t from;
	size_t to;
	Weight cost;
	ll interval;
	Edge(size_t f, size_t t, Weight c, ll i)
		: from(f), to(t), cost(c), interval(i) {}
	bool operator<(const Edge& rhs) const { return this->cost > rhs.cost; }
};

// グラフ G=(V,E)
struct Graph {
	size_t node;
	std::vector<std::vector<Edge>> edges;

	Graph(size_t n) : node(n), edges(n) {}
	void link(size_t from, size_t to, Weight cost, ll interval) {
		edges[from].push_back(Edge(from, to, cost, interval));
		edges[to].push_back(Edge(to, from, cost, interval));
	}
};

// 最短経路探索(非負閉路)
std::vector<Weight> dijkstra(const Graph& graph, const size_t s) {
	size_t n = graph.node;
	std::vector<bool> used(n, false);
	std::vector<Weight> distances(n, INF);

	distances[s] = 0;
	std::priority_queue<Edge> pq;
	pq.push(Edge(0, s, 0, 1));
	while(!pq.empty()) {
		Edge edge = pq.top();
		pq.pop();
		if(used[edge.to]) {
			continue;
		}
		used[edge.to] = true;
		for(auto&& e : graph.edges[edge.to]) {
			Weight alt =
				((edge.cost + e.interval - 1) / e.interval) * e.interval +
				e.cost;
			if(alt < distances[e.to]) {
				distances[e.to] = alt;
				pq.push(Edge(e.from, e.to, alt, e.interval));
			}
		}
	}
	return distances;
}

int main() {
	int n, m, x, y;
	cin >> n >> m >> x >> y;
	x--;
	y--;
	VI a(m), b(m);
	VLL t(m), k(m);
	REP(i, m) {
		cin >> a[i] >> b[i] >> t[i] >> k[i];
		a[i]--;
		b[i]--;
	}

	auto graph = Graph(n);
	REP(i, m) { graph.link(a[i], b[i], t[i], k[i]); }

	auto distances = dijkstra(graph, x);

	cout << (distances[y] == INF ? -1 : distances[y]) << endl;
	return 0;
}
