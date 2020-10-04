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

constexpr int INF = 2e9;
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
	void link(size_t from, size_t to, Weight cost = 1) {
		edges[from].push_back(Edge(from, to, cost));
	}
	void unordered_link(size_t a, size_t b, Weight cost = 1) {
		this->link(a, b, cost);
		this->link(b, a, cost);
	}
};

// 幅優先探索(重み無しグラフ)
std::vector<Weight> bfs(const Graph& graph, const size_t s) {
	size_t n = graph.node;
	std::vector<bool> used(n, false);
	std::vector<Weight> distances(n, INF);
	VI prev(n, -1);

	distances[s] = 0;
	used[s] = true;
	std::queue<Edge> q;
	q.push(Edge(s, s, 0));
	while(!q.empty()) {
		Edge edge = q.front();
		q.pop();
		prev[edge.to] = edge.from;
		for(auto&& e : graph.edges[edge.to]) {
			if(used[e.to]) {
				continue;
			}
			distances[e.to] = distances[e.from] + 1;
			q.push(Edge(e.from, e.to, 1));
			used[e.to] = true;
		}
	}
	VI path;
	int i = 0;
	path.push_back(i);
	while(i != s) {
		i = prev[i];
		path.push_back(i);
	}
	return path;
}

int solve(Graph& g, VI& board, int color, int node, int parent) {
	int ret = 1;
	EACH(e, g.edges[node]) {
		if((board[e.to] == color || board[e.to] == 0) && e.to != parent) {
			board[e.to] = color;
			ret += solve(g, board, color, e.to, node);
		}
	}
	return ret;
}

int main() {
	int n;
	cin >> n;
	VI a(n - 1), b(n - 1);
	REP(i, n - 1) {
		cin >> a[i] >> b[i];
		a[i]--;
		b[i]--;
	}

	Graph g(n);
	REP(i, n - 1) { g.unordered_link(a[i], b[i]); }
	auto path = bfs(g, n - 1);

	VI board(n, 0);
	int f_i = 0;
	int s_i = path.size() - 1;
	while(true) {
		if(f_i > s_i) {
			break;
		}
		board[path[f_i]] = 1;
		if(f_i == s_i) {
			break;
		}
		board[path[s_i]] = -1;
		f_i++;
		s_i--;
	}
	int fennec = solve(g, board, 1, 0, 0);
	int snuke = solve(g, board, -1, n - 1, n - 1);

	cout << (fennec > snuke ? "Fennec" : "Snuke") << endl;
	return 0;
}
