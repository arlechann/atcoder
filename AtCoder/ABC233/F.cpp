#include <algorithm>
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
#define RREP(i, n) for(int i = (n)-1; i >= 0; i--)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define RRANGE(i, a, b) for(int i = (b)-1, i##_MACRO = (a); i >= i##_MACRO; i--)
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
constexpr long long INFLL = 2e18;
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
constexpr bool chmax(T& m, U x) {
	m = max<T>(m, x);
	return m < x;
}

template <typename T, typename U>
constexpr bool chmin(T& m, U x) {
	m = min<T>(m, x);
	return m > x;
}

template <typename T>
constexpr T square(T x) {
	return x * x;
}

template <typename T>
constexpr T pow(T a, int n) {
	T ret = 1;
	while(n != 0) {
		if(n % 2) {
			ret *= a;
		}
		a *= a;
		n /= 2;
	}
	return ret;
}

template <typename T>
constexpr T diff(T a, T b) {
	return abs(a - b);
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
	unordered_map<int, unordered_map<int, int>> m;

	Graph(size_t n) : node(n), edges(n) {}

	size_t size() const { return node; }

	void link(size_t from, size_t to, int index) {
		edges[from].push_back(Edge(from, to, 1));
		m[min(from, to)][max(from, to)] = index;
	}

	int edge_index(size_t from, size_t to) {
		return m[min(from, to)][max(from, to)];
	}
};

// 幅優先探索(重み無しグラフ)
std::vector<int> bfs(const Graph& graph, const size_t s, const size_t g) {
	size_t n = graph.node;
	std::vector<bool> used(n, false);
	std::vector<Weight> distances(n, INF);

	distances[s] = 0;
	used[s] = true;
	std::queue<Edge> q;
	q.push(Edge(s, s, 0));
	while(!q.empty() && !used[g]) {
		Edge edge = q.front();
		q.pop();
		for(auto&& e : graph.edges[edge.to]) {
			if(used[e.to]) {
				continue;
			}
			distances[e.to] = distances[e.from] + 1;
			q.push(Edge(e.from, e.to, 1));
			used[e.to] = true;
		}
	}

	vector<int> route;
	int now = g;
	while(now != s) {
		route.push_back(now);
		for(auto&& e : graph.edges[now]) {
			if(distances[e.to] == distances[now] - 1) {
				now = e.to;
				break;
			}
		}
	}
	route.push_back(now);
	reverse(ALL(route));
	return route;
}

// トポロジカルソート
std::vector<int> topological_sort(const Graph& graph,
								  std::vector<int> indegrees) {
	int n = graph.size();
	std::vector<int> sorted;
	std::vector<bool> is_used(n, false);
	std::queue<int> q;
	for(int i = 0; i < n; i++) {
		if(indegrees[i] == 1) {
			q.push(i);
		}
	}

	while(!q.empty() && sorted.size() <= n) {
		int node = q.front();
		q.pop();
		sorted.push_back(node);
		is_used[node] = true;
		for(const Edge& e : graph.edges[node]) {
			indegrees[e.to]--;
			if(!is_used[e.to] && indegrees[e.to] == 1) {
				q.push(e.to);
			}
		}
	}
	return sorted;
}

// UnionFind(disjoint set)
class UnionFind {
	std::vector<size_t> parents;
	std::vector<size_t> rank;
	std::vector<size_t> tree_size;

	public:
	UnionFind(size_t size) : parents(size), rank(size, 0), tree_size(size, 1) {
		std::iota(this->parents.begin(), this->parents.end(), 0);
	}

	// 併合
	bool merge(size_t a, size_t b) {
		size_t ar = this->root(a);
		size_t br = this->root(b);
		if(ar == br) {
			return false;
		}
		if(this->rank[ar] < this->rank[br]) {
			std::swap(ar, br);
		}
		if(this->rank[ar] == this->rank[br]) {
			this->rank[ar]++;
		}
		this->tree_size[ar] += this->tree_size[br];
		return this->parents[br] = ar;
	}
	bool unite(size_t a, size_t b) { return this->merge(a, b); }

	// 同集合か判定
	bool is_same(size_t a, size_t b) { return this->root(a) == this->root(b); }
	bool is_union(size_t a, size_t b) { return this->is_same(a, b); }

	// 集合の大きさ
	size_t size(size_t n) { return this->tree_size[this->root(n)]; }

	private:
	size_t root(int n) {
		if(this->parents[n] == n) {
			return n;
		}

		return this->parents[n] = this->root(this->parents[n]);
	}
};

int main() {
	int n;
	cin >> n;
	VI p(n);
	EACH(e, p) {
		cin >> e;
		e--;
	}
	int m;
	cin >> m;
	VI a(m), b(m);
	REP(i, m) {
		cin >> a[i] >> b[i];
		a[i]--;
		b[i]--;
	}

	Graph g(n);
	UnionFind uf(n);
	VI indegrees(n, 0);
	REP(i, m) {
		if(!uf.is_same(a[i], b[i])) {
			uf.merge(a[i], b[i]);
			g.link(a[i], b[i], i);
			g.link(b[i], a[i], i);
			indegrees[a[i]]++;
			indegrees[b[i]]++;
		}
	}

	REP(i, n) {
		if(!uf.is_same(i, p[i])) {
			cout << -1 << endl;
			return 0;
		}
	}

	auto tsorted = topological_sort(g, indegrees);

	VI result;
	REP(i, tsorted.size()) {
		int move_to = tsorted[i];
		int move_from;
		REP(j, n) {
			if(p[j] == move_to) {
				move_from = j;
			}
		}

		VI route = bfs(g, move_from, move_to);
		REP(j, route.size() - 1) {
			swap(p[route[j]], p[route[j + 1]]);
			result.push_back(g.edge_index(route[j], route[j + 1]));
		}
	}

	int k = result.size();
	cout << k << endl;
	REP(i, k) { cout << result[i] + 1 << " "; }
	cout << endl;
	return 0;
}
