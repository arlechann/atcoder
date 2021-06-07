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

// 終了判定
template <typename T,
		  typename enable_if<is_integral<T>::value>::type* = nullptr>
bool finds(T left, T right) {
	return abs(right - left) <= 1;
}

// 二分探索
template <typename T>
T bin_search(T left, T right, auto pred) {
	while(!finds<T>(left, right)) {
		T middle = (left + right) / 2;
		if(pred(middle)) {
			left = middle;
		} else {
			right = middle;
		}
	}
	return left;
}

using Weight = ll;

// 辺
struct Edge {
	size_t from;
	size_t to;
	Weight cost;
	Weight c;
	Weight d;
	Edge(size_t t, Weight c) : to(t), cost(c) {}
	Edge(size_t f, size_t t, Weight c) : from(f), to(t), cost(c) {}
	Edge(size_t f, size_t t, Weight cost, Weight c, Weight d)
		: from(f), to(t), cost(cost), c(c), d(d) {}
	bool operator<(const Edge& rhs) const { return this->cost > rhs.cost; }
};

// グラフ G=(V,E)
struct Graph {
	size_t node;
	std::vector<std::vector<Edge>> edges;

	Graph(size_t n) : node(n), edges(n) {}
	void link(size_t from, size_t to, Weight c, Weight d) {
		edges[from].push_back(Edge(from, to, 1, c, d));
	}
};

// 最短経路探索(非負閉路)
Weight dijkstra(const Graph& graph, const size_t s, const size_t g) {
	size_t n = graph.node;
	std::vector<bool> used(n, false);
	std::vector<Weight> distances(n, INFLL);

	distances[s] = 0LL;
	std::priority_queue<Edge> pq;
	pq.push(Edge(s, 0LL));
	while(!pq.empty()) {
		Edge edge = pq.top();
		pq.pop();
		if(edge.to == g) {
			return distances[g];
		}
		if(used[edge.to]) {
			continue;
		}
		used[edge.to] = true;
		for(auto&& e : graph.edges[edge.to]) {
			Weight now = distances[e.from];
			Weight sqrt_d = max(now, static_cast<ll>(sqrt(e.d)));
			Weight alt = INFLL;
			RANGE(i, -2, 2) {
				Weight t_dep = max(now, sqrt_d + i);
				chmin(alt, t_dep + e.c + e.d / (t_dep + 1));
			}
			if(alt < distances[e.to]) {
				distances[e.to] = alt;
				pq.push(Edge(e.to, alt));
			}
		}
	}
	return distances[g];
}
int main() {
	int n, m;
	cin >> n >> m;
	VI a(m), b(m);
	VLL c(m), d(m);
	REP(i, m) {
		cin >> a[i] >> b[i] >> c[i] >> d[i];
		a[i]--;
		b[i]--;
	}

	Graph g(n);
	REP(i, m) {
		g.link(a[i], b[i], c[i], d[i]);
		g.link(b[i], a[i], c[i], d[i]);
	}

	ll result = dijkstra(g, 0, n - 1);

	if(result == INFLL) {
		cout << -1 << endl;
	} else {
		cout << result << endl;
	}
	return 0;
}
