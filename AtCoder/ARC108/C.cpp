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

struct Edge {
	size_t from;
	size_t to;
	int color;
	Edge(size_t f, size_t t, int c) : from(f), to(t), color(c) {}
};

vector<int> solve(vector<vector<Edge>>& edges) {
	int n = edges.size();
	vector<int> used(n, 0);
	vector<int> colors(n, -1);

	colors[0] = 0;
	used[0] = 1;
	queue<Edge> q;
	q.push(Edge(0, 0, 0));
	while(!q.empty()) {
		Edge edge = q.front();
		q.pop();
		for(auto&& e : edges[edge.to]) {
			if(used[e.to]) {
				continue;
			}
			if(colors[e.from] == e.color) {
				colors[e.to] = (e.color + 1 % n);
			} else {
				colors[e.to] = e.color;
			}
			q.push(e);
			used[e.to] = true;
		}
	}
	return colors;
}

int main() {
	int n, m;
	cin >> n >> m;
	VI u(m), v(m), c(m);
	REP(i, m) {
		cin >> u[i] >> v[i] >> c[i];
		u[i]--;
		v[i]--;
		c[i]--;
	}

	vector<vector<Edge>> edges(n);
	REP(i, m) {
		edges[u[i]].push_back(Edge(u[i], v[i], c[i]));
		edges[v[i]].push_back(Edge(v[i], u[i], c[i]));
	}
	auto colors = solve(edges);
	EACH(e, colors) { cout << e + 1 << endl; }
	return 0;
}
