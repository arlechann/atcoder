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

#ifndef ATCODER_SCC_HPP
#define ATCODER_SCC_HPP 1

#include <cassert>

#ifndef ATCODER_INTERNAL_SCC_HPP
#define ATCODER_INTERNAL_SCC_HPP 1

namespace atcoder {
namespace internal {

template <class E>
struct csr {
	std::vector<int> start;
	std::vector<E> elist;
	csr(int n, const std::vector<std::pair<int, E>>& edges)
		: start(n + 1), elist(edges.size()) {
		for(auto e : edges) {
			start[e.first + 1]++;
		}
		for(int i = 1; i <= n; i++) {
			start[i] += start[i - 1];
		}
		auto counter = start;
		for(auto e : edges) {
			elist[counter[e.first]++] = e.second;
		}
	}
};

// Reference:
// R. Tarjan,
// Depth-First Search and Linear Graph Algorithms
struct scc_graph {
	public:
	scc_graph(int n) : _n(n) {}

	int num_vertices() { return _n; }

	void add_edge(int from, int to) { edges.push_back({from, {to}}); }

	// @return pair of (# of scc, scc id)
	std::pair<int, std::vector<int>> scc_ids() {
		auto g = csr<edge>(_n, edges);
		int now_ord = 0, group_num = 0;
		std::vector<int> visited, low(_n), ord(_n, -1), ids(_n);
		visited.reserve(_n);
		auto dfs = [&](auto self, int v) -> void {
			low[v] = ord[v] = now_ord++;
			visited.push_back(v);
			for(int i = g.start[v]; i < g.start[v + 1]; i++) {
				auto to = g.elist[i].to;
				if(ord[to] == -1) {
					self(self, to);
					low[v] = std::min(low[v], low[to]);
				} else {
					low[v] = std::min(low[v], ord[to]);
				}
			}
			if(low[v] == ord[v]) {
				while(true) {
					int u = visited.back();
					visited.pop_back();
					ord[u] = _n;
					ids[u] = group_num;
					if(u == v)
						break;
				}
				group_num++;
			}
		};
		for(int i = 0; i < _n; i++) {
			if(ord[i] == -1)
				dfs(dfs, i);
		}
		for(auto& x : ids) {
			x = group_num - 1 - x;
		}
		return {group_num, ids};
	}

	std::vector<std::vector<int>> scc() {
		auto ids = scc_ids();
		int group_num = ids.first;
		std::vector<int> counts(group_num);
		for(auto x : ids.second)
			counts[x]++;
		std::vector<std::vector<int>> groups(ids.first);
		for(int i = 0; i < group_num; i++) {
			groups[i].reserve(counts[i]);
		}
		for(int i = 0; i < _n; i++) {
			groups[ids.second[i]].push_back(i);
		}
		return groups;
	}

	private:
	int _n;
	struct edge {
		int to;
	};
	std::vector<std::pair<int, edge>> edges;
};

} // namespace internal

} // namespace atcoder

#endif // ATCODER_INTERNAL_SCC_HPP

namespace atcoder {

struct scc_graph {
	public:
	scc_graph() : internal(0) {}
	scc_graph(int n) : internal(n) {}

	void add_edge(int from, int to) {
		int n = internal.num_vertices();
		assert(0 <= from && from < n);
		assert(0 <= to && to < n);
		internal.add_edge(from, to);
	}

	std::vector<std::vector<int>> scc() { return internal.scc(); }

	private:
	internal::scc_graph internal;
};

} // namespace atcoder

#endif // ATCODER_SCC_HPP

using namespace atcoder;

int used[200001];
int find_loop(const VI2D& edges, VI& has_loop, int node) {
	if(used[node] != 0) {
		return has_loop[node];
	}
	if(has_loop[node] != 0) {
		return 1;
	}
	used[node] = 1;
	int ret = 0;
	EACH(next, edges[node]) { ret |= find_loop(edges, has_loop, next); }
	return has_loop[node] = ret;
}

int main() {
	int n, m;
	cin >> n >> m;
	VI u(m), v(m);
	REP(i, m) {
		cin >> u[i] >> v[i];
		u[i]--;
		v[i]--;
	}

	VI2D edges(n);
	scc_graph graph(n);
	REP(i, m) {
		edges[u[i]].push_back(v[i]);
		graph.add_edge(u[i], v[i]);
	}

	vector<vector<int>> scc = graph.scc();

	VI has_loop(n, 0);
	EACH(vs, scc) {
		int size = vs.size();
		if(size == 1) {
			continue;
		}
		EACH(v, vs) { has_loop[v] = 1; }
	}

	int result = 0;
	REP(i, n) {
		if(has_loop[i] != 0) {
			result++;
		} else {
			int f = find_loop(edges, has_loop, i);
			result += f;
		}
	}

	cout << result << endl;
	// REP(i, n) { cout << has_loop[i] << " "; }
	// cout << endl;
	return 0;
}
