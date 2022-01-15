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

struct Edge {
	size_t id;
	size_t from;
	size_t to;
	ll cost;
	Edge() {}
	Edge(size_t id, size_t f, size_t t, ll c)
		: id(id), from(f), to(t), cost(c) {}
	bool operator>(const Edge& rhs) const { return this->cost > rhs.cost; }
	bool operator<(const Edge& rhs) const { return this->cost < rhs.cost; }
};

bool add_edge(UnionFind uf, Edge edge) {
	if(uf.is_same(edge.from, edge.to)) {
		return false;
	}
	uf.merge(edge.from, edge.to);
	return true;
}

int main() {
	int n, m, q;
	cin >> n >> m >> q;
	VI a(m), b(m);
	VLL c(m);
	REP(i, m) {
		cin >> a[i] >> b[i] >> c[i];
		a[i]--;
		b[i]--;
	}
	VI u(q), v(q);
	VLL w(q);
	REP(i, q) {
		cin >> u[i] >> v[i] >> w[i];
		u[i]--;
		v[i]--;
	}

	vector<Edge> base_edges(m);
	REP(i, m) { base_edges[i] = Edge(i, a[i], b[i], c[i]); }
	sort(ALL(base_edges));

	vector<Edge> additional_edges(q);
	REP(i, q) { additional_edges[i] = Edge(i, u[i], v[i], w[i]); }
	sort(ALL(additional_edges));

	UnionFind uf(n);
	VI result(q, 0);
	int aindex = 0;
	REP(i, m) {
		while(aindex < q &&
			  additional_edges[aindex].cost < base_edges[i].cost) {
			bool is_used = !uf.is_same(additional_edges[aindex].from,
									   additional_edges[aindex].to);
			if(is_used) {
				result[additional_edges[aindex].id] = 1;
			}
			aindex++;
		}
		uf.merge(base_edges[i].from, base_edges[i].to);
	}

	REP(i, q) { cout << (result[i] == 0 ? "No" : "Yes") << endl; }
	return 0;
}
