#include <algorithm>
#include <cassert>
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
#define CAST(x, t) (static_cast<t>(x))
#define PRECISION(x) std::fixed << std::setprecision(x)

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;
using VLL = vector<long long>;
using VLL2D = vector<vector<long long>>;

constexpr int INF = 2e9;
constexpr long long INFLL = 2e18;
constexpr double EPS = 1e-10;
constexpr double PI = acos(-1.0);

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

template <typename T>
constexpr bool chmax(T& m, T x) {
	if(m >= x) {
		return false;
	}
	m = x;
	return true;
}

template <typename T>
constexpr bool chmin(T& m, T x) {
	if(m <= x) {
		return false;
	}
	m = x;
	return true;
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
constexpr T div_ceil(T a, T b) {
	assert(b != 0);
	if(a < 0 && b < 0) {
		a = -a;
		b = -b;
	}
	if(a >= 0 && b > 0) {
		return (a + b - 1) / b;
	}
	return a / b;
}

template <typename T>
constexpr T div_floor(T a, T b) {
	assert(b != 0);
	if(a < 0 && b < 0) {
		a = -a;
		b = -b;
	}
	if(a >= 0 && b > 0) {
		return a / b;
	}
	assert(false);
}

template <typename T>
constexpr bool is_power_of_two(T n) {
	if constexpr(n == std::numeric_limits<T>::min()) {
		return true;
	}
	return (n & (n - 1)) == 0;
}

constexpr std::size_t next_power_of_two(std::size_t n) {
	if((n & (n - 1)) == 0) {
		return n;
	}
	std::size_t ret = 1;
	while(n != 0) {
		ret <<= 1;
		n >>= 1;
	}
	return ret;
}

template <typename T>
constexpr T next_multiple_of(T a, T b) {
	return div_ceil(a, b) * b;
}

template <typename T>
constexpr bool is_mul_overflow(T a, T b) {
	if(a >= 0 && b >= 0) {
		return a > std::numeric_limits<T>::max() / b;
	}
	if(a <= 0 && b < 0) {
		return a < div_ceil(std::numeric_limits<T>::max(), b);
	}
	if(a < 0) {
		return a > std::numeric_limits<T>::min() / b;
	}
	if(b < 0) {
		return a < div_ceil(std::numeric_limits<T>::max(), b);
	}
}

template <typename T>
constexpr T diff(T a, T b) {
	return max(a, b) - min(a, b);
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

/**
 *  _       _                     _        ____
 * (_)_ __ | |_   _ __ ___   __ _(_)_ __  / /\ \ _
 * | | '_ \| __| | '_ ` _ \ / _` | | '_ \| |  | (_)
 * | | | | | |_  | | | | | | (_| | | | | | |  | |_
 * |_|_| |_|\__| |_| |_| |_|\__,_|_|_| |_| |  | ( )
 *                                        \_\/_/|/
 */

bool find_loop(VI2D& edges, VI& used, VI& loop_list, int now, int prev) {
	if(used[now] != 0) {
		loop_list.push_back(now);
		return true;
	}
	used[now] = 1;

	EACH(next, edges[now]) {
		if(next == prev) {
			continue;
		}
		bool is_found = find_loop(edges, used, loop_list, next, now);
		if(is_found) {
			loop_list.push_back(now);
			return true;
		}
	}
	return false;
}

int main() {
	int n;
	cin >> n;
	VI u(n), v(n);
	REP(i, n) {
		cin >> u[i] >> v[i];
		u[i]--;
		v[i]--;
	}
	int q;
	cin >> q;
	VI x(q), y(q);
	REP(i, q) {
		cin >> x[i] >> y[i];
		x[i]--;
		y[i]--;
	}

	VI2D edges(n);
	REP(i, n) {
		edges[u[i]].push_back(v[i]);
		edges[v[i]].push_back(u[i]);
	}

	VI find_loop_used(n, 0);
	VI loop_nodes;
	find_loop(edges, find_loop_used, loop_nodes, 0, 0);
	RREP(i, loop_nodes.size()) {
		if(loop_nodes[i] == loop_nodes[0]) {
			loop_nodes.pop_back();
			break;
		}
		loop_nodes.pop_back();
	}

	VI in_loop(n, 0);
	REP(i, loop_nodes.size()) {
		in_loop[loop_nodes[i]] = 1;
	}

	UnionFind uf(n);
	REP(i, n) {
		if(in_loop[u[i]] != 0 && in_loop[v[i]] != 0) {
			continue;
		}
		uf.merge(u[i], v[i]);
	}

	REP(i, q) {
		if(uf.is_same(x[i], y[i])) {
			cout << "Yes" << endl;
		} else {
			cout << "No" << endl;
		}
	}

	return 0;
}
