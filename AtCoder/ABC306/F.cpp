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

int roundup_pow2(int n) {
	if(!(n & (n - 1))) {
		return n;
	}

	int i = 1;
	while((n >> i) != 0) {
		i++;
	}
	return 1 << i;
}

// セグメント木(一点更新、区間取得)
template <typename T>
class SegmentTree {
	using F = function<T(T, T)>;

	// 演算
	F merge;
	// 単位元
	T identity;
	vector<T> tree;
	size_t size;

	public:
	SegmentTree(const vector<T>& a, const F f, const T id)
		: tree(roundup_pow2(a.size()) * 2 - 1, id),
		  size(roundup_pow2(a.size())), merge(f), identity(id) {
		int offset = this->size - 1;
		for(int i = 0; i < a.size(); i++) {
			this->tree[i + offset] = a[i];
		}
		for(int i = offset - 1; i >= 0; i--) {
			this->tree[i] = this->apply(i);
		}
	}
	// モノイド(Z,+)
	SegmentTree(const vector<T> a)
		: SegmentTree(
			  a, [](T a, T b) { return a + b; }, 0) {}

	// 更新
	// 関数の指定がなければ置き換え
	void update(
		const size_t index, const T value, const F f = [](T a, T b) {
			return b;
		}) {
		size_t i = index + size - 1;
		this->tree[i] = f(this->tree[i], value);
		while(i > 0) {
			i = (i - 1) / 2;
			this->tree[i] = this->apply(i);
		}
	}

	// 一点取得
	T find(const size_t index) { return this->tree[index + size - 1]; }

	// 区間取得
	T find(const size_t query_left, const size_t query_right) const {
		return this->find_impl(query_left, query_right, 0, 0, this->size);
	}

	private:
	T apply(size_t index) const {
		return this->merge(this->tree[index * 2 + 1],
						   this->tree[index * 2 + 2]);
	}

	T find_impl(size_t query_left,
				size_t query_right,
				size_t node,
				size_t node_left,
				size_t node_right) const {
		if(node_right <= query_left || query_right <= node_left) {
			return this->identity;
		}
		if(query_left <= node_left && node_right <= query_right) {
			return this->tree[node];
		}

		return this->merge(find_impl(query_left,
									 query_right,
									 node * 2 + 1,
									 node_left,
									 node_left + (node_right - node_left) / 2),
						   find_impl(query_left,
									 query_right,
									 node * 2 + 2,
									 node_left + (node_right - node_left) / 2,
									 node_right));
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

const int A_MAX = 1000000;

int main() {
	int n, m;
	cin >> n >> m;
	VLL2D a(n, VLL(m));
	REP(i, n) {
		REP(j, m) {
			cin >> a[i][j];
		}
	}

	REP(i, n) {
		sort(ALL(a[i]));
	}

	VLL values(n * m);
	REP(i, n) {
		REP(j, m) {
			values[i * m + j] = a[i][j];
		}
	}
	sort(ALL(values));
	unordered_map<ll, int> dict;
	REP(i, n * m) {
		dict[values[i]] = i;
	}
	REP(i, n) {
		REP(j, m) {
			a[i][j] = dict[a[i][j]];
		}
	}

	SegmentTree<int> st(VI(n * m, 0));
	ll result = 0;
	REP(i, n) {
		REP(j, m) {
			result += i * (j + 1);
			result += st.find(0, a[n - i - 1][j]);
		}
		REP(j, m) {
			st.update(a[n - i - 1][j], 1);
		}
	}

	cout << result << endl;
	return 0;
}
