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

namespace atcoder {

namespace internal {

#ifndef _MSC_VER
template <class T>
using is_signed_int128 =
	typename std::conditional<std::is_same<T, __int128_t>::value ||
								  std::is_same<T, __int128>::value,
							  std::true_type,
							  std::false_type>::type;

template <class T>
using is_unsigned_int128 =
	typename std::conditional<std::is_same<T, __uint128_t>::value ||
								  std::is_same<T, unsigned __int128>::value,
							  std::true_type,
							  std::false_type>::type;

template <class T>
using make_unsigned_int128 =
	typename std::conditional<std::is_same<T, __int128_t>::value,
							  __uint128_t,
							  unsigned __int128>;

template <class T>
using is_integral = typename std::conditional<std::is_integral<T>::value ||
												  is_signed_int128<T>::value ||
												  is_unsigned_int128<T>::value,
											  std::true_type,
											  std::false_type>::type;

template <class T>
using is_signed_int = typename std::conditional<(is_integral<T>::value &&
												 std::is_signed<T>::value) ||
													is_signed_int128<T>::value,
												std::true_type,
												std::false_type>::type;

template <class T>
using is_unsigned_int =
	typename std::conditional<(is_integral<T>::value &&
							   std::is_unsigned<T>::value) ||
								  is_unsigned_int128<T>::value,
							  std::true_type,
							  std::false_type>::type;

template <class T>
using to_unsigned = typename std::conditional<
	is_signed_int128<T>::value,
	make_unsigned_int128<T>,
	typename std::conditional<std::is_signed<T>::value,
							  std::make_unsigned<T>,
							  std::common_type<T>>::type>::type;

#else

template <class T>
using is_integral = typename std::is_integral<T>;

template <class T>
using is_signed_int =
	typename std::conditional<is_integral<T>::value && std::is_signed<T>::value,
							  std::true_type,
							  std::false_type>::type;

template <class T>
using is_unsigned_int =
	typename std::conditional<is_integral<T>::value &&
								  std::is_unsigned<T>::value,
							  std::true_type,
							  std::false_type>::type;

template <class T>
using to_unsigned = typename std::conditional<is_signed_int<T>::value,
											  std::make_unsigned<T>,
											  std::common_type<T>>::type;

#endif

template <class T>
using is_signed_int_t = std::enable_if_t<is_signed_int<T>::value>;

template <class T>
using is_unsigned_int_t = std::enable_if_t<is_unsigned_int<T>::value>;

template <class T>
using to_unsigned_t = typename to_unsigned<T>::type;

} // namespace internal

} // namespace atcoder

namespace atcoder {

// Reference: https://en.wikipedia.org/wiki/Fenwick_tree
template <class T>
struct fenwick_tree {
	using U = internal::to_unsigned_t<T>;

	public:
	fenwick_tree() : _n(0) {}
	explicit fenwick_tree(int n) : _n(n), data(n) {}

	void add(int p, T x) {
		assert(0 <= p && p < _n);
		p++;
		while(p <= _n) {
			data[p - 1] += U(x);
			p += p & -p;
		}
	}

	T sum(int l, int r) {
		assert(0 <= l && l <= r && r <= _n);
		return sum(r) - sum(l);
	}

	private:
	int _n;
	std::vector<U> data;

	U sum(int r) {
		U s = 0;
		while(r > 0) {
			s += data[r - 1];
			r -= r & -r;
		}
		return s;
	}
};

} // namespace atcoder

/**
 *  _       _                     _        ____
 * (_)_ __ | |_   _ __ ___   __ _(_)_ __  / /\ \ _
 * | | '_ \| __| | '_ ` _ \ / _` | | '_ \| |  | (_)
 * | | | | | |_  | | | | | | (_| | | | | | |  | |_
 * |_|_| |_|\__| |_| |_| |_|\__,_|_|_| |_| |  | ( )
 *                                        \_\/_/|/
 */

using namespace atcoder;

int main() {
	int n;
	cin >> n;
	VI c(n), x(n);
	EACH(e, c) {
		cin >> e;
		e--;
	}
	EACH(e, x) {
		cin >> e;
		e--;
	}

	ll all_inv = 0;
	fenwick_tree<ll> fw(n + 1);
	REP(i, n) {
		ll sum = fw.sum(x[i] + 1, n + 1);
		// cout << "sum: " << sum << endl;
		all_inv += sum;
		fw.add(x[i], 1);
	}

	// cout << all_inv << endl;
	VI2D colors(n);
	REP(i, n) {
		colors[c[i]].push_back(x[i]);
	}

	fw = fenwick_tree<ll>(n + 1);
	REP(i, n) {
		int size = colors[i].size();
		if(size == 0) {
			continue;
		}
		ll inv = 0;
		REP(j, size) {
			inv += fw.sum(colors[i][j] + 1, n + 1);
			fw.add(colors[i][j], 1);
		}
		all_inv -= inv;

		REP(j, size) {
			fw.add(colors[i][j], -1);
		}
	}

	ll result = all_inv;
	cout << result << endl;
	return 0;
}
