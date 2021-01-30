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

#ifndef ATCODER_FENWICKTREE_HPP
#define ATCODER_FENWICKTREE_HPP 1

#include <cassert>
#include <type_traits>
#include <vector>

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

// Reference: https://en.wikipedia.org/wiki/Fenwick_tree
template <class T>
struct fenwick_tree {
	using U = internal::to_unsigned_t<T>;

	public:
	fenwick_tree() : _n(0) {}
	fenwick_tree(int n) : _n(n), data(n) {}

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

#endif // ATCODER_FENWICKTREE_HPP

using namespace atcoder;
int main() {
	int n;
	cin >> n;
	VI a(n);
	EACH(e, a) { cin >> e; }

	auto fw = fenwick_tree<ll>(n + 1);
	ll inv_sum = 0;
	for(int i = n - 1; i >= 0; i--) {
		fw.add(a[i], 1);
		inv_sum += fw.sum(0, a[i]);
	}

	REP(i, n) {
		cout << inv_sum << endl;
		inv_sum -= a[i];
		inv_sum += n - a[i] - 1;
	}
	return 0;
}
