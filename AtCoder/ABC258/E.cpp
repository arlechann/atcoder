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

template <typename T>
bool finds(T left, T right) {
	return abs(right - left) <= 1;
}

// 二分探索
template <typename T>
T bin_search(T ok, T ng, auto pred) {
	while(!finds<T>(ok, ng)) {
		T middle = (ok + ng) / 2;
		if(pred(middle)) {
			ok = middle;
		} else {
			ng = middle;
		}
	}
	return ok;
}

template <typename T>
class Doubling {
	std::size_t n;
	std::size_t k;
	std::vector<std::vector<T>> doubling;

	public:
	template <typename F>
	Doubling(long long m, std::size_t n, F next)
		: n(n), k(([=]() {
			  std::size_t k = 1;
			  while((1LL << k) < m) {
				  k++;
			  }
			  return k;
		  })()),
		  doubling(k + 1, std::vector<T>(n)) {
		for(int i = 0; i < this->n; i++) {
			this->doubling[0][i] = next(i);
		}
		for(int i = 0; i < this->k; i++) {
			for(int j = 0; j < this->n; j++) {
				this->doubling[i + 1][j] =
					this->doubling[i][this->doubling[i][j]];
			}
		}
	}

	T operator()(std::size_t x, std::size_t p = 0) {
		for(std::size_t i = 0; i < this->k; i++) {
			if(x & (1LL << i)) {
				p = this->doubling[i][p];
			}
		}
		return p;
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

const ll K_MAX = 1000000000000LL;

int main() {
	int n, q;
	ll x;
	cin >> n >> q >> x;
	VLL w(n);
	EACH(e, w) {
		cin >> e;
	}
	VLL k(q);
	EACH(e, k) {
		cin >> e;
	}

	VLL w_cumsum(n + 1, 0);
	REP(i, n) {
		w_cumsum[i + 1] = w_cumsum[i] + w[i];
	}
	ll& w_sum = w_cumsum[n];

	VI next_start(n);
	VLL count_by_start(n);
	REP(i, n) {
		int start = i;
		ll rest_weight = x;
		ll count = 0;

		ll to_end_weight = w_sum - w_cumsum[start];
		if(to_end_weight <= rest_weight) {
			rest_weight -= to_end_weight;
			count += n - start;

			ll full_count = rest_weight / w_sum;
			rest_weight %= w_sum;
			count += full_count * n;

			start = 0;
		}

		int next = bin_search<int>(n - 1, start - 1, [&](int index) {
			return rest_weight <= w_cumsum[index] - w_cumsum[start];
		});
		count += next - start;

		next_start[i] = next;
		count_by_start[i] = count;
	}

	// REP(i, n) {
	// 	cout << i << " =>" << endl;
	// 	cout << "  next: " << next_start[i] << endl;
	// 	cout << "  count: " << count_by_start[i] << endl;
	// }
	Doubling<int> doubling(
		K_MAX + 1, n, [&](int index) { return next_start[index]; });
	VLL result(q);
	REP(i, q) {
		int start = doubling(k[i] - 1);
		// cout << i << ", start => " << start << endl;
		result[i] = count_by_start[start];
	}

	REP(i, q) {
		cout << result[i] << endl;
	}
	return 0;
}
