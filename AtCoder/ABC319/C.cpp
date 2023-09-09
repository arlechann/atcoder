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

/**
 *  _       _                     _        ____
 * (_)_ __ | |_   _ __ ___   __ _(_)_ __  / /\ \ _
 * | | '_ \| __| | '_ ` _ \ / _` | | '_ \| |  | (_)
 * | | | | | |_  | | | | | | (_| | | | | | |  | |_
 * |_|_| |_|\__| |_| |_| |_|\__,_|_|_| |_| |  | ( )
 *                                        \_\/_/|/
 */

constexpr int fact(int n) {
	int ret = 1;
	REP(i, n) {
		ret *= (i + 1);
	}
	return ret;
}

VI2D c;
int used[3][3];

bool check(int row, int col) {
	int used_count = 0;
	REP(i, 3) {
		used_count += used[i][col];
	}
	if(used_count == 2) {
		int first = -1;
		REP(i, 3) {
			if(used[i][col] == 0) {
				continue;
			}
			if(first == c[i][col]) {
				return false;
			}
			first = c[i][col];
		}
	}

	used_count = 0;
	REP(j, 3) {
		used_count += used[row][j];
	}
	if(used_count == 2) {
		int first = -1;
		REP(j, 3) {
			if(used[row][j] == 0) {
				continue;
			}
			if(first == c[row][j]) {
				return false;
			}
			first = c[row][j];
		}
	}

	if((row == 1 || col == 1) && !(row == 1 && col == 1)) {
		return true;
	}

	used_count = 0;
	REP(i, 3) {
		used_count += used[i][i];
	}
	if(used_count == 2) {
		int first = -1;
		REP(i, 3) {
			if(used[i][i] == 0) {
				continue;
			}
			if(first == c[i][i]) {
				return false;
			}
			first = c[i][i];
		}
	}

	used_count = 0;
	REP(i, 3) {
		used_count += used[i][2 - i];
	}
	if(used_count == 2) {
		int first = -1;
		REP(i, 3) {
			if(used[i][2 - i] == 0) {
				continue;
			}
			if(first == c[i][2 - i]) {
				return false;
			}
			first = c[i][2 - i];
		}
	}

	return true;
}

int dfs(int k, int row, int col) {
	used[row][col] = 1;

	if(!check(row, col)) {
		used[row][col] = 0;
		return 0;
	}
	if(k == 8) {
		used[row][col] = 0;
		return 1;
	}

	int ret = 0;
	REP(i, 3) {
		REP(j, 3) {
			if(used[i][j] != 0) {
				continue;
			}
			ret += dfs(k + 1, i, j);
		}
	}
	used[row][col] = 0;
	return ret;
}

int main() {
	c = VI2D(3, VI(3));
	REP(i, 3) {
		REP(j, 3) {
			cin >> c[i][j];
		}
	}

	int all = fact(9);
	int result = 0;
	REP(i, 3) {
		REP(j, 3) {
			result += dfs(0, i, j);
		}
	}

	cout << PRECISION(16)
		 << static_cast<double>(result) / static_cast<double>(all) << endl;
	return 0;
}
