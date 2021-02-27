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
constexpr long long LLINF = 2e18;
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

// 最大公約数を返す
constexpr long long gcd(long long a, long long b) {
	if(a < b) {
		std::swap(a, b);
	}

	long long r = a % b;
	while(r != 0) {
		a = b;
		b = r;
		r = a % b;
	}

	return b;
}

// 拡張ユークリッド互除法
tuple<long long, long long, long long> ext_gcd(long long a, long long b) {
	if(b == 0) {
		return std::make_tuple(1, 0, a);
	}
	auto [y, x, d] = ext_gcd(b, a % b);
	return std::make_tuple(x, y - a / b * x, d);
}

ll solve_bezout_identity(ll a, ll b, ll d) {
	if(a < 0) {
		a = -a;
		d = -d;
	}
	b = abs(b);
	auto [x, y, c] = ext_gcd(a, b);
	x *= d / c;
	ll bdc = b / c;
	if(x < 0) {
		x += ((-x + bdc - 1) / bdc) * bdc;
	} else {
		x %= bdc;
	}
	return x;
}

void test_case(int t) {
	ll x, y, p, q;
	cin >> x >> y >> p >> q;

	ll result = LLINF;
	ll a = p + q, b = -2 * (x + y);
	ll gcd_ab = gcd(a, b);
	REP(Q, q) {
		REP(Y, y) {
			ll c = x + Y - p - Q;
			if(c % gcd_ab != 0) {
				continue;
			}

			ll r = solve_bezout_identity(a, b, c);
			ll time = r * (p + q) + p + Q;
			chmin(result, time);
		}
	}

	if(result == LLINF) {
		cout << "infinity" << endl;
	} else {
		cout << result << endl;
	}
	return;
}

int main() {
	int t;
	cin >> t;
	REP(i, t) { test_case(i); }
	return 0;
}
