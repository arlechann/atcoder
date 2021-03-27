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
constexpr ll INFLL = 2e18;
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

enum { MIN, MAX };
enum { DIST, POINT };

ll diff(ll a, ll b) {
	return abs(a - b);
}

int main() {
	int n;
	cin >> n;
	VLL x(n);
	VI c(n);
	REP(i, n) { cin >> x[i] >> c[i]; }

	VI color;
	VLL max_x(n + 1, -INFLL);
	VLL min_x(n + 1, INFLL);
	REP(i, n) {
		color.push_back(c[i]);
		chmax(max_x[c[i]], x[i]);
		chmin(min_x[c[i]], x[i]);
	}
	sort(ALL(color));
	color.erase(unique(ALL(color)), color.end());

	ll now = 0;
	ll size = color.size();
	VLL2D dp(size + 1, VLL(2, INFLL));
	dp[0][MIN] = dp[0][MAX] = 0;
	REP(i, size) {
		ll d = diff(min_x[color[i]], max_x[color[i]]);
		if(i == 0) {
			dp[1][MIN] = min(dp[0][MAX] + abs(min_x[color[i]]),
							 dp[0][MIN] + abs(min_x[color[i]])) +
						 d;
			dp[1][MAX] = min(dp[0][MAX] + abs(max_x[color[i]]),
							 dp[0][MIN] + abs(max_x[color[i]])) +
						 d;
		} else {
			dp[i + 1][MIN] =
				min(dp[i][MAX] + diff(min_x[color[i - 1]], min_x[color[i]]),
					dp[i][MIN] + diff(max_x[color[i - 1]], min_x[color[i]])) +
				d;
			dp[i + 1][MAX] =
				min(dp[i][MAX] + diff(min_x[color[i - 1]], max_x[color[i]]),
					dp[i][MIN] + diff(max_x[color[i - 1]], max_x[color[i]])) +
				d;
		}
	}

	ll result = min(dp[size][MAX] + abs(min_x[color[size - 1]]),
					dp[size][MIN] + abs(max_x[color[size - 1]]));
	cout << result << endl;
	return 0;
}
