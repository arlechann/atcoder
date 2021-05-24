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

int h, w;
vector<string> a;
vector<vector<vector<int>>> memo;

int dp(int y, int x, int turn) {
	int& ret = memo[y][x][turn];
	if(turn == 0 && ret != -INF) {
		return ret;
	}
	if(turn == 1 && ret != INF) {
		return ret;
	}

	if(y == h - 1 && x == w - 1) {
		return ret = 0;
	}

	if(y != h - 1) {
		int ny = y + 1, nx = x;
		if(turn == 0) {
			if(a[ny][nx] == '+') {
				chmax(ret, dp(ny, nx, !turn) + 1);
			} else {
				chmax(ret, dp(ny, nx, !turn) - 1);
			}
		} else {
			if(a[ny][nx] == '+') {
				chmin(ret, dp(ny, nx, !turn) - 1);
			} else {
				chmin(ret, dp(ny, nx, !turn) + 1);
			}
		}
	}
	if(x != w - 1) {
		int ny = y, nx = x + 1;
		if(turn == 0) {
			if(a[ny][nx] == '+') {
				chmax(ret, dp(ny, nx, !turn) + 1);
			} else {
				chmax(ret, dp(ny, nx, !turn) - 1);
			}
		} else {
			if(a[ny][nx] == '+') {
				chmin(ret, dp(ny, nx, !turn) - 1);
			} else {
				chmin(ret, dp(ny, nx, !turn) + 1);
			}
		}
	}
	return ret;
}

int main() {
	cin >> h >> w;
	a = vector<string>(h);
	REP(i, h) { cin >> a[i]; }

	memo = make_vector({h, w, 2}, 0);
	REP(i, h) {
		REP(j, w) {
			memo[i][j][0] = -INF;
			memo[i][j][1] = INF;
		}
	}
	int result = dp(0, 0, 0);

	// REP(i, h) {
	// 	REP(j, w) {
	// 		cout << (memo[i][j][0] == -INF ? "/"s : to_string(memo[i][j][0]))
	// 			 << " ";
	// 	}
	// 	cout << endl;
	// }
	// cout << endl;
	// REP(i, h) {
	// 	REP(j, w) {
	// 		cout << (memo[i][j][1] == INF ? "/"s : to_string(memo[i][j][1]))
	// 			 << " ";
	// 	}
	// 	cout << endl;
	// }
	// cout << endl;

	if(result == 0) {
		cout << "Draw" << endl;
	} else if(result > 0) {
		cout << "Takahashi" << endl;
	} else {
		cout << "Aoki" << endl;
	}
	return 0;
}