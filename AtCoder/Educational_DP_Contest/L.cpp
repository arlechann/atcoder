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
using VI = vector<int>;
using VI2D = vector<vector<int>>;

constexpr int INF = 2e9;
constexpr double EPS = 1e-10;
constexpr double PI = acos(-1.0);

constexpr int dx[] = {-1, 0, 1, 0};
constexpr int dy[] = {0, -1, 0, 1};

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
	m = max(m, x);
}

template <typename T, typename U>
constexpr void chmin(T& m, U x) {
	m = min(m, x);
}

template <typename T>
constexpr T square(T x) {
	return x * x;
}

inline int opposite(int p) {
	return (p + 1) % 2;
}

int n;
ll a[3001];
ll dp[3001][3001];
int is_used[3001][3001];

ll solve(int l, int r) {
	if(is_used[l][r]) {
		return dp[l][r];
	}

	if(l + r == n) {
		return 0;
	}

	is_used[l][r] = true;

	int x1, x2;
	ll xy;
	if((l + r) % 2 == 0) {
		/* Taro */
		xy = max(solve(l + 1, r) + a[l], solve(l, r + 1) + a[n - (r + 1)]);
	} else {
		/* Jiro */
		xy = min(solve(l + 1, r) - a[l], solve(l, r + 1) - a[n - (r + 1)]);
	}

	return dp[l][r] = xy;
}

int main() {
	cin >> n;
	REP(i, n) { cin >> a[i]; }
	solve(0, 0);
	int result;
	cout << solve(0, 0) << endl;
	return 0;
}
