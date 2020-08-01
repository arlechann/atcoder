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

// 終了判定
template <typename T,
		  typename enable_if<is_integral<T>::value>::type* = nullptr>
bool finds(T left, T right) {
	return right - left > 1;
}

// 二分探索
template <typename T>
T bin_search(T left, T right, auto pred) {
	while(finds<T>(left, right)) {
		T middle = (left + right) / 2;
		if(pred(middle)) {
			left = middle;
		} else {
			right = middle;
		}
	}
	return left;
}

int main() {
	int n;
	cin >> n;
	vector<vector<ll>> abc(3, vector<ll>(n + 1, 0));
	REP(i, n) { cin >> abc[0][i]; }
	REP(i, n) { cin >> abc[1][i]; }
	REP(i, n) { cin >> abc[2][i]; }

	sort(ALL(abc[0]));
	sort(ALL(abc[1]));
	sort(ALL(abc[2]));
	vector<vector<ll>> dp(3, vector<ll>(n + 1, 0));
	REP(i, 3) {
		REP(j, n + 1) {
			if(i == 0) {
				dp[0][j] = j;
				continue;
			}
			if(j == 0) {
				dp[i][0] = 0;
				continue;
			}
			int l = bin_search(
				0, n + 1, [&](int x) { return abc[i - 1][x] < abc[i][j]; });
			dp[i][j] = dp[i][j - 1] + dp[i - 1][l];
		}
	}

	cout << dp[2][n] << endl;
	return 0;
}
