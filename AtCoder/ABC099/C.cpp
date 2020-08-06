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

int main() {
	int n;
	cin >> n;
	VI operations;
	operations.push_back(1);
	operations.push_back(6);
	int tmp;
	for(int i = 1; (tmp = *(operations.end() - 1) * 6) < 100001; i++) {
		operations.push_back(tmp);
	}
	operations.push_back(9);
	for(int i = 1; (tmp = *(operations.end() - 1) * 9) < 100001; i++) {
		operations.push_back(tmp);
	}
	sort(ALL(operations));
	int m = operations.size();

	VI2D dp(n + 1, VI(m + 1, INF)); // dp[合計][使える引き出し操作]
	REP(i, m + 1) { dp[0][i] = 0; }
	REP(i, n) {
		REP(j, m) {
			if(i + 1 < operations[j]) {
				dp[i + 1][j + 1] = dp[i + 1][j];
			} else {
				dp[i + 1][j + 1] =
					min(dp[i + 1 - operations[j]][j + 1] + 1, dp[i + 1][j]);
			}
		}
	}

	cout << dp[n][m] << endl;
	return 0;
}
