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

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

constexpr int INF = 1e9 + 1;
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
	int n, w_max;
	cin >> n >> w_max;
	VI w(n);
	VI v(n);
	REP(i, n) { cin >> w[i] >> v[i]; }
	int sum = 0;
	REP(i, n) { sum += v[i]; }
	VI2D dp(n, VI(sum + 1, INF));
	REP(i, n) {
		REP(j, sum + 1) {
			if(j == 0) {
				dp[i][0] = 0;
				continue;
			}

			if(i == 0) {
				dp[0][j] = j == v[0] ? w[0] : INF;
				continue;
			}

			if(j < v[i]) {
				dp[i][j] = dp[i - 1][j];
			} else {
				dp[i][j] = min(dp[i - 1][j - v[i]] + w[i], dp[i - 1][j]);
			}
		}
	}
	int result;
	int dpmax = 0;
	for(int i = sum; i >= 0; i--) {
		if(dp[n - 1][i] <= w_max) {
			result = i;
			break;
		}
	}
	cout << result << endl;
	return 0;
}
