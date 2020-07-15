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

int main() {
	string s;
	cin >> s;
	int n = s.size();
	int dp[10][2][2][2][2][2] =
		{}; // dp[桁]['3']['5']['7'][n未満][0以外使用したか]
	dp[0][0][0][0][0][0] = 1;
	REP(i, n) {
		REP(j, 2) {
			REP(k, 2) {
				REP(l, 2) {
					REP(m, 2) {
						REP(o, 2) {
							for(int x = 0; x <= (m ? 9 : (s[i] - '0')); x++) {
								int m2 = 1;
								int o2 = 1;
								if(!m && x == s[i] - '0') {
									m2 = 0;
								}
								if(!o && x == 0) {
									o2 = 0;
								}

								if(x == 3) {
									dp[i + 1][1][k][l][m2][o2] +=
										dp[i][j][k][l][m][o];
								} else if(x == 5) {
									dp[i + 1][j][1][l][m2][o2] +=
										dp[i][j][k][l][m][o];
								} else if(x == 7) {
									dp[i + 1][j][k][1][m2][o2] +=
										dp[i][j][k][l][m][o];
								} else if(!o && x == 0) {
									dp[i + 1][j][k][l][m2][o2] +=
										dp[i][j][k][l][m][o];
								}
							}
						}
					}
				}
			}
		}
	}

	cout << dp[n][1][1][1][1][1] + dp[n][1][1][1][0][1] << endl;

	return 0;
}
