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

constexpr int INF = 2e9;
constexpr double EPS = 1e-10;
constexpr double PI = acos(-1.0);

constexpr int dx[] = {-1, 0};
constexpr int dy[] = {0, -1};

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
	if(m < x) {
		m = x;
		return true;
	}
	return false;
}

template <typename T, typename U>
constexpr bool chmin(T& m, U x) {
	if(m > x) {
		m = x;
		return true;
	}
	return false;
}

template <typename T>
constexpr T square(T x) {
	return x * x;
}

int main() {
	string s, t;
	cin >> s;
	cin >> t;
	int n, m;
	n = s.size();
	m = t.size();
	VI2D dp(n, VI(m, 0));
	vector<vector<pair<int, int>>> prev(
		n, vector<pair<int, int>>(m, make_pair(-1, -1)));
	REP(i, n) {
		REP(j, m) {
			if(s[i] == t[j]) {
				dp[i][j] = ((i > 0 && j > 0) ? dp[i - 1][j - 1] : 0) + 1;
				prev[i][j] = make_pair(i - 1, j - 1);
				continue;
			}

			REP(k, 2) {
				if(i + dy[k] < 0 || j + dx[k] < 0) {
					continue;
				}
				if(chmax(dp[i][j], dp[i + dy[k]][j + dx[k]])) {
					prev[i][j] = make_pair(i + dy[k], j + dx[k]);
				}
			}
		}
	}

	int i = n - 1;
	int j = m - 1;
	string result = "";
	while(i >= 0 && j >= 0) {
		auto p = prev[i][j];
		if(s[i] == t[j]) {
			result = s[i] + move(result);
		}
		i = p.first;
		j = p.second;
	}
	cout << result << endl;
	return 0;
}
