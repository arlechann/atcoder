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

template <typename T>
std::unordered_map<size_t, T> group_count(std::vector<T>& v) {
	std::unordered_map<size_t, T> c;
	for(T& e : v) {
		c[e]++;
	}
	return c;
}

double memo[301][301][301];

double solve(int n, int c1, int c2, int c3) {
	if(memo[c1][c2][c3] >= 0) {
		return memo[c1][c2][c3];
	}
	if(c1 == 0 && c2 == 0 && c3 == 0) {
		return 0.0;
	}

	return memo[c1][c2][c3] =
			   ((c1 > 0 ? (solve(n, c1 - 1, c2, c3) * c1) : 0.0) +
				(c2 > 0 ? (solve(n, c1 + 1, c2 - 1, c3) * c2) : 0.0) +
				(c3 > 0 ? (solve(n, c1, c2 + 1, c3 - 1) * c3) : 0.0) + n) /
			   static_cast<double>(c1 + c2 + c3);
}

int main() {
	int n;
	cin >> n;
	VI a(n);
	REP(i, n) { cin >> a[i]; }
	unordered_map<size_t, int> cnt = group_count(a);
	FILL(memo, -1);
	cout << PRECISION(15) << solve(n, cnt[1], cnt[2], cnt[3]) << endl;
	return 0;
}
