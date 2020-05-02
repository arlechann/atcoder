#include <algorithm>
#include <boost/optional.hpp>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
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

int n, m, q;

int solve(VI& a, VI& b, VI& c, VI& d, VI A, int i, int x) {
	int result = 0;
	if(i == n) {
		REP(i, q) {
			if(A[b[i] - 1] - A[a[i] - 1] == c[i]) {
				result += d[i];
			}
		}
		return result;
	}
	if(i != -1) {
		A.push_back(x);
	}
	RANGE(j, x, m) { chmax(result, solve(a, b, c, d, A, i + 1, j)); }
	return result;
}

int main() {
	cin >> n >> m >> q;
	VI a(q);
	VI b(q);
	VI c(q);
	VI d(q);
	REP(i, q) { cin >> a[i] >> b[i] >> c[i] >> d[i]; }
	cout << solve(a, b, c, d, VI(), -1, 0) << endl;
	return 0;
}
