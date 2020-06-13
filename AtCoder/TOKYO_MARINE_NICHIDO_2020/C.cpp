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

int main() {
	int n, k;
	cin >> n >> k;
	VI a(n);
	REP(i, n) { cin >> a[i]; }

	// 0: 0 0 0 0 0 | 1 0 0 0 0 | 2 0 0 0 0 | 1 0 0 1 0
	// 1: 1 1 1 1 1 | 1 2 1 1 1 | 1 2 2 1 1 | 1 2 2 1 2
	// 2: 2 3 3 3 2 | 2 3 3 4 2 | 3 3 3 4 3 | 3 3 4 4 3
	// 3: 4 4 5 4 4 | 4 4 5 4 4 | 4 5 5 5 4 | 4 5 5 5 4
	// 4: 5 5 5 5 5 | 5 5 5 5 5 | 5 5 5 5 5 | 5 5 5 5 5

	REP(i, k) {
		VI b(n + 1, 0);
		REP(j, n) {
			b[max(j - a[j], 0)]++;
			b[min(j + a[j] + 1, n + 1)]--;
		}
		int tmp = 0;
		REP(j, n) {
			tmp += b[j];
			a[j] = tmp;
		}
	}

	REP(j, n) { cout << a[j] << " "; }
	cout << endl;
	return 0;
}
