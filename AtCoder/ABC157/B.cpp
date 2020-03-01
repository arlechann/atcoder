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

bool check(VI2D& a) {
	REP(i, 3) {
		if(!a[i][0] && !a[i][1] && !a[i][2]) {
			return true;
		}
	}
	REP(i, 3) {
		if(!a[0][i] && !a[1][i] && !a[2][i]) {
			return true;
		}
	}
	if(!a[0][0] && !a[1][1] && !a[2][2]) {
		return true;
	}
	if(!a[2][0] && !a[1][1] && !a[0][2]) {
		return true;
	}
	return false;
}

int main() {
	VI2D a(3, VI(3));
	REP(i, 3) {
		REP(j, 3) { cin >> a[i][j]; }
	}
	int n;
	cin >> n;
	VI b(n);
	REP(i, n) { cin >> b[i]; }
	REP(i, 3) {
		REP(j, 3) {
			REP(k, n) {
				if(a[i][j] == b[k]) {
					a[i][j] = 0;
					break;
				}
			}
		}
	}

	cout << (check(a) ? "Yes" : "No") << endl;

	return 0;
}
