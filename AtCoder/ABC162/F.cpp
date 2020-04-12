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
	int n;
	cin >> n;
	VI a(n);
	REP(i, n) { cin >> a[i]; }

	ll odd_sum = 0;
	vector<ll> even;
	vector<ll> odd;
	even.push_back(0);
	odd.push_back(0);
	REP(i, n) {
		if(i % 2) {
			even.push_back(even[even.size() - 1] + a[i]);
		} else {
			odd.push_back(odd[odd.size() - 1] + a[i]);
			odd_sum += a[i];
		}
	}

	EACH(e, even) { cout << e << ' '; }
	cout << endl;
	EACH(e, odd) { cout << e << ' '; }
	cout << endl;

	ll result = -INF;
	if(n % 2) {
		REP(i, n + 1) {
			RANGE(j, i, n + 1) {
				chmax(result,
					  odd_sum + (even[j / 2] - even[i / 2]) -
						  (odd[j / 2] - odd[i / 2]));
			}
		}
	} else {
		REP(i, n + 1) {
			chmax(result, odd[i / 2] + (even[(n + 1) / 2] - even[i / 2]));
		}
	}
	cout << result << endl;
	return 0;
}
