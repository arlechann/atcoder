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

// 二項係数
// 計算量 O(r)
template <typename T>
constexpr T combination(long long n, long long r) {
	if(n < r || n < 0 || r < 0) {
		return T(0);
	}
	T ret = 1;
	r = std::min(r, n - r);
	for(int i = 0; i < r; i++) {
		ret *= (n - i);
		ret /= i + 1;
	}
	return ret;
}

int main() {
	int n;
	cin >> n;
	vector<ll> a(n);
	unordered_map<ll, ll> m;
	REP(i, n) {
		cin >> a[i];
		m[a[i]]++;
	}
	unordered_map<ll, ll> comb;
	ll sum = 0;
	REP(i, n) {
		if(comb[a[i]] == 0) {
			comb[a[i]] = combination<ll>(m[a[i]], 2);
			sum += comb[a[i]];
		}
	}
	REP(i, n) {
		cout << sum - comb[a[i]] + comb[a[i]] * (m[a[i]] - 2) / m[a[i]] << endl;
	}
	return 0;
}
