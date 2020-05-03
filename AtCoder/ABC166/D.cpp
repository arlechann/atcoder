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

// 終了判定
template <typename T,
		  typename enable_if<is_integral<T>::value>::type* = nullptr>
bool finds(T left, T right) {
	return right - left > 1;
}

// 終了判定(浮動小数)
template <typename T,
		  typename enable_if<is_floating_point<T>::value>::type* = nullptr>
bool finds(T left, T right) {
	return abs(right - left) > EPS;
}

// 二分探索
template <typename T>
pair<T, bool> bin_search(T left, T right, auto pred, auto f) {
	int i = 0;
	while(f(left, right)) {
		if(i > 200) {
			return make_pair(0, false);
		}
		T middle = (left + right) / 2;
		if(pred(middle)) {
			left = middle;
		} else {
			right = middle;
		}
		i++;
	}
	return make_pair(left, true);
}

int main() {
	ll x;
	cin >> x;
	for(ll b = -1000LL; b <= 1000LL; b++) {
		pair<ll, bool> searched = bin_search(
			-1000LL,
			1000LL,
			[&](ll a) { return a * a * a * a * a <= x + (b * b * b * b * b); },
			[&](ll l, ll r) {
				return l * l * l * l * l != x + (b * b * b * b * b);
			});
		if(searched.second) {
			cout << searched.first << " " << b << endl;
			break;
		}
	}
	return 0;
}
