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

// 繰り返し2乗法
// 計算量 O(logn)
constexpr ll pow(ll a, int n, int mod) {
	ll ret = 1;
	while(n != 0) {
		if(n % 2) {
			ret *= a;
			ret %= mod;
		}
		a *= a;
		a %= mod;
		n /= 2;
	}
	return ret;
}

const int n_max = 200000;

inline int popcount(string bignum) {
	return count(ALL(bignum), '1');
}

int big_mod(string bignum, int mod) {
	int n = bignum.size();
	int ret = 0;
	RANGE(i, 1, n + 1) {
		if(bignum[n - i] == '1') {
			ret += pow(2, i - 1, mod);
			ret %= mod;
		}
	}
	return ret;
}

int main() {
	int n;
	string x;
	cin >> n;
	cin >> x;

	VI results(n_max + 1, 0);
	RANGE(i, 1, n_max + 1) {
		int y = i;
		int j = 0;
		while(true) {
			j++;
			if((y %= __builtin_popcount(y)) == 0) {
				results[i] = j;
				break;
			}
			// if(results[y] != 0) {
			// 	results[i] = results[y] + j;
			// 	break;
			// }
		}
	}

	REP(i, n) {
		string xx = x;
		if(xx[i] == '1') {
			xx[i] = '0';
		} else {
			xx[i] = '1';
		}
		int xxx = big_mod(xx, popcount(xx));
		cout << results[xxx] + 1 << endl;
	}

	return 0;
}
