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
		int j = 0;
		int k = i;
		while(true) {
			j++;
			if((k %= __builtin_popcount(k)) == 0) {
				results[i] = j;
				break;
			}
			if(results[k] != 0) {
				results[i] = results[k] + j;
				break;
			}
		}
	}

	vector<vector<ll>> pow_table(2, vector<ll>(n_max + 1, 0));
	int x_pcount = popcount(x);
	REP(i, 2) {
		REP(j, n_max + 1) {
			if(j == 0) {
				pow_table[i][0] = 1;
				continue;
			}
			pow_table[i][j] = pow_table[i][j - 1] * 2;
			if((x_pcount == 1 && i == 0) || (x_pcount == 0 && i == 0)) {
				/* do nothing */;
			} else {
				pow_table[i][j] %= (x_pcount + (i == 0 ? -1 : 1));
			}
		}
	}

	int x_mod_pcount_minus1 = 0;
	int modnum_pcount_minus1 = (x_pcount == 1) ? 1 : x_pcount - 1;
	int x_mod_pcount_plus1 = 0;

	if(x_pcount != 0) {
		REP(i, n) {
			if(x[n - (i + 1)] == '1') {
				x_mod_pcount_minus1 += pow_table[0][i];
				x_mod_pcount_minus1 %= modnum_pcount_minus1;
			}
		}
	}
	REP(i, n) {
		if(x[n - (i + 1)] == '1') {
			x_mod_pcount_plus1 += pow_table[1][i];
			x_mod_pcount_plus1 %= x_pcount + 1;
		}
	}

	REP(i, n) {
		if(x[i] == '1') {
			if(x_pcount == 1) {
				cout << 0 << endl;
			} else {
				int index = (x_mod_pcount_minus1 - pow_table[0][n - (i + 1)]) %
							modnum_pcount_minus1;
				index += index < 0 ? (modnum_pcount_minus1) : 0;
				cout << results[index] + 1 << endl;
			}
		} else {
			int index = (x_mod_pcount_plus1 + pow_table[1][n - (i + 1)]) %
						(x_pcount + 1);
			cout << results[index] + 1 << endl;
		}
	}

	return 0;
}
