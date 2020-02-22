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
#include <numeric>
#include <queue>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < i##_MACRO; i++)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) (a).begin(), (a).end()
#define AALL(a, n) (a), ((a) + (n))
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define INT(x) (static_cast<int>(x))
#define MODNUM (INT(1e9 + 7))
#define MOD(x) ((x) % MODNUM)

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

const int INF = 2e9;
const double EPS = 1e-10;
const double PI = acos(-1.0);

const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, -1, 0, 1};

template <typename T>
int sign(T x) {
	return x < 0 ? -1 : x > 0 ? 1 : 0;
}

template <>
int sign(double x) {
	return x < -EPS ? -1 : x > EPS ? 1 : 0;
}

template <typename T>
void chmax(T& m, T x) {
	m = max(m, x);
}

template <typename T>
void chmin(T& m, T x) {
	m = min(m, x);
}

template <typename T>
T square(T x) {
	return x * x;
}

inline int toInt(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

long long mod_pow(long long a, long long n, long long mod) {
	long long ret = 1;
	while(n != 0) {
		if(n % 2) {
			ret = (ret * a) % mod;
		}
		a = (a * a) % mod, n /= 2;
	}
	return ret;
}

// modの逆元
long long mod_inv(long long n, long long mod) {
	return mod_pow(n, mod - 2, mod);
}

ll mod_sub(ll a, ll b) {
	ll retval = a - b;
	if(retval < 0) {
		retval += MODNUM;
	}
	return retval;
}

ll mod_comb(ll n, ll r) {
	int retval = 1;
	chmin(r, n - r);
	REP(i, r) {
		retval = MOD(MOD(retval * (n - i)) * mod_inv((i + 1), MODNUM));
	}
	return retval;
}

int main() {
	int n, a, b;
	scanf("%d %d %d", &n, &a, &b);
	ll all = mod_sub(mod_pow(2, n, MODNUM), 1);
	printf("%lld\n", mod_sub(mod_sub(all, mod_comb(n, a)), mod_comb(n, b)));
	return 0;
}