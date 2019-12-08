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
#define MODNUM (static_cast<unsigned long long>(1e9 + 7))
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
T square(T x) {
	return x * x;
}

inline int toInt(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

ll mod_pow(ll a, ll n) {
#ifndef MOD
	static_assert(false, "MOD() is not defined.");
#endif

	ll ret = 1;
	while(n != 0) {
		if(n % 2) {
			ret = MOD(ret * a);
		}
		a = MOD(a * a);
		n /= 2;
	}
	return ret;
}

int main() {
	ll n;
	scanf("%lld", &n);
	vector<ll> a(n);
	EACH(e, a) { scanf("%lld", &e); }

	ll result = 0;
	REP(i, 60) {
		ll one = 0;
		ll zero = 0;

		REP(j, n) {
			((a[j] & 1) ? one : zero) += 1;
			a[j] >>= 1;
		}

		result = MOD(result + MOD(MOD(one * zero) * mod_pow(2, i)));
	}

	printf("%lld\n", result);

	return 0;
}