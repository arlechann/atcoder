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
#define MODNUM (static_cast<int>(1e9 + 7))
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

inline int toInt(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

int sign(ll n) {
	if(n > 0) {
		return 1;
	}
	if(n < 0) {
		return -1;
	}
	return 0;
}

int main() {
	ll t[2];
	ll a[2];
	ll b[2];
	scanf("%lld %lld", &t[0], &t[1]);
	scanf("%lld %lld", &a[0], &a[1]);
	scanf("%lld %lld", &b[0], &b[1]);
	ll ab[2];
	ab[0] = b[0] - a[0];
	ab[1] = b[1] - a[1];
	if(sign(ab[0]) == sign(ab[1])) {
		puts("0");
		return 0;
	}
	ll abt[2];
	abt[0] = ab[0] * t[0];
	abt[1] = ab[1] * t[1];
	if(abt[0] + abt[1] == 0) {
		puts("infinity");
		return 0;
	}
	if(abs(abt[0]) > abs(abt[1])) {
		puts("0");
		return 0;
	}
	ll abti = abs(abt[0] + abt[1]);
	abt[0] = abs(abt[0]);

	ll result = (abt[0] + (abti - 1)) / abti;
	printf("%lld\n", result * 2 - (abt[0] % abti ? 1 : 0));
	return 0;
}