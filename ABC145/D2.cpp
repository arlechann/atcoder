#include <algorithm>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iostream>
#include <list>
#include <memory>
#include <numeric>
#include <queue>
#include <sstream>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < (i##_MACRO); i++)
#define RANGE(i, a, b) for(int i = (a); i < (b); i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) (a).begin(), (a).end()
#define AALL(a, n) (a), a + n
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define MODNUM (static_cast<int>(1e9 + 7))
#define MOD(x) ((x) % MODNUM)

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

const int INF = 2e9;
const double EPS = 1e-9;
const double PI = acos(-1.0);

inline int toInt(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

auto init_mod_comb() {
#ifndef MODNUM
	static_assert(false, "MODNUM is not defined.");
#endif
#ifndef MOD
	static_assert(false, "MOD() is not defined.");
#endif

	const int COMB_MAX = 10000000;
	vector<ll> fact(COMB_MAX);
	vector<ll> fact_inv(COMB_MAX);
	vector<ll> inv(COMB_MAX);

	fact[0] = fact[1] = 1;
	fact_inv[0] = fact_inv[1] = 1;
	inv[1] = 1;

	RANGE(i, 2, COMB_MAX) {
		fact[i] = MOD(fact[i - 1] * i);
		inv[i] = MODNUM - MOD(inv[MODNUM % i] * (MODNUM / i));
		fact_inv[i] = MOD(fact_inv[i - 1] * inv[i]);
	}

	return
		[fact = move(fact), fact_inv = move(fact_inv)](const ll n, const ll r) {
			if(n < r || n < 0 || r < 0) {
				return 0LL;
			}
			return MOD(fact[n] * MOD(fact_inv[r] * fact_inv[n - r]));
		};
}

int main() {
	int x, y;
	scanf("%d %d", &x, &y);

	double nx, ny;
	nx = (2 * y - x) / 3.0;
	ny = y - 2 * nx;

	if(abs(nx - static_cast<int>(nx)) > EPS ||
	   abs(ny - static_cast<int>(ny)) > EPS) {
		puts("0");
		return 0;
	}

	auto mod_comb = init_mod_comb();

	printf("%lld\n", mod_comb(nx + ny, nx));

	return 0;
}