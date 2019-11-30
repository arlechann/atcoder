#include <vector>

#define RANGE(i, a, b) for(int i = (a), i < (b), i++)
#define MODNUM (1e9 + 7)
#define MOD(x) ((x) % static_cast<int>(MODNUM))

using namespace std;
using ll = long long;

// mod_pow ここから
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
// mod_pow ここまで

// mod_comb ここから
auto make_mod_comb() {
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
// mod_comb ここまで