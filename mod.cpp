#define MOD(x) ((x) % static_cast<int>(1e9 + 7))

using ll = long long;

// コピペここから

ll mod_pow(ll a, ll n) {
#ifndef MOD
	static_assert(false, "Not Defined MOD().");
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