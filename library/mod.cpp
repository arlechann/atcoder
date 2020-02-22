#include <vector>

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

// modを取りつつ二項係数を計算する関数を返す
auto make_mod_comb(long long mod) {
	const int COMB_MAX = 10000000;
	vector<long long> fact(COMB_MAX);
	vector<long long> fact_inv(COMB_MAX);
	vector<long long> inv(COMB_MAX);

	fact[0] = fact[1] = 1;
	fact_inv[0] = fact_inv[1] = 1;
	inv[1] = 1;

	for(int i = 2; i < COMB_MAX; i++) {
		fact[i] = (fact[i - 1] * i) % mod;
		inv[i] = mod - (inv[mod % i] * (mod / i)) % mod;
		fact_inv[i] = (fact_inv[i - 1] * inv[i]) % mod;
	}

	return [mod = mod, fact = move(fact), fact_inv = move(fact_inv)](
			   const long long n, const long long r) {
		if(n < r || n < 0 || r < 0) {
			return 0LL;
		}
		return (fact[n] * ((fact_inv[r] * fact_inv[n - r]) % mod)) % mod;
	};
}
