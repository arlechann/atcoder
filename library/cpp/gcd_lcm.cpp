#include <functional>
#include <map>
#include <vector>

// 最大公約数を返す
constexpr long long gcd(long long a, long long b) {
	if(a < b) {
		std::swap(a, b);
	}

	long long r = a % b;
	while(r != 0) {
		a = b;
		b = r;
		r = a % b;
	}

	return b;
}

// 最小公倍数を返す
constexpr long long lcm(long long a, long long b) {
	return a / gcd(a, b) * b;
}

// 拡張ユークリッド互除法
tuple<long long, long long, long long> ext_gcd(long long a, long long b) {
	if(b == 0) {
		return std::make_tuple(1, 0, a);
	}
	auto [y, x, d] = ext_gcd(b, a % b);
	return std::make_tuple(x, y - a / b * x, d);
}

// 一次不定方程式 (ax + by = d) を解き、非負整数になる x を求める
// d % gcd(a, b) == 0 を前提とする
ll solve_bezout_identity(ll a, ll b, ll d) {
	if(a < 0) {
		a = -a;
		d = -d;
	}
	b = abs(b);
	auto [x, y, c] = ext_gcd(a, b);
	x *= d / c;
	ll bdc = b / c;
	if(x < 0) {
		x += ((-x + bdc - 1) / bdc) * bdc;
	} else {
		x %= bdc;
	}
	return x;
}