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
