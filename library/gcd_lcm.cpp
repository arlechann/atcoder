#include <functional>
#include <map>
#include <vector>

// 最大公約数を返す
long long gcd(long long a, long long b) {
	if(a < b) {
		std::swap(a, b);
	}

	T r = a % b;
	while(r != 0) {
		a = b;
		b = r;
		r = a % b;
	}

	return b;
}

// 最小公倍数を返す
long long lcm(long long a, long long b) {
	return a / gcd(a, b) * b;
}
