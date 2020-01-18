#include <functional>

// 最大公約数を返す
template <typename T>
T gcd(T a, T b) {
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
template <typename T>
T lcm(T a, T b) {
	return a / gcd(a, b) * b;
}