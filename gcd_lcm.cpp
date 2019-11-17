#include <functional>

using namespace std;

// lcm ここから
// gcd ここから
template <typename T>
T gcd(T a, T b) {
	if(a < b) {
		swap(a, b);
	}

	T r = a % b;
	while(r != 0) {
		a = b;
		b = r;
		r = a % b;
	}

	return b;
}
// gcd ここまで

template <typename T>
T lcm(T a, T b) {
	return a * b / gcd(a, b); // オーバーフロー注意
}
// lcm ここまで