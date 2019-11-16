#include <functional>

using namespace std;

// lcm ここから
// gcd ここから
int gcd(int a, int b) {
	if(a < b) {
		swap(a, b);
	}

	int r = a % b;
	while(r != 0) {
		a = b;
		b = r;
		r = a % b;
	}

	return b;
}
// gcd ここまで

int lcm(int a, int b) {
	return a * b / gcd(a, b); // オーバーフロー注意
}
// lcm ここまで