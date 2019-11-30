#include <algorithm>
#include <cmath>
#include <functional>
#include <type_traits>
#include <vector>

using namespace std;

const double EPS = 1e-10;

// gs_search ここから
// bin_search ここから
template <typename T,
		  typename enable_if<is_integral<T>::value>::type* = nullptr>
bool finds(T left, T right) {
	return right - left > 1;
}

template <typename T,
		  typename enable_if<is_floating_point<T>::value>::type* = nullptr>
bool finds(T left, T right) {
	return abs(right - left) > EPS;
}

template <typename T>
T bin_search(T left, T right, auto pred) {
	while(finds<T>(left, right)) {
		T middle = (left + right) / 2;
		if(pred(middle)) {
			left = middle;
		} else {
			right = middle;
		}
	}
	return left;
}
// bin_search ここまで

// 黄金分割探索
const double GOLDEN_RATIO = (1 + sqrt(5)) / 2;
double gs_search(double left, double right, auto func) {
	double nl, nr;
	nl = (left * GOLDEN_RATIO + right) / (GOLDEN_RATIO + 1.0);
	nr = (left + right * GOLDEN_RATIO) / (GOLDEN_RATIO + 1.0);
	while(finds<double>(left, right)) {
		if(func(nl) > func(nr)) {
			right = nr;
			nr = nl;
			nl = right - (nr - left);
		} else {
			left = nl;
			nl = nr;
			nr = left + (right - nl);
		}
	}
	return nl;
}
// gs_search ここまで

int main() {
	vector<int> v{0, 0, 1, 1, 1, 2, 3, 4, 4, 5};
	printf("%d\n",
		   bin_search(0, (int)v.size(), [&](auto x) { return v[x] <= 1; }));

	auto f = [](double x) { return x * x / 3; };
	printf("%f\n",
		   f(bin_search(0.0, 1000.0, [&](auto x) { return f(x) < 100.0; })));

	auto g = [](double x) { return -(x * x) + 5.5; };
	printf("%f\n", g(gs_search(-100.0, 100.0, g)));
	return 0;
}