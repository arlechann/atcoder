#include <algorithm>
#include <cmath>
#include <functional>
#include <type_traits>
#include <vector>

using namespace std;

const double EPS = 1e-10;

// 終了判定
template <typename T,
		  typename enable_if<is_integral<T>::value>::type* = nullptr>
bool finds(T left, T right) {
	return right - left > 1;
}

// 終了判定(浮動小数)
template <typename T,
		  typename enable_if<is_floating_point<T>::value>::type* = nullptr>
bool finds(T left, T right) {
	return abs(right - left) > EPS;
}

// 二分探索
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

// 黄金分割探索
// 凸関数の極値を求める
const double GOLDEN_RATIO = (1 + sqrt(5)) / 2; // 黄金比
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
