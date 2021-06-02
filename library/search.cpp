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
	return abs(right - left) <= 1;
}

// 終了判定(浮動小数)
int ts_count = 0;
template <typename T,
		  typename enable_if<is_floating_point<T>::value>::type* = nullptr>
bool finds(T left, T right) {
	return ts_count++ < 500;
}

// 二分探索
template <typename T>
T bin_search(T left, T right, auto pred) {
	while(!finds<T>(left, right)) {
		T middle = (left + right) / 2;
		if(pred(middle)) {
			left = middle;
		} else {
			right = middle;
		}
	}
	return left;
}

// 3分探索
// 凸関数の極値を求める
double tri_search(double left, double right, auto func) {
	while(!finds<double>(left, right)) {
		double nl = (right - left) / 3.0 + left,
			   nr = (right - left) * 2.0 / 3.0 + left;
		if(func(nl) < func(nr)) {
			right = nr;
		} else {
			left = nl;
		}
	}
	return left;
}