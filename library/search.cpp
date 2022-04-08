#include <algorithm>
#include <cmath>
#include <functional>
#include <type_traits>
#include <vector>

using namespace std;

const double EPS = 1e-10;

template <typename T>
bool finds(T left, T right) {
	return abs(right - left) <= 1;
}

int ts_count = 0;
template <typename double>
bool finds(double left, double right) {
	return ts_count++ < 300;
}

// 二分探索
template <typename T>
T bin_search(T ok, T ng, auto pred) {
	while(!finds<T>(ok, ng)) {
		T middle = (ok + ng) / 2;
		if(pred(middle)) {
			ok = middle;
		} else {
			ng = middle;
		}
	}
	return ok;
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