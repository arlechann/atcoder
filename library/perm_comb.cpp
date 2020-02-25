#include <algorithm>
#include <functional>
#include <iostream>
#include <vector>

// 繰り返し2乗法
// 計算量 O(logn)
template <typename T>
constexpr T pow(T a, int n) {
	T ret = 1;
	while(n != 0) {
		if(n % 2) {
			ret *= a;
		}
		a *= a;
		n /= 2;
	}
	return ret;
}

// 順列
// 計算量 O(k)
template <typename T>
constexpr T permutation(int n, int k) {
	if(n < k || n < 0 || k < 0) {
		return T(0);
	}

	T ret = 1;
	for(int i = 0; i < k; i++) {
		ret *= n - i;
	}
	return ret;
}

// 引数の関数にすべての順列を渡して実行する
// trueを返すと打ち切る
#define ALL_PERM(v, block)                      \
	call_with_all_permutations(v, [&](auto v) { \
		block;                                  \
		return false;                           \
	})
template <typename T, typename F>
void call_with_all_permutations(T a, F proc) {
	do {
		if(proc(a)) {
			return;
		};
	} while(std::next_permutation(a.begin(), a.end()));
}

// 二項係数
// 計算量 O(r)
template <typename T>
constexpr T combination(long long n, long long r) {
	if(n < r || n < 0 || r < 0) {
		return T(0);
	}
	T ret = 1;
	r = std::min(r, n - r);
	for(int i = 0; i < r; i++) {
		ret *= (n - i);
		ret /= i + 1;
	}
	return ret;
}

