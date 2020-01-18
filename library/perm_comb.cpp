#include <algorithm>
#include <functional>
#include <iostream>
#include <vector>

// 順列
long long permutation(int n, int k) {
	if(n < k || n < 0 || k < 0) {
		return 0LL;
	}

	long long ret = 1;
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
long long combination(int n, int r) {
	if(n < r || n < 0 || r < 0) {
		return 0LL;
	}
	if(n == r || r == 0) {
		return 1LL;
	}
	if(r > n / 2) {
		r = n - r;
	}

	long long ret = 1;
	for(int i = 1; i < r + 1; i++) {
		ret *= (n - i + 1);
		ret /= i;
	}
	return ret;
}
