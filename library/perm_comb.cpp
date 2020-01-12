#include <algorithm>
#include <functional>
#include <iostream>
#include <vector>

// permutation ここから
// 順列
long long permutation(int n, int k) {
	if(n < k || n < 0 || k < 0) {
		return 0LL;
	}

	ll ret = 1;
	for(int i = 0; i < k; i++) {
		ret *= n - i;
	}
	return ret;
}
// premutation ここまで

// ALL_PERM ここから
// 引数の関数にすべての順列を渡して実行する 実行した関数がtrueを返すと打ち切る
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
// ALL_PREM ここまで

// combination ここから
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
// combination ここまで

int main() {
	std::vector<int> v = {1, 2, 3, 4, 5};
	std::vector<vector<int>> a;

	ALL_PERM(v, {
		a.push_back(v);
		if(v == VI{1, 5, 4, 3, 2}) {
			// return true;
		}
	});

	std::for_each(a.begin(), a.end(), [](VI x) {
		std::for_each(x.begin(), x.end(), [](int n) { cout << n << " "; });
		cout << endl;
	});

	cout << "size:" << a.size() << endl;
	cout << "permutation:" << permutation(5, 5) << endl;
	cout << "combination:" << combination(33, 17) << endl;

	return 0;
}