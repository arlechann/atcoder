#include <algorithm>
#include <functional>
#include <iostream>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < (i##_MACRO); i++)
#define RANGE(i, a, b) for(int i = (a); i < (b); i++)
#define ALL(a) (a).begin(), (a).end()

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

// permutation ここから
// 順列
ll permutation(int n, int k) {
	if(n < k || n < 0 || k < 0) {
		return 0LL;
	}

	ll ret = 1;
	REP(i, k) { ret *= n - i; }
	return ret;
}
// premutation ここまで

// ALL_PERM ここから
// 引数の関数にすべての順列を渡して実行する 実行した関数がtrueを返すと打ち切る
#define ALL_PERM(v, lambda)                     \
	call_with_all_permutations(v, [&](auto v) { \
		lambda;                                 \
		return false;                           \
	})
template <typename T, typename F>
void call_with_all_permutations(T a, F proc) {
#ifndef ALL
	static_assert(false, "ALL() is not defined.");
#endif

	do {
		if(proc(a)) {
			return;
		};
	} while(next_permutation(ALL(a)));
}
// ALL_PREM ここまで

// combination ここから
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

// 二項係数
ll combination(int n, int r) {
	if(n < r || n < 0 || r < 0) {
		return 0LL;
	}
	if(n == r || r == 0) {
		return 1LL;
	}
	if(r > n / 2) {
		r = n - r;
	}

	ll ret = 1;
	RANGE(i, 1, r + 1) {
		ret *= (n - i + 1);
		ret /= i;
	}
	return ret;
}
// combination ここまで

int main() {
	VI v = {1, 2, 3, 4, 5};
	VI2D a;

	ALL_PERM(v, {
		a.push_back(v);
		if(v == VI{1, 5, 4, 3, 2}) {
			// return true;
		}
	});

	for_each(a.begin(), a.end(), [](VI x) {
		for_each(x.begin(), x.end(), [](int n) { cout << n << " "; });
		cout << endl;
	});

	cout << "size:" << a.size() << endl;
	cout << "permutation:" << permutation(5, 5) << endl;
	cout << "combination:" << combination(33, 17) << endl;

	return 0;
}