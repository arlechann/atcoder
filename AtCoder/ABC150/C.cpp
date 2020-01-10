#include <algorithm>
#include <boost/optional.hpp>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iostream>
#include <limits>
#include <list>
#include <numeric>
#include <queue>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < i##_MACRO; i++)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) (a).begin(), (a).end()
#define AALL(a, n) (a), ((a) + (n))
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define MODNUM (static_cast<int>(1e9 + 7))
#define MOD(x) ((x) % MODNUM)

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

const int INF = 2e9;
const double EPS = 1e-10;
const double PI = acos(-1.0);

const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, -1, 0, 1};

template <typename T>
int sign(T x) {
	return x < 0 ? -1 : x > 0 ? 1 : 0;
}

template <>
int sign(double x) {
	return x < -EPS ? -1 : x > EPS ? 1 : 0;
}

template <typename T>
T square(T x) {
	return x * x;
}

inline int toInt(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

// ALL_PERM ここから
// 引数の関数にすべての順列を渡して実行する 実行した関数がtrueを返すと打ち切る
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

int abs_diff(int a, int b) {
	if(a > b) {
		return a - b;
	}
	return b - a;
}

int main() {
	int n;
	scanf("%d", &n);
	VI p(n);
	VI q(n);
	EACH(e, p) { scanf("%d", &e); }
	EACH(e, q) { scanf("%d", &e); }
	int p_order = 0;
	int q_order = 0;
	VI perm(n);
	iota(ALL(perm), 1);
	call_with_all_permutations(perm, [&](auto v) {
		p_order++;
		if(v == p) {
			return true;
		}
		return false;
	});
	call_with_all_permutations(perm, [&](auto v) {
		q_order++;
		if(v == q) {
			return true;
		}
		return false;
	});
	printf("%d\n", abs_diff(p_order, q_order));
	return 0;
}