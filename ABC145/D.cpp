#include <algorithm>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iostream>
#include <list>
#include <numeric>
#include <queue>
#include <sstream>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < (i##_MACRO); i++)
#define RANGE(i, a, b) for(int i = (a), i < (b), i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) (a).begin(), (a).end()
#define AALL(a, n) (a), a + n
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

const int INF = 2e9;
const double EPS = 1e-9;
const double PI = acos(-1.0);

inline int toInt(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

// http://drken1215.hatenablog.com/entry/2018/06/08/210000
const int MAX = 10000000;
const int MOD = 1000000007;

long long fac[MAX], finv[MAX], inv[MAX];

// テーブルを作る前処理
void COMinit() {
	fac[0] = fac[1] = 1;
	finv[0] = finv[1] = 1;
	inv[1] = 1;
	for(int i = 2; i < MAX; i++) {
		fac[i] = fac[i - 1] * i % MOD;
		inv[i] = MOD - inv[MOD % i] * (MOD / i) % MOD;
		finv[i] = finv[i - 1] * inv[i] % MOD;
	}
}

// 二項係数計算
long long COM(int n, int k) {
	if(n < k)
		return 0;
	if(n < 0 || k < 0)
		return 0;
	return fac[n] * (finv[k] * finv[n - k] % MOD) % MOD;
}

double degree2radian(double degree) {
	return degree * PI / 180;
}

int main() {
	int x, y;
	scanf("%d %d", &x, &y);

	double nx, ny;
	nx = (2 * y - x) / 3.0;
	ny = y - 2 * nx;

	if(abs(nx - static_cast<int>(nx)) > EPS ||
	   abs(ny - static_cast<int>(ny)) > EPS) {
		puts("0");
		return 0;
	}

	COMinit();

	printf("%lld\n", COM(static_cast<int>(nx) + static_cast<int>(ny),
						 static_cast<int>(ny)));

	return 0;
}