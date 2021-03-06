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
#define INT(x) (static_cast<int>(x))
#define MODNUM (INT(1e9 + 7))
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
void chmax(T& m, T x) {
	m = max(m, x);
}

template <typename T>
void chmin(T& m, T x) {
	m = min(m, x);
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

int main() {
	int n, k;
	scanf("%d %d", &n, &k);
	VI p(n);
	REP(i, n) { scanf("%d", &p[i]); }
	VI sum(1001);
	sum[0] = 0;
	REP(i, 1000) { sum[i + 1] += sum[i] + i + 1; }
	vector<double> a(n);
	REP(i, n) {
		a[i] = static_cast<double>(sum[p[i]]) / static_cast<double>(p[i]);
	}
	// REP(i, n) { printf("a[%d]: %f\n", i, a[i]); }
	vector<double> aa(n - k + 1);
	aa[0] = 0;
	REP(i, k) { aa[0] += a[i]; }
	REP(i, n - k) { aa[i + 1] = aa[i] - a[i] + a[i + k]; }
	// REP(i, n - k + 1) { printf("aa[%d]: %f\n", i, aa[i]); }
	double result = 0;
	REP(i, n - k + 1) { chmax(result, aa[i]); }
	printf("%.9f\n", result);
	return 0;
}