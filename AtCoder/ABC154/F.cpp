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

// modを取りつつ二項係数を計算する関数を返す
auto make_mod_comb(long long mod) {
	const int COMB_MAX = 1100000;
	vector<long long> fact(COMB_MAX);
	vector<long long> fact_inv(COMB_MAX);
	vector<long long> inv(COMB_MAX);

	fact[0] = fact[1] = 1;
	fact_inv[0] = fact_inv[1] = 1;
	inv[1] = 1;

	for(int i = 2; i < COMB_MAX; i++) {
		fact[i] = (fact[i - 1] * i) % mod;
		inv[i] = mod - (inv[mod % i] * (mod / i)) % mod;
		fact_inv[i] = (fact_inv[i - 1] * inv[i]) % mod;
	}

	return [mod = mod, fact = move(fact), fact_inv = move(fact_inv)](
			   const long long n, const long long r) {
		if(n < r || n < 0 || r < 0) {
			return 0LL;
		}
		return (fact[n] * ((fact_inv[r] * fact_inv[n - r]) % mod)) % mod;
	};
}

int main() {
	int r1, c1, r2, c2;
	scanf("%d %d %d %d", &r1, &c1, &r2, &c2);
	auto mod_comb = make_mod_comb(MODNUM);
	ll result = 0;
	RANGE(i, r1, r2 + 1) {
		RANGE(j, c1, c2 + 1) { result = MOD(result + mod_comb(i + j, i)); }
	}
	printf("%lld\n", result);
	return 0;
}