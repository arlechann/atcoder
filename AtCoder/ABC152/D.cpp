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

int top_digit(int n) {
	while(n / 10 > 0) {
		n /= 10;
	}
	return n;
}

int bottom_digit(int n) {
	return n % 10;
}

int kirisage(int n) {
	int ret = 1;
	while(ret * 10 < n) {
		ret *= 10;
	}
	return ret;
}

int count(int n, int y, int x) {
	int ret = 0;

	RANGE(i, 1, n + 1) {
		int top = top_digit(i);
		int bottom = bottom_digit(i);
		if(top == y && bottom == x) {
			ret++;
		}
	}
	return ret;
}
int main() {
	int n;
	scanf("%d", &n);
	VI2D num(10, VI(10, 0));
	RANGE(i, 1, 10) {
		RANGE(j, 1, 10) { num[i][j] = count(n, i, j); }
	}
	ll result = 0;
	RANGE(i, 1, 10) {
		RANGE(j, 1, 10) { result += (num[i][j] * num[j][i]); }
	}

	printf("%lld\n", result);
	return 0;
}