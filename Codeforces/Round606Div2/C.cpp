#include <algorithm>
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

int match(char* t, char* p) {
	int n = strlen(t), m = strlen(p);
	int M[0x100];
	fill(M, M + 0x100, 0);
	int count = 0;
	for(int i = 0; i < m; ++i)
		M[p[i]] |= (1 << i);
	for(int i = 0, S = 0; i < n; ++i) {
		S = ((S << 1) | 1) & M[t[i]];
		if(S & (1 << (m - 1))) {
			++count; // match at t[i-m+1 ... i]
		}
	}
	return count;
}

VI solve(string s) {
	VI result;
	int c[3];
	c[0] = match(s.c_str(), "twone");
	c[1] = match(s.c_str(), "two");
	c[2] = match(s.c_str(), "one");
	return
}

int main() {
	int t;
	cin >> t;
	vector<string> s(t);
	REP(i, t) { cin >> s[i]; }

	REP(i, t) {
		VI result = solve(s[i]);
		printf("%d\n", result.size());
		REP(j, result.size()) { printf("%d ", result[j] + 1); }
		putchar('\n');
	}
	return 0;
}