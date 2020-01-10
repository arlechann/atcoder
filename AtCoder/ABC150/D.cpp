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
#include <set>
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

set<ll> solve(set<ll>& p, int index, vector<set<ll>>& sets) {
	if(index == sets.size()) {
		return p;
	}
	set<ll> ret;
	set_intersection(ALL(p), ALL(sets[index]), inserter(ret, ret.end()));
	return solve(ret, index + 1, sets);
}

int main() {
	int n, m;
	scanf("%d %d", &n, &m);
	vector<ll> a(n);
	EACH(e, a) { scanf("%lld", &e); }
	sort(ALL(a), greater<ll>());
	a.erase(unique(ALL(a)), a.end());
	EACH(e, a) { printf("%d ", e); }
	putchar('\n');
	vector<set<ll>> multiples(a.size());
	REP(i, a.size()) {
		int j = 0;
		while(true) {
			if(static_cast<ll>(a[i] * (j + 0.5)) > m) {
				break;
			}
			multiples[i].insert(a[i] * (j + 0.5));
			j++;
		}
		EACH(e, multiples[i]) { printf("%d ", e); }
		putchar('\n');
	}

	set<ll> result = solve(multiples[0], 0, multiples);
	EACH(e, result) { printf("%d ", e); }
	putchar('\n');
	printf("%ld\n", result.size());
	return 0;
}