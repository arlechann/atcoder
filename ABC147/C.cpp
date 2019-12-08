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

int n;
VI a;
VI2D x;
VI2D y;

bool check(int set) {
	REP(i, n) {
		REP(j, x[i].size()) {
			if(set & (1 << i)) {
				if(y[i][j] == 1 && !(set & (1 << (x[i][j] - 1)))) {
					//					printf("y[i][j] == 1\n(1 << (x[i][j] - 1)):
					//%d\n", 						   (1 << (x[i][j] - 1)));
					return false;
				} else if(y[i][j] == 0 && (set & (1 << (x[i][j] - 1)))) {
					//					printf("y[i][j] == 0\n(1 << (x[i][j] - 1)):
					//%d\n", 						   (1 << (x[i][j] - 1)));
					return false;
				}
			}
		}
	}
	return true;
}

int main() {
	scanf("%d", &n);
	a.resize(n);
	x.resize(n);
	y.resize(n);

	REP(i, n) {
		scanf("%d", &a[i]);
		REP(j, a[i]) {
			int xx, yy;
			scanf("%d %d", &xx, &yy);
			x[i].push_back(xx);
			y[i].push_back(yy);
		}
	}

	int result = 0;
	int honest_set = 0;
	while(honest_set < (1 << n)) {
		if(check(honest_set)) {
			result = max(result, __builtin_popcount(honest_set));
			//			printf("popcount: %d\n",
			//__builtin_popcount(honest_set));
		}
		//		printf("set: %d\n", honest_set);
		honest_set++;
	}
	printf("%d\n", result);
	return 0;
}