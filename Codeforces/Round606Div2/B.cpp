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

int solve(set<int, greater<int>>& a) {
	int i = 0;
	while(!a.empty()) {
		auto ittr = a.begin();
		int tmp;
		bool update = false;
		for(; ittr != a.end(); ittr++) {
			if(!(*ittr % 2)) {
				update = true;
				tmp = *ittr / 2;
				ittr++;

				break;
			}
		}
		a.erase(a.begin(), ittr);
		if(update) {
			i++;
			a.insert(tmp);
		}
	}
	return i;
}

int main() {
	int t;
	scanf("%d", &t);
	VI n(t);
	vector<set<int, greater<int>>> a(t);
	REP(i, t) {
		scanf("%d", &n[i]);
		REP(j, n[i]) {
			int tmp;
			scanf("%d", &tmp);
			a[i].insert(tmp);
		}
	}

	REP(i, t) { printf("%d\n", solve(a[i])); }

	return 0;
}