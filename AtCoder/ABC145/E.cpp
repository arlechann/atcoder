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
#define MOD(x) ((x) % static_cast<int>(1e9 + 7))

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

const int INF = 2e9;
const double EPS = 1e-10;
const double PI = acos(-1.0);

inline int toInt(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

int dp[3001][3001];

int main() {
	int n, t;
	scanf("%d %d", &n, &t);
	VI a(n);
	VI b(n);
	REP(i, n) { scanf("%d %d", &a[i], &b[i]); }

	REP(i, n) {
		REP(j, t + 1) {
			if(j + a[i] < 3001) {
				dp[i + 1][j + a[i]] = max(dp[i][j] + b[i], dp[i + 1][j + a[i]]);
			}
		}
	}

	printf("%d\n", dp[n][t]);

	return 0;
}