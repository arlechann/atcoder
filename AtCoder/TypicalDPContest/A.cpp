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

#define REP(i, n) for(int i = 0; i < (n); i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) (a).begin(), (a).end()
#define AALL(a, n) (a), a + n
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define MOD(x) ((x) % (1e9 + 7))

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

const double EPS = 1e-10;
const double PI = acos(-1.0);

inline int toInt(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

int solve(int n, VI p, int max) {
	VI2D dp(n + 1, VI(max + 1, 0));
	REP(i, n + 1) {
		REP(j, max + 1) {
			if(i == 0 && j == 0) {
				dp[0][0] = 1;
			} else if(i == 0) {
				dp[0][j] = 0;
			} else if(j == 0) {
				dp[i][0] = 1;
			} else if(j - p[i - 1] < 0) {
				dp[i][j] = dp[i - 1][j];
			} else {
				dp[i][j] = dp[i - 1][j] | dp[i - 1][j - p[i - 1]];
			}
		}
	}

	/*
		REP(i, n + 1) {
			REP(j, max + 1) { printf("%d ", dp[i][j]); }
			putchar('\n');
		}
	*/

	int result = 0;
	REP(i, max + 1) { result += dp[n][i]; }
	return result;
}

int main() {
	int n;
	scanf("%d", &n);
	VI p(n);
	int max = 0;
	EACH(e, p) {
		scanf("%d", &e);
		max += e;
	}
	printf("%d\n", solve(n, p, max));
	return 0;
}