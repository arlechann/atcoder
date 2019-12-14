#include <algorithm>
#include <cstdio>
#include <vector>

#define REP(i, n) for(int i = 0; i < (n); i++)

using namespace std;

int n, q;
int v[200000];
vector<vector<int>> edge;

void solve(int a, int x) {
	if(v[a] == 0) {
		v[a] += x;
	} else {
		v[a] += x;
		x = v[a];
	}
	for(auto&& e : edge[a]) {
		solve(e, x);
	}
}

int main() {
	scanf("%d %d", &n, &q);

	edge.reserve(n);

	REP(i, n - 1) {
		int a, b;
		scanf("%d %d", &a, &b);
		edge[a - 1].push_back(b - 1);
	}

	REP(i, q) {
		int p, x;
		scanf("%d %d", &p, &x);
		v[p - 1] += x;
	}

	/* REP(i, n + 5) {
		REP(j, n + 5) { printf("%d ", static_cast<int>(edge[i][j])); }
		putchar('\n');
	}

	REP(i, n + 5) { printf("v[%d]:%d ", i, v[i]); }
	putchar('\n'); */

	solve(0, 0);

	REP(i, n) { printf("%d ", v[i]); }
	putchar('\n');

	return 0;
}