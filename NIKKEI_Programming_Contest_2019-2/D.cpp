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

int main() {
	int n, m;
	scanf("%d %d", &n, &m);
	VI l(m);
	VI r(m);
	VI c(m);
	REP(i, m) { scanf("%d %d %d", &l[i], &r[i], &c[i]); }

	VI2D graph(n, VI(n, 0));

	priority_queue<pair<int, int>> pq;
	REP(i, n) {
		if(i != 0) {
			pq.push(make_pair(graph[0][i], i));
		}
	}

	return 0;
}