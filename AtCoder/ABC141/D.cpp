#include <algorithm>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <list>
#include <numeric>
#include <queue>
#include <sstream>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0; i < (n); i++)
#define ALL(a) (a).begin(), (a).end()
#define AALL(a, n) (a), (a) + (n)
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)

using namespace std;

typedef long long ll;

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
	scanf("%d%d", &n, &m);
	priority_queue<int> a;
	REP(i, n) {
		int tmp;
		scanf("%d", &tmp);
		a.push(tmp);
	}

	REP(j, m) {
		bool flag = false;
		int tmp = a.top() / 2;
		a.pop();
		a.push(tmp);
	}

	ll result = 0;
	REP(i, n) {
		result += (ll)a.top();
		a.pop();
	}

	printf("%lld\n", result);

	return 0;
}
