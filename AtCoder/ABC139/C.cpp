#include <algorithm>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <queue>
#include <sstream>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0; i < (n); i++)
#define ALL(a) (a).begin(), (a).end()
#define AALL(a, n) (a), a + n
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
	int n;
	scanf("%d", &n);
	vector<int> h(n);
	REP(i, n) { scanf("%d", &h[i]); }
	int m = 0;
	int num = 0;
	REP(i, n - 1) {
		if(h[i] >= h[i + 1]) {
			num++;
		} else {
			m = max(num, m);
			num = 0;
		}
	}
	m = max(num, m);

	printf("%d\n", m);

	return 0;
}