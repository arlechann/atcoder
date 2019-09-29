#include <algorithm>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <numeric>
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
	vector<int> b(n - 1);
	for(auto&& e : b) {
		scanf("%d", &e);
	}

	vector<int> a(n);
	REP(i, n) {
		if(i == 0) {
			a[i] = b[i];
			continue;
		}

		if(i == n - 1) {
			a[i] = b[i - 1];
			continue;
		}

		a[i] = min(b[i - 1], b[i]);
	}

	int result = accumulate(ALL(a), 0);

	printf("%d\n", result);
	return 0;
}