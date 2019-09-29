#include <algorithm>
#include <cmath>
#include <cstdio>
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
	vector<double> v(n);
	for(auto&& e : v) {
		scanf("%lf", &e);
	}

	double result = 0.0;
	for(auto&& e : v) {
		result += 1 / e;
	}

	printf("%lf", 1 / result);
	return 0;
}