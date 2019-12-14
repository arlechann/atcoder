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
	vector<int> a(n);
	vector<int> b(n);
	vector<int> c(n - 1);
	for(auto&& e : a) {
		scanf("%d", &e);
	}
	for(auto&& e : b) {
		scanf("%d", &e);
	}
	for(auto&& e : c) {
		scanf("%d", &e);
	}

	int result = 0;
	REP(i, n) {
		result += b[a[i] - 1];
		if(i != n - 1) {
			if(a[i] + 1 == a[i + 1]) {
				result += c[a[i] - 1];
			}
		}
	}

	printf("%d\n", result);
	return 0;
}