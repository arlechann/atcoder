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
	int n;
	scanf("%d", &n);
	VI l(n);
	EACH(e, l) { scanf("%d", &e); }

	sort(ALL(l));
	int result = 0;
	for(auto i = l.begin() + 2; i < l.end(); i++) {
		for(auto j = l.begin(); j < i - 1; j++) {
			int lower = *i - *j + 1;
			int upper = *i + *j - 1;
			result += distance(lower_bound(j + 1, i, lower),
							   upper_bound(j + 1, i, upper));
		}
	}

	printf("%d\n", result);
	return 0;
}