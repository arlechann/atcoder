#include <algorithm>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0; i < (n); i++)
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)

using namespace std;

typedef long long ll;

const double EPS = 1e-10;
const double PI = acos(-1.0);

int main(void) {
	int n;
	string s[100000];

	scanf("%d", &n);
	REP(i, n) {
		char cs[11];
		scanf("%s", cs);
		s[i] = string(cs);
	}

	REP(i, n) { sort(s[i].begin(), s[i].end()); }
	sort(s, s + n);

	ll result = 0;
	ll c = 0;
	REP(i, n - 1) {
		if(s[i] == s[i + 1]) {
			c++;
		} else {
			result += c * (c + 1) / 2;
			c = 0;
		}
	}
	result += c * (c + 1) / 2;

	printf("%lld\n", result);
	return 0;
}