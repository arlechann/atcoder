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
	char s[101];
	scanf("%s", s);
	for(int i = 0; s[i] != '\0'; i++) {
		if(i % 2 == 0) {
			if(!(s[i] == 'R' || s[i] == 'U' || s[i] == 'D')) {
				puts("No");
				return 0;
			}
		} else {
			if(!(s[i] == 'L' || s[i] == 'U' || s[i] == 'D')) {
				puts("No");
				return 0;
			}
		}
	}

	puts("Yes");
	return 0;
}