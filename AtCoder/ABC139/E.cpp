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
	vector<queue<int>> a(n);
	REP(i, n) {
		REP(j, n - 1) {
			int input;
			scanf("%d", &input);
			a[i].push(input - 1);
		}
	}

	int i = 0;
	int day = 0;
	bool loop = true;
	int used[1000];
	while(loop) {
		loop = false;
		FILLZ(used);
		REP(i, n) {
			if(!used[i] && !a[i].empty()) {
				int opponent = a[i].front();
				// printf("i:%d opponent:%d\n", i, opponent);
				if(!a[opponent].empty()) {
					if(i == a[opponent].front()) {
						a[i].pop();
						a[opponent].pop();
						used[opponent] = 1;
						loop = true;
					}
				}
			}
		}
		day++;
	}

	REP(i, n) {
		if(!a[i].empty()) {
			puts("-1");
			exit(0);
		}
	}

	printf("%d\n", day);

	return 0;
}