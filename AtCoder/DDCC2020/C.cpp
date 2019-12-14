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

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < (i##_MACRO); i++)
#define RANGE(i, a, b) for(int i = (a); i < (b); i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) (a).begin(), (a).end()
#define AALL(a, n) (a), ((a) + (n))
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define MODNUM (static_cast<int>(1e9 + 7))
#define MOD(x) ((x) % MODNUM)

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

const int INF = 2e9;
const double EPS = 1e-10;
const double PI = acos(-1.0);

const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, -1, 0, 1};

inline int toInt(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

int main() {
	int h, w, k;
	scanf("%d %d %d", &h, &w, &k);
	VI2D cake(h, VI(w, 0));
	getchar();
	REP(i, h) {
		REP(j, w) {
			char c;
			c = getchar();
			if(c == '#') {
				cake[i][j] = 1;
			}
		}
		getchar();
	}
	VI2D result(h, VI(w, 0));

	int s = 0;
	REP(i, h) {
		bool empty = true;
		REP(j, w) {
			if(cake[i][j] == 1) {
				s++;
				if(empty) {
					empty = false;
					REP(x, j) { result[i][x] = s; }
				}
			}
			if(!empty) {
				result[i][j] = s;
			}
		}
	}

	REP(i, h) {
		bool empty = true;
		REP(j, w) {
			if(result[i][j] != 0) {
				empty = false;
				break;
			}
		}
		if(empty && i != 0) {
			REP(j, w) { result[i][j] = result[i - 1][j]; }
		}
	}

	for(int i = h - 1; i >= 0; i--) {
		bool empty = true;
		REP(j, w) {
			if(result[i][j] != 0) {
				empty = false;
				break;
			}
		}
		if(empty && i != h - 1) {
			REP(j, w) { result[i][j] = result[i + 1][j]; }
		}
	}

	REP(i, h) {
		REP(j, w) { printf("%d ", result[i][j]); }
		putchar('\n');
	}
	return 0;
}
