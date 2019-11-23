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
	vector<pair<int, int>> strawberry;
	getchar();
	REP(i, h) {
		REP(j, w) {
			char c;
			c = getchar();
			if(c == '#') {
				cake[i][j] = 1;
				strawberry.push_back(pair<int, int>(i, j));
			}
		}
		getchar();
	}
	VI2D result(h, VI(w, 0));
	REP(i, k) { result[strawberry[i].first][strawberry[i].second] = i + 1; }
	REP(i, k) {
		int x1, y1, x2, y2;
		pair<int, int> s = strawberry[i];
		x1 = s.second;
		y1 = s.first;
		x2 = s.second;
		y2 = s.first;

		if(x2 != w - 1) {
			int i = x2 + 1;
			while(true) {
				if(result[y2][i] != 0) {
					x2 = i - 1;
					break;
				}
				if(i == w - 1) {
					x2 = i;
					break;
				}
				i++;
			}
		}

		if(y2 != h - 1) {
			int i = y2 + 1;
			while(true) {
				bool ok = true;
				RANGE(xx, x1, x2 + 1) {
					if(result[i][xx] != 0) {
						ok = false;
						break;
					}
				}
				if(!ok) {
					y2 = i - 1;
					break;
				}
				if(i == h - 1) {
					y2 = i;
					break;
				}
				i++;
			}
		}

		if(x1 != 0) {
			int i = x1 - 1;

			while(true) {
				bool ok = true;
				RANGE(yy, y1, y2 + 1) {
					if(result[yy][i] != 0) {
						ok = false;
						break;
					}
				}
				if(!ok) {
					x1 = i + 1;
					break;
				}
				if(i == 0) {
					x1 = i;
					break;
				}
			}
		}

		if(y1 != 0) {
			int i = y1 - 1;
			while(true) {
				bool ok = true;
				RANGE(xx, x1, x2 + 1) {
					if(result[i][xx] != 0) {
						ok = false;
						break;
					}
				}
				if(!ok) {
					y1 = i + 1;
					break;
				}
				if(i == 0) {
					y1 = i;
					break;
				}
				i--;
			}
		}

		RANGE(ii, y1, y2 + 1) {
			RANGE(jj, x1, x2 + 1) { result[ii][jj] = i + 1; }
		}
	}

	REP(i, h) {
		REP(j, w) { printf("%d ", result[i][j]); }
		putchar('\n');
	}
	return 0;
}
