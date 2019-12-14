#include <algorithm>
#include <boost/optional.hpp>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iostream>
#include <limits>
#include <list>
#include <numeric>
#include <queue>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < i##_MACRO; i++)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
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
	int x;
	scanf("%d", &x);
	int min = x / 105;
	int max = (x + 99) / 100;
	bool result = false;
	for(int a = 0; a <= max; a++) {
		for(int b = 0; a + b <= max; b++) {
			for(int c = 0; a + b + c <= max; c++) {
				for(int d = 0; a + b + c + d <= max; d++) {
					for(int e = 0; a + b + c + d + e <= max; e++) {
						for(int f = min - a - b - c - d - e;
							a + b + c + d + e + f <= max;
							f++) {
							if(a * 100 + b * 101 + c * 102 + d * 103 + e * 104 +
								   f * 105 ==
							   x) {
								result = true;
								goto loop;
							}
						}
					}
				}
			}
		}
	}
loop:
	printf("%d\n", result ? 1 : 0);
	return 0;
}