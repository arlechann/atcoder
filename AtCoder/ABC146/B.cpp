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
	int n;
	scanf("%d\n", &n);
	char s[10001];
	scanf("%s", s);
	int i = 0;
	while(s[i] != '\n' && s[i] != '\0') {
		s[i] = (((s[i] - 0x41) + n) % 26) + 0x41;
		i++;
	}
	puts(s);
	return 0;
}