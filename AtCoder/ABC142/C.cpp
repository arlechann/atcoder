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
	vector<pair<int, int>> result;
	REP(i, n) {
		int tmp;
		scanf("%d", &tmp);
		result.push_back(make_pair(tmp, i));
	}
	sort(ALL(result));
	for(auto&& e : result) {
		printf("%d ", e.second + 1);
	}
	return 0;
}