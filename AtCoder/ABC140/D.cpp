#include <algorithm>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <numeric>
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

int solve(vector<int> v, int depth) {
	int n = v.size();
	int acc = accumulate(ALL(v), 0);

	// printf("n:%d, acc:%d\n", n, acc);

	REP(i, depth) {
		// printf("i:%d, n:%d, acc:%d\n", i, n, acc);
		if(n >= 3) {
			acc += 2;
			n -= 2;
			continue;
		}

		if(n == 2) {
			acc++;
			n--;
			continue;
		}
	}

	return acc;
}

int main() {
	int n, k;
	scanf("%d%d", &n, &k);
	vector<char> s(n);
	getchar();
	for(auto&& e : s) {
		e = getchar();
	}

	vector<int> v;
	int cnt = 0;
	REP(i, n - 1) {
		if(s[i] == s[i + 1]) {
			cnt++;
		} else {
			v.push_back(cnt);
			cnt = 0;
		}
	}
	v.push_back(cnt);

	// REP(i, v.size()) { printf("%d ", v[i]); }

	printf("%d\n", solve(v, k));
	return 0;
}