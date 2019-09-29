#include <algorithm>
#include <cmath>
#include <cstdio>
#include <sstream>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0; i < (n); i++)
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

int main(void) {
	int k, x;
	scanf("%d%d", &k, &x);

	vector<int> result;
	for(int i = x - k + 1; i < x + k; i++) {
		if(-1000000 <= i && i <= 1000000) {
			result.push_back(i);
		}
	}

	for(auto&& num : result) {
		printf("%d ", num);
	}
	putchar('\n');
	return 0;
}