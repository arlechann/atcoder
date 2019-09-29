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
	int n, k, q;
	scanf("%d %d %d", &n, &k, &q);
	vector<int> a(q);
	for(auto&& e : a) {
		scanf("%d", &e);
	}

	vector<int> players(n, 0);
	for(auto&& e : a) {
		players[e - 1]++;
	}

	for(auto&& score : players) {
		puts(k - (q - score) > 0 ? "Yes" : "No");
	}

	return 0;
}