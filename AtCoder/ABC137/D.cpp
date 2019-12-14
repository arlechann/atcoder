#include <algorithm>
#include <cmath>
#include <cstdio>
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

int main(void) {
	int n, m;
	pair<int, int> ab[100000];

	scanf("%d%d", &n, &m);
	REP(i, n) { scanf("%d%d", &ab[i].second, &ab[i].first); }

	sort(ab, ab + n, [](auto&& a, auto&& b) { return a.second < b.second; });

	int w = 0;
	int result = 0;
	priority_queue<pair<int, int>> pq;
	REP(i, m) {
		while(ab[w].second == i + 1) {
			pq.push(ab[w]);
			w++;
		}

		if(!pq.empty()) {
			auto today = pq.top();
			pq.pop();
			result += today.first;
		}
	}

	printf("%d\n", result);

	return 0;
}
