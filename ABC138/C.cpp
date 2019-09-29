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
	int n;
	scanf("%d", &n);
	vector<double> v(n);
	for(auto&& e : v) {
		scanf("%lf", &e);
	}

	sort(ALL(v));

	/*
		double result = v.front();
		v.erase(v.begin());
		for(auto i = v.begin(); v.size() >= 2;) {
			while(*i * 2 < i[1]) {
				i += 2;
				if(i == v.end()) {
					i--;
					break;
				}
				i--;
			}

			result = (result + *i) / 2;
			v.erase(i);
		}

		result = (result + v[0]) / 2;
	*/

	double result = v.front();
	v.erase(v.begin());
	for(auto i = v.begin(); !v.empty();) {
		result = (result + *i) / 2;
		v.erase(i);
	}

	printf("%lf\n", result);
	return 0;
}