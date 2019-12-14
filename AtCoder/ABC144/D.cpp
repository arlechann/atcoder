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

#define REP(i, n) for(int i = 0; i < (n); i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) (a).begin(), (a).end()
#define AALL(a, n) (a), a + n
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define MOD(x) ((x) % (1e9 + 7))

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

const double EPS = 1e-10;
const double PI = acos(-1.0);

inline int to_int(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

inline double vol(double rad, double a, double b) {
	if(a * tan(rad) < b) {
		return (a * a * b) - (a * a * a * tan(rad) / 2.0);
	} else {
		return a * b * b / tan(rad) / 2.0;
	}
}

inline double to_degree(double rad) {
	return rad / PI * 180.0;
}

int main() {
	double a, b, x;
	scanf("%lf %lf %lf", &a, &b, &x);
	double l, r, m;
	l = 0.0;
	r = PI / 2.0;
	int count = 0;
	while(count < 1000000) {
		m = (r + l) / 2.0;
		if(vol(m, a, b) < x) {
			r = m;
		} else {
			l = m;
		}
		count++;
	}
	printf("%.10f\n", to_degree(r));
	return 0;
}