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
#define MODNUM (998244353)
#define MOD(x) ((x) % MODNUM)

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

const double EPS = 1e-10;
const double PI = acos(-1.0);

inline int toInt(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

long long modpow(long long a, long long n, long long mod) {
	long long res = 1;
	while(n > 0) {
		if(n & 1)
			res = res * a % mod;
		a = a * a % mod;
		n >>= 1;
	}
	return res;
}

int main() {
	int n;
	scanf("%d", &n);
	VI d(n);
	EACH(e, d) { scanf("%d", &e); }
	if(d[0] != 0) {
		puts("0");
		return 0;
	}
	sort(ALL(d));

	int last = 0;
	int lastIndex = 0;
	VI depth_nums(d[n - 1] + 1);
	REP(i, n) {
		if(d[i] != last) {
			depth_nums[last] = i - lastIndex;
			last = d[i];
			lastIndex = i;
		}
	}
	depth_nums[d[n - 1]] = n - lastIndex;

	ll result = 1;
	if(depth_nums[0] != 1) {
		puts("0");
		return 0;
	}
	REP(i, depth_nums.size() - 1) {
		if(depth_nums[i] == 0) {
			puts("0");
			return 0;
		}
		result = MOD(result * modpow(depth_nums[i], depth_nums[i + 1], MODNUM));
	}
	printf("%lld\n", result);
	return 0;
}