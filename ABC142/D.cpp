#include <algorithm>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iostream>
#include <list>
#include <map>
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

map<ll, int> factor(ll n) {
	map<ll, int> ret;
	for(ll i = 2; i * i <= n; i++) {
		while(n % i == 0) {
			ret[i]++;
			n /= i;
		}
	}
	if(n != 1) {
		ret[n] = 1;
	}
	return ret;
}

int main() {
	ll a, b;
	scanf("%lld %lld", &a, &b);

	vector<ll> v1;
	for(auto e : factor(a)) {
		v1.push_back(e.first);
	}
	vector<ll> v2;
	for(auto e : factor(b)) {
		v2.push_back(e.first);
	}
	int result = 0;
	int i = 0;
	int j = 0;
	while(i < v1.size() && j < v1.size()) {
		if(v1[i] == v2[j]) {
			result++;
			i++;
			j++;
		} else if(v1[i] < v2[j]) {
			i++;
		} else {
			j++;
		}
	}
	printf("%d\n", result + 1);

	return 0;
}