#include <algorithm>
#include <boost/optional.hpp>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <numeric>
#include <queue>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < i##_MACRO; i++)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) std::begin(a), std::end(a)
#define RALL(a) std::rbegin(a), std::rend(a)
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define INT(x) (static_cast<int>(x))

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

constexpr int INF = 2e9;
constexpr double EPS = 1e-10;
constexpr double PI = acos(-1.0);

constexpr int dx[] = {-1, 0, 1, 0};
constexpr int dy[] = {0, -1, 0, 1};

template <typename T>
constexpr int sign(T x) {
	return x < 0 ? -1 : x > 0 ? 1 : 0;
}

template <>
constexpr int sign(double x) {
	return x < -EPS ? -1 : x > EPS ? 1 : 0;
}

template <typename T, typename U>
constexpr void chmax(T& m, U x) {
	m = max(m, x);
}

template <typename T, typename U>
constexpr void chmin(T& m, U x) {
	m = min(m, x);
}

template <typename T>
constexpr T square(T x) {
	return x * x;
}

const ll first[] = {
	1LL,
	27LL,
	703LL,
	18279LL,
	475255LL,
	12356631LL,
	321272407LL,
	8353082583LL,
	217180147159LL,
	5646683826135LL,
};

string solve(int n, int i) {
	if(i < 0) {
		return string("");
	}
	char s[2];
	s[1] = '\0';
	s[0] = (n / first[i]) + 'a';
	return string(s) + solve(n % first[i], i - 1);
}

int main() {
	ll n;
	cin >> n;
	int i;
	for(i = 9; i >= 0; i--) {
		if(n >= first[i]) {
			break;
		}
	}
	char s[2] = {static_cast<char>((n / first[i]) + 'a' - 1), '\0'};
	cout << string(s) + solve(n % first[i], i - 1) << endl;
	return 0;
}