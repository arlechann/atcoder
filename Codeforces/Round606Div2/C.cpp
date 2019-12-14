#include <algorithm>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iostream>
#include <limits>
#include <list>
#include <numeric>
#include <queue>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < i##_MACRO; i++)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) (a).begin(), (a).end()
#define AALL(a, n) (a), ((a) + (n))
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define MODNUM (static_cast<int>(1e9 + 7))
#define MOD(x) ((x) % MODNUM)

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

const int INF = 2e9;
const double EPS = 1e-10;
const double PI = acos(-1.0);

const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, -1, 0, 1};

template <typename T>
int sign(T x) {
	return x < 0 ? -1 : x > 0 ? 1 : 0;
}

template <>
int sign(double x) {
	return x < -EPS ? -1 : x > EPS ? 1 : 0;
}

template <typename T>
T square(T x) {
	return x * x;
}

inline int toInt(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

VI solve(string s) {
	VI result;
	char* twone = "twone";
	char* two = "two";
	char* one = "one";
	int i = 0;
	while(i < s.size()) {
		int j = 0;
		while(true) {
			if(i + j >= s.size() || s[i + j] != twone[j]) {
				if(j != 0) {
					j--;
				}
				break;
			}
			if(j == 4) {
				s.replace(i, 5, "-----");
				result.push_back(i + 2);
				break;
			}
			j++;
		}
		i += j;
		i++;
	}
	i = 0;
	while(i < s.size()) {
		int j = 0;
		char* comp;
		if(s[i] == two[0]) {
			comp = two;
		} else {
			comp = one;
		}
		while(true) {
			if(i + j >= s.size() || s[i + j] != comp[j]) {
				if(j != 0) {
					j--;
				}
				break;
			}
			if(j == 2) {
				s.replace(i, 3, "---");
				result.push_back(i + 1);
				break;
			}
			j++;
		}
		i += j;
		i++;
	}
	return result;
}

int main() {
	int t;
	cin >> t;
	vector<string> s(t);
	REP(i, t) { cin >> s[i]; }

	REP(i, t) {
		VI result = solve(s[i]);
		printf("%d\n", result.size());
		REP(j, result.size()) { printf("%d ", result[j] + 1); }
		putchar('\n');
	}
	return 0;
}