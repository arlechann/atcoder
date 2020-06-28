#include <algorithm>
#include <boost/optional.hpp>
#include <chrono>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iomanip>
#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <numeric>
#include <queue>
#include <random>
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
#define PRECISION(x) std::fixed << std::setprecision(x)

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

const int I_MAX = 26;

int calc_point(const int& D, const VI& c, const VI2D& s, const VI& t) {
	int point = 0;
	VI last(I_MAX + 1, 0);
	REP(i, D) {
		point += s[i][t[i]];
		last[t[i]] = i + 1;
		int down = 0;
		REP(j, I_MAX) { down += c[j] * ((i + 1) - last[j]); }
		point -= down;
	}

	return point;
}

int main() {
	int D;
	cin >> D;
	VI c(I_MAX);
	REP(i, I_MAX) { cin >> c[i]; }
	VI2D s(D, VI(I_MAX));
	REP(i, D) {
		REP(j, I_MAX) { cin >> s[i][j]; }
	}

	std::random_device seed_gen;
	std::mt19937 engine(seed_gen());

	VI t(D);
	REP(i, D) { t[i] = engine() % I_MAX; }

	std::chrono::system_clock::time_point start, end;
	start = std::chrono::system_clock::now();

	int prev = calc_point(D, c, s, t);
	while(true) {
		end = std::chrono::system_clock::now();
		double elapsed =
			std::chrono::duration_cast<std::chrono::milliseconds>(end - start)
				.count();
		if(elapsed > 1980.0) {
			break;
		}
		int d, q;
		d = engine() % D;
		q = engine() % I_MAX;

		int old = t[d];
		t[d] = q;
		int now = calc_point(D, c, s, t);
		if(prev > now) {
			t[d] = old;
		} else {
			prev = now;
		}
	}

	REP(i, D) { cout << t[i] + 1 << endl; }

	return 0;
}