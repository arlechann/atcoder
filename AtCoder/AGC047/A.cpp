#include <algorithm>
#include <boost/optional.hpp>
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
using VI = std::vector<int>;
using VI2D = std::vector<vector<int>>;
using VLL = std::vector<long long>;
using VLL2D = std::vector<vector<long long>>;

constexpr int INF = 2e9;
constexpr double EPS = 1e-15;
constexpr double PI = acos(-1.0);

constexpr int dx[] = {-1, 0, 1, 0};
constexpr int dy[] = {0, -1, 0, 1};

template <typename T, std::size_t N>
struct make_vector_type {
	using type =
		typename std::vector<typename make_vector_type<T, (N - 1)>::type>;
};

template <typename T>
struct make_vector_type<T, 0> {
	using type = typename std::vector<T>;
};

template <typename T, size_t N>
auto make_vector_impl(const std::vector<std::size_t>& ls, T init_value) {
	if constexpr(N == 0) {
		return std::vector<T>(ls[N], init_value);
	} else {
		return typename make_vector_type<T, N>::type(
			ls[N], make_vector_impl<T, (N - 1)>(ls, init_value));
	}
}

template <typename T, std::size_t N>
auto make_vector(const std::size_t (&ls)[N], T init_value) {
	std::vector<std::size_t> dimensions(N);
	for(int i = 0; i < N; i++) {
		dimensions[N - i - 1] = ls[i];
	}
	return make_vector_impl<T, N - 1>(dimensions, init_value);
}

template <typename T>
std::vector<T> make_vector(std::size_t size, T init_value) {
	return std::vector<T>(size, init_value);
}

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

int count_div2(ll n) {
	int i = 0;
	while(n % 2 == 0) {
		n /= 2;
		i++;
	}
	return i;
}

int count_div5(ll n) {
	int i = 0;
	while(n % 5 == 0) {
		n /= 5;
		i++;
	}
	return i;
}

int main() {
	int n;
	cin >> n;
	vector<string> a(n);
	REP(i, n) { cin >> a[i]; }

	VLL pow10 = {1LL,
				 10LL,
				 100LL,
				 1000LL,
				 10000LL,
				 100000LL,
				 1000000LL,
				 10000000LL,
				 100000000LL,
				 1000000000LL,
				 10000000000LL};
	vector<pair<int, int>> a2(n);
	REP(i, n) {
		int dot = a[i].find('.');
		int li = 0;
		if(dot != string::npos) {
			li = a[i].size() - dot - 1;
		}
		ll u = stol(a[i].substr(0, dot));
		ll tmp1 = u * pow10[9];
		ll d;
		if(dot == string::npos) {
			d = 0;
		} else {
			d = stol(a[i].substr(dot + 1));
		}
		ll tmp2 = d * pow10[9 - li];
		ll t = tmp1 + tmp2;
		a2[i].first = count_div2(t);
		a2[i].second = count_div5(t);
	}

	auto map = make_vector({50, 50}, 0LL);
	REP(i, n) { map[a2[i].first][a2[i].second]++; }

	// REP(i, 50) {
	// 	REP(j, 50) { cout << map[i][j] << " "; }
	// 	cout << endl;
	// }

	ll result = 0;
	REP(i, 50) {
		REP(j, 50) {
			REP(k, 50) {
				REP(l, 50) {
					if(i + k < 18 || j + l < 18) {
						continue;
					}
					if(k == i && l == j) {
						result += map[i][j] * (map[i][j] - 1);
					} else {
						result += map[k][l] * map[i][j];
					}
				}
			}
		}
	}
	cout << result / 2 << endl;
	return 0;
}
