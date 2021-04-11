#include <algorithm>
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
constexpr long long INFLL = 2e18;
constexpr double EPS = 1e-10;
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
	m = max<T>(m, x);
}

template <typename T, typename U>
constexpr void chmin(T& m, U x) {
	m = min<T>(m, x);
}

template <typename T>
constexpr T square(T x) {
	return x * x;
}

template <typename T>
constexpr T diff(T a, T b) {
	return abs(a - b);
}

void unsolvable() {
	cout << "UNSOLVABLE" << endl;
}

string s[3];
vector<char> used_chars;

bool is_solved(VI a) {
	string si[3];
	REP(i, 3) { si[i] = s[i]; }
	REP(i, 3) {
		REP(k, si[i].size()) {
			REP(j, used_chars.size()) {
				if(si[i][k] == used_chars[j]) {
					si[i][k] = a[j] + '0';
				}
			}
		}
	}

	REP(i, 3) {
		if(si[i][0] == '0') {
			return false;
		}
	}
	ll n[3];
	REP(i, 3) { n[i] = stoll(si[i]); }

	bool result = n[0] + n[1] == n[2];
	if(result) {
		REP(i, 3) { cout << n[i] << endl; }
	}
	return result;
}

bool dfs(VI a, int used, int d) {
	if(d == 0) {
		return is_solved(a);
	}

	REP(i, 10) {
		if((used & (1 << i)) == 0) {
			VI x = a;
			x.push_back(i);
			if(dfs(x, used | (1 << i), d - 1)) {
				return true;
			}
		}
	}
	return false;
}

int main() {
	REP(i, 3) { cin >> s[i]; }

	VI is_used('z' - 'a' + 1, 0);
	REP(i, 3) {
		REP(j, s[i].size()) { is_used[s[i][j] - 'a'] = 1; }
	}

	int count_chars = 0;
	REP(i, 'z' - 'a' + 1) {
		if(is_used[i] == 1) {
			count_chars++;
			used_chars.push_back(i + 'a');
		}
	}
	if(count_chars > 10) {
		unsolvable();
		return 0;
	}

	if(!dfs(VI(), 0, count_chars)) {
		unsolvable();
	}
	return 0;
}
