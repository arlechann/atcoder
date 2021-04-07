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

int main() {
	int n;
	cin >> n;
	string s[2];
	cin >> s[0];
	cin >> s[1];

	RANGE(c, 'A', 'Z' + 1) {
		bool is_just = false;
		char d;
		bool finds_oppo = false;
		char oppo;
		REP(i, n) {
			if(s[0][i] == c && isdigit(s[1][i])) {
				is_just = true;
				d = s[1][i];
			} else if(s[1][i] == c && isdigit(s[0][i])) {
				is_just = true;
				d = s[0][i];
			} else if(s[0][i] == c && s[1][i] != c) {
				finds_oppo = true;
				oppo = s[1][i];
			} else if(s[1][i] == c && s[0][i] != c) {
				finds_oppo = true;
				oppo = s[0][i];
			}
		}

		if(is_just) {
			REP(i, n) {
				if(s[0][i] == c) {
					s[0][i] = d;
				}
				if(s[1][i] == c) {
					s[1][i] = d;
				}
			}
		} else if(finds_oppo) {
			REP(i, n) {
				if(s[0][i] == c) {
					s[0][i] = oppo;
				}
				if(s[1][i] == c) {
					s[1][i] = oppo;
				}
			}
		}
	}

	VI used('Z' - 'A' + 1, 0);

	RANGE(i, 1, n) {
		if(!isdigit(s[0][i])) {
			used[s[0][i] - 'A'] = 1;
		}
		if(!isdigit(s[1][i])) {
			used[s[1][i] - 'A'] = 1;
		}
	}
	if(!isdigit(s[0][0])) {
		used[s[0][0] - 'A'] = 2;
	}
	if(!isdigit(s[1][0])) {
		used[s[1][0] - 'A'] = 2;
	}

	ll result = 1;
	REP(i, 'Z' - 'A' + 1) {
		if(used[i] == 1) {
			result *= 10;
		} else if(used[i] == 2) {
			result *= 9;
		}
	}

	cout << result << endl;
	return 0;
}
