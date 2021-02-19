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
constexpr double EPS = 1e-10;
constexpr double PI = acos(-1.0);

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

const int width = 9;
const int length = 4;

const int dy[] = {0, -1, -1, -1, 0, 1, 1, 1};
const int dx[] = {1, 1, 0, -1, -1, -1, 0, 1};

int main() {
	int x, y;
	string w;
	cin >> x >> y >> w;
	x--;
	y--;
	string c[width];
	REP(i, width) { cin >> c[i]; }

	char ext[width * 3][width * 3];
	REP(i, width) {
		REP(j, width) {
			ext[i + width][j + width] = c[i][j];

			ext[i + 1][j + 1] = c[width - i - 1][width - j - 1];
			ext[i + 1][j + width * 2 - 1] = c[width - i - 1][width - j - 1];
			ext[i + width * 2 - 1][j + 1] = c[width - i - 1][width - j - 1];
			ext[i + width * 2 - 1][j + width * 2 - 1] =
				c[width - i - 1][width - j - 1];

			ext[i + 1][j + width] = c[width - i - 1][j];
			ext[i + width * 2 - 1][j + width] = c[width - i - 1][j];

			ext[i + width][j + 1] = c[i][width - j - 1];
			ext[i + width][j + width * 2 - 1] = c[i][width - j - 1];
		}
	}

	string directions[] = {"R", "RU", "U", "LU", "L", "LD", "D", "RD"};
	vector<char> result;
	x += width;
	y += width;
	REP(i, sizeof(directions) / sizeof(directions[0])) {
		if(w == directions[i]) {
			REP(j, length) {
				result.push_back(ext[y][x]);
				x += dx[i];
				y += dy[i];
			}
			break;
		}
	}

	REP(i, length) { cout << result[i]; }
	cout << endl;
	return 0;
}
