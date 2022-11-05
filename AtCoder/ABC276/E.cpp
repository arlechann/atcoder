#include <algorithm>
#include <cassert>
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
#define RREP(i, n) for(int i = (n)-1; i >= 0; i--)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define RRANGE(i, a, b) for(int i = (b)-1, i##_MACRO = (a); i >= i##_MACRO; i--)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) std::begin(a), std::end(a)
#define RALL(a) std::rbegin(a), std::rend(a)
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define CAST(x, t) (static_cast<t>(x))
#define PRECISION(x) std::fixed << std::setprecision(x)

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;
using VLL = vector<long long>;
using VLL2D = vector<vector<long long>>;

constexpr int INF = 2e9;
constexpr long long INFLL = 2e18;
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

template <typename T>
constexpr bool chmax(T& m, T x) {
	if(m >= x) {
		return false;
	}
	m = x;
	return true;
}

template <typename T>
constexpr bool chmin(T& m, T x) {
	if(m <= x) {
		return false;
	}
	m = x;
	return true;
}

template <typename T>
constexpr T square(T x) {
	return x * x;
}

template <typename T>
constexpr T pow(T a, int n) {
	T ret = 1;
	while(n != 0) {
		if(n % 2) {
			ret *= a;
		}
		a *= a;
		n /= 2;
	}
	return ret;
}

template <typename T>
constexpr T div_ceil(T a, T b) {
	assert(b != 0);
	if(a < 0 && b < 0) {
		a = -a;
		b = -b;
	}
	if(a >= 0 && b > 0) {
		return (a + b - 1) / b;
	}
	return a / b;
}

template <typename T>
constexpr T div_floor(T a, T b) {
	assert(b != 0);
	if(a < 0 && b < 0) {
		a = -a;
		b = -b;
	}
	if(a >= 0 && b > 0) {
		return a / b;
	}
	assert(false);
}

template <typename T>
constexpr bool is_power_of_two(T n) {
	if constexpr(n == std::numeric_limits<T>::min()) {
		return true;
	}
	return (n & (n - 1)) == 0;
}

constexpr std::size_t next_power_of_two(std::size_t n) {
	if((n & (n - 1)) == 0) {
		return n;
	}
	std::size_t ret = 1;
	while(n != 0) {
		ret <<= 1;
		n >>= 1;
	}
	return ret;
}

template <typename T>
constexpr T next_multiple_of(T a, T b) {
	return div_ceil(a, b) * b;
}

template <typename T>
constexpr bool is_mul_overflow(T a, T b) {
	if(a >= 0 && b >= 0) {
		return a > std::numeric_limits<T>::max() / b;
	}
	if(a <= 0 && b < 0) {
		return a < div_ceil(std::numeric_limits<T>::max(), b);
	}
	if(a < 0) {
		return a > std::numeric_limits<T>::min() / b;
	}
	if(b < 0) {
		return a < div_ceil(std::numeric_limits<T>::max(), b);
	}
}

template <typename T>
constexpr T diff(T a, T b) {
	return max(a, b) - min(a, b);
}

const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, -1, 0, 1};

// 迷路探索
// 予め端は壁にしておく
const char wall = '#';	// 壁
const char space = '.'; // 通路
int bfs(const std::vector<std::vector<char>>& maze,
		std::pair<int, int> s,
		std::pair<int, int> g) {
	const int h = maze.size();
	const int w = maze[0].size();
	std::vector<std::vector<int>> distances(h, std::vector<int>(w, INF));
	distances[s.second][s.first] = 0;
	std::queue<std::pair<int, int>> q;
	q.push(s);
	while(!q.empty()) {
		std::pair<int, int> p = q.front();
		q.pop();
		int d = distances[p.second][p.first];
		if(p == g) {
			return d;
		}
		for(size_t i = 0; i < 4; i++) {
			if((p.second + dy[i] < 0 || h <= p.second + dy[i]) ||
			   (p.first + dx[i] < 0 || w <= p.first + dx[i]) ||
			   (distances[p.second + dy[i]][p.first + dx[i]] < INF) ||
			   (maze[p.second + dy[i]][p.first + dx[i]] != space)) {
				continue;
			}
			distances[p.second + dy[i]][p.first + dx[i]] = d + 1;
			q.push(make_pair(p.first + dx[i], p.second + dy[i]));
		}
	}
	return INF;
}

/**
 *  _       _                     _        ____
 * (_)_ __ | |_   _ __ ___   __ _(_)_ __  / /\ \ _
 * | | '_ \| __| | '_ ` _ \ / _` | | '_ \| |  | (_)
 * | | | | | |_  | | | | | | (_| | | | | | |  | |_
 * |_|_| |_|\__| |_| |_| |_|\__,_|_|_| |_| |  | ( )
 *                                        \_\/_/|/
 */

int main() {
	int h, w;
	cin >> h >> w;
	vector<string> c(h);
	REP(i, h) {
		cin >> c[i];
	}

	vector<vector<char>> maze(h + 2, vector<char>(w + 2, wall));
	int s_x, s_y;
	REP(i, h) {
		REP(j, w) {
			maze[i + 1][j + 1] = c[i][j];
			if(c[i][j] == 'S') {
				s_y = i + 1;
				s_x = j + 1;
				maze[i + 1][j + 1] = wall;
			}
		}
	}

	REP(i, 4) {
		int start_y = s_y + dy[i];
		int start_x = s_x + dx[i];
		if(maze[start_y][start_x] == wall) {
			continue;
		}
		REP(j, 4) {
			int goal_y = s_y + dy[j];
			int goal_x = s_x + dx[j];
			if(maze[goal_y][goal_x] == wall) {
				continue;
			}
			if((start_y < goal_y) || (start_y == goal_y && start_x <= goal_x)) {
				continue;
			}
			int distance = bfs(maze, {start_x, start_y}, {goal_x, goal_y});
			if(distance < INF) {
				cout << "Yes" << endl;
				return 0;
			}
		}
	}
	cout << "No" << endl;
	return 0;
}
