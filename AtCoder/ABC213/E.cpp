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
#define RREP(i, n) for(int i = (n)-1; i >= 0; i--)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define RRANGE(i, a, b) for(int i = (b)-1, i##_MACRO = (a); i >= i##_MACRO; i--)
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
constexpr bool chmax(T& m, U x) {
	m = max<T>(m, x);
	return m < x;
}

template <typename T, typename U>
constexpr bool chmin(T& m, U x) {
	m = min<T>(m, x);
	return m > x;
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
constexpr T diff(T a, T b) {
	return abs(a - b);
}

using Maze = vector<string>;

struct Edge {
	int y;
	int x;
};

int dby[] = {-2, -2, -1, -1, 0, 1, 1, 2, 2, 2, 1, 1, 0, -1, -1, -2};
int dbx[] = {0, 1, 1, 2, 2, 2, 1, 1, 0, -1, -1, -2, -2, -2, -1, -1};

// 幅優先探索(重み無しグラフ)
int bfs_01(const Maze& maze, int sy, int sx, int gy, int gx) {
	size_t h = maze.size();
	size_t w = maze[0].size();
	std::vector<std::vector<int>> used(h, std::vector<int>(w, 0));
	std::vector<std::vector<int>> distances(h, std::vector<int>(w, INF));

	distances[sy][sx] = 0;
	std::deque<Edge> q;
	q.push_front({sy, sx});
	while(!q.empty()) {
		Edge edge = q.front();
		q.pop_front();
		if(edge.y == gy && edge.x == gx) {
			// REP(i, h) {
			// 	REP(j, w) {
			// 		if(distances[i][j] == INF) {
			// 			cout << "- ";
			// 		} else {
			// 			cout << distances[i][j] << " ";
			// 		}
			// 	}
			// 	cout << endl;
			// }
			return distances[gy][gx];
		}
		if(used[edge.y][edge.x] != 0) {
			continue;
		}
		used[edge.y][edge.x] = 1;
		REP(i, 4) {
			int ny = edge.y + dy[i];
			int nx = edge.x + dx[i];
			if(ny < 0 || h <= ny || nx < 0 || w <= nx) {
				continue;
			}
			if(maze[ny][nx] == '#') {
				int alt = distances[edge.y][edge.x] + 1;
				if(alt < distances[ny][nx]) {
					distances[ny][nx] = alt;
					q.push_back({ny, nx});
				}
			} else {
				int alt = distances[edge.y][edge.x];
				if(alt < distances[ny][nx]) {
					distances[ny][nx] = alt;
					q.push_front({ny, nx});
				}
			}
		}
		REP(i, 16) {
			int ny = edge.y + dby[i];
			int nx = edge.x + dbx[i];
			if(ny < 0 || h <= ny || nx < 0 || w <= nx) {
				continue;
			}
			int alt = distances[edge.y][edge.x] + 1;
			if(alt < distances[ny][nx]) {
				distances[ny][nx] = alt;
				q.push_back({ny, nx});
			}
		}
	}
	return INF;
}
int main() {
	int h, w;
	cin >> h >> w;
	vector<string> s(h);
	REP(i, h) { cin >> s[i]; }

	cout << bfs_01(s, 0, 0, h - 1, w - 1) << endl;
	return 0;
}
