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

const int dx[] = {1, -1, -1, 1};
const int dy[] = {1, 1, -1, -1};

struct Maze {
	int h;
	int w;
	std::vector<string> m;

	Maze(vector<string> m) : h(m.size()), w(m[0].size()), m(m) {}
	bool is_wall(int y, int x) const { return m[y][x] == '#'; }
};

struct Point {
	int y;
	int x;
	int cost;
	int direct;

	Point(int y, int x, int cost, int direct)
		: y(y), x(x), cost(cost), direct(direct) {}
	Point(int y, int x) : y(y), x(x) {}
	Point() {}

	vector<Point> next(const Maze& m) const {
		vector<Point> ret;
		REP(i, 4) {
			int nx = this->x + dx[i];
			int ny = this->y + dy[i];
			if(ny < 0 || m.h <= ny || nx < 0 || m.w <= nx ||
			   m.is_wall(ny, nx)) {
				continue;
			}
			ret.push_back(Point(
				ny, nx, this->cost + static_cast<int>(i != this->direct), i));
		}
		return ret;
	}

	bool operator<(const Point& rhs) const { return this->cost > rhs.cost; }
};

// 最短経路探索(非負閉路)
std::vector<vector<vector<int>>> dijkstra(
	const Maze& maze, const int sy, const int sx, const int gy, const int gx) {
	int h = maze.h, w = maze.w;
	std::vector<vector<vector<bool>>> used(
		h, vector<vector<bool>>(w, vector<bool>(4, false)));
	std::vector<vector<vector<int>>> distances(h, VI2D(w, VI(4, INF)));

	REP(i, 4) { distances[sy][sx][i] = 0; }
	std::priority_queue<Point> pq;
	REP(i, 4) {
		int nx = sx + dx[i];
		int ny = sy + dy[i];
		if(ny < 0 || h <= ny || nx < 0 || w <= nx || maze.is_wall(ny, nx)) {
			continue;
		}
		distances[ny][nx][i] = 1;
		pq.push(Point(ny, nx, 1, i));
	}
	while(!pq.empty()) {
		Point point = pq.top();
		pq.pop();
		if(used[point.y][point.x][point.direct]) {
			continue;
		}
		used[point.y][point.x][point.direct] = true;
		for(auto&& p : point.next(maze)) {
			int alt = p.cost;
			if(alt < distances[p.y][p.x][p.direct]) {
				distances[p.y][p.x][p.direct] = alt;
				pq.push(p);
			}
		}
	}
	return distances;
}

int main() {
	int n;
	cin >> n;
	int ax, ay, bx, by;
	cin >> ax >> ay;
	cin >> bx >> by;
	ax--;
	ay--;
	bx--;
	by--;
	vector<string> s(n);
	REP(i, n) { cin >> s[i]; }

	if((ax + ay) % 2 != (bx + by) % 2) {
		cout << -1 << endl;
		return 0;
	}

	Maze maze(s);
	auto distances = dijkstra(maze, ax, ay, bx, by);
	int result = INF;
	REP(i, 4) { chmin(result, distances[bx][by][i]); }

	cout << (result == INF ? -1 : result) << endl;
	return 0;
}
