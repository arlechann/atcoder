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

// 迷路探索
// 予め端は壁にしておく
const char wall = '#';	// 壁
const char space = '.'; // 通路
std::vector<std::vector<int>> bfs(const std::vector<std::vector<char>>& maze,
								  vector<vector<pair<int, int>>>& teleporters,
								  std::pair<int, int> s) {
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
		for(size_t i = 0; i < 4; i++) {
			if((p.second + dy[i] < 0 || h <= p.second + dy[i]) ||
			   (p.first + dx[i] < 0 || w <= p.first + dx[i]) ||
			   (distances[p.second + dy[i]][p.first + dx[i]] < INF) ||
			   (maze[p.second + dy[i]][p.first + dx[i]] == wall)) {
				continue;
			}

			distances[p.second + dy[i]][p.first + dx[i]] = d + 1;
			q.push(make_pair(p.first + dx[i], p.second + dy[i]));
		}
		if('a' <= maze[p.second][p.first] && maze[p.second][p.first] <= 'z') {
			EACH(point, teleporters[maze[p.second][p.first] - 'a']) {
				if(distances[point.first][point.second] < INF) {
					continue;
				}

				distances[point.first][point.second] = d + 1;
				q.push(make_pair(point.second, point.first));
			}
			teleporters[maze[p.second][p.first] - 'a'].clear();
		}
	}
	return distances;
}

int main() {
	int h, w;
	cin >> h >> w;
	getchar();

	vector<vector<char>> maze(h, vector<char>(w));
	REP(i, h) {
		REP(j, w) { maze[i][j] = getchar(); }
		getchar();
	}

	vector<vector<pair<int, int>>> teleporters('z' - 'a' + 1);
	pair<int, int> start, goal;
	REP(i, h) {
		REP(j, w) {
			if('a' <= maze[i][j] && maze[i][j] <= 'z') {
				teleporters[maze[i][j] - 'a'].push_back(make_pair(i, j));
			} else if(maze[i][j] == 'S') {
				start = make_pair(j, i);
				maze[i][j] = '.';
			} else if(maze[i][j] == 'G') {
				goal = make_pair(j, i);
				maze[i][j] = '.';
			}
		}
	}

	auto distances = bfs(maze, teleporters, start);

	cout << (distances[goal.second][goal.first] == INF
				 ? -1
				 : distances[goal.second][goal.first])
		 << endl;
	return 0;
}
