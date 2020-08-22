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

constexpr int dx[] = {-2, -2, -2, -2, -2, -1, -1, -1, -1, -1, 0, 0,
					  0,  0,  1,  1,  1,  1,  1,  2,  2,  2,  2, 2};
constexpr int dy[] = {-2, -1, 0,  1,  2, -2, -1, 0,	 1,	 2, -2, -1,
					  1,  2,  -2, -1, 0, 1,	 2,	 -2, -1, 0, 1,	2};

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

struct Node {
	int x;
	int y;
	int cost;

	Node(int x, int y, int c) : x(x), y(y), cost(c) {}
	bool operator<(const Node& rhs) const { return this->cost > rhs.cost; }
};

// 迷路探索
// 予め端は壁にしておく
const char wall = '#';	// 壁
const char space = '.'; // 通路
std::vector<std::vector<int>> bfs(const std::vector<std::vector<char>>& maze,
								  std::pair<int, int> s) {
	const int h = maze.size();
	const int w = maze[0].size();
	std::vector<std::vector<int>> distances(h, std::vector<int>(w, INF));
	distances[s.second][s.first] = 0;
	std::priority_queue<Node> q;
	Node start(s.first, s.second, 0);
	q.push(start);
	while(!q.empty()) {
		Node p = q.top();
		q.pop();
		int d = p.cost;
		for(size_t i = 0; i < 24; i++) {
			if((p.y + dy[i] < 0 || h <= p.y + dy[i]) ||
			   (p.x + dx[i] < 0 || w <= p.x + dx[i]) ||
			   (distances[p.y + dy[i]][p.x + dx[i]] <= d) ||
			   (maze[p.y + dy[i]][p.x + dx[i]] == wall)) {
				continue;
			}

			distances[p.y + dy[i]][p.x + dx[i]] =
				d + ((abs(dx[i]) == 1 && dy[i] == 0) ||
							 (abs(dy[i]) == 1 && dx[i] == 0)
						 ? 0
						 : 1);
			q.push(Node(
				p.x + dx[i], p.y + dy[i], distances[p.y + dy[i]][p.x + dx[i]]));
		}
	}
	return distances;
}

int main() {
	int h, w;
	cin >> h >> w;
	int c[2];
	cin >> c[0] >> c[1];
	c[0]--;
	c[1]--;
	int d[2];
	cin >> d[0] >> d[1];
	d[0]--;
	d[1]--;
	auto s = make_vector({h, w}, '.');
	REP(i, h) {
		string line;
		cin >> line;
		REP(j, w) { s[i][j] = line[j]; }
	}
	auto distances = bfs(s, make_pair(c[1], c[0]));
	cout << (distances[d[0]][d[1]] == INF ? -1 : distances[d[0]][d[1]]) << endl;
	return 0;
}
