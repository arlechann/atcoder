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

struct Point {
	int x;
	int y;
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
	}
	return distances;
}

int main() {
	int h, w;
	cin >> h >> w;
	vector<vector<char>> s(h, vector<char>(w));
	int wc = 0;
	REP(i, h) {
		string line;
		cin >> line;
		REP(j, w) {
			s[i][j] = line[j];
			wc += s[i][j] == wall ? 1 : 0;
		}
	}

	if(s[0][0] == '#') {
		cout << -1 << endl;
		return 0;
	}

	VI2D dists = bfs(s, make_pair(0, 0));
	cout << (dists[h - 1][w - 1] == INF
				 ? -1
				 : (h * w - (dists[h - 1][w - 1] + 1) - wc))
		 << endl;
	return 0;
}
