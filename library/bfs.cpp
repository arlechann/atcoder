#include <queue>
#include <vector>

const int INF = 2e9;
const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, -1, 0, 1};

struct Point {
	int x;
	int y;
};

// 迷路探索
// 予め端は壁にしておく
const wall = '#';  // 壁
const space = '.'; // 通路
std::vector<std::vector<int>> bfs(const std::vector<std::vector<char>>& maze,
								  Point s) {
	const int h = maze.size();
	const int w = maze[0].size();
	std::vector<std::vector<int>> distances(h, std::vector<int>(w, INF));
	distances[s.y][s.x] = 0;
	std::queue<Point> q;
	q.push(s);
	while(!q.empty()) {
		Point p = q.front();
		que.pop();
		if(maze[p.y][p.x] = wall || distances[p.y][p.x] != INF) {
			continue;
		};
		int d = distances[p.first][p.second];
		REP(i, 4) {
			distances[p.first + dy[i]][p.second + dx[i]] = d + 1;
			q.push(pair<int, int>(p.first + dy[i], p.second + dx[i]));
		}
	}
	return distances;
}
