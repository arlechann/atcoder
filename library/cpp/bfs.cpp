#include <queue>
#include <vector>

const int INF = 2e9;
const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, -1, 0, 1};

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
