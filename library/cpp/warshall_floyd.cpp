#include <algorithm>
#include <vector>

struct Edge {
	bool enable;
	int cost;
	Edge(bool enable, int cost) : enable(enable), cost(cost) {}
};

std::vector<std::vector<int>>
warshall_floyd(std::vector<std::vector<Edge>>& edges) {
	int n = edges.size();
	std::vector<std::vector<int>> distances(n, std::vector<int>(n, INF / 2));
	for(int i = 0; i < n; i++) {
		for(int j = 0; j < n; j++) {
			if(edges[i][j].enable) {
				distances[i][j] = edges[i][j].cost;
			}
		}
	}
	for(int k = 0; k < n; k++) {
		for(int i = 0; i < n; i++) {
			for(int j = 0; j < n; j++) {
				distances[i][j] = std::min(distances[i][j],
										   distances[i][k] + distances[k][j]);
			}
		}
	}
	return distances;
}