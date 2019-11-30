#include <limits>
#include <queue>
#include <utility>
#include <vector>

const int INF = 2e9;

// コピペここから

using Distance = int;

struct Edge {
	size_t to;
	Distance cost;
	Edge(size_t t, Distance c) : to(t), cost(c) {}
	bool operator<(const Edge& rhs) const { return this->cost > rhs.cost; }
};

using Graph = std::vector<std::vector<Edge>>;

std::vector<Distance> dijkstra(const Graph& graph, const size_t s) {
	size_t n = graph.size();
	std::vector<bool> used(n, false);
	std::vector<Distance> distances(n, INF);

	distances[s] = 0;
	std::priority_queue<Edge> pq;
	pq.push(Edge(s, 0));
	while(!pq.empty()) {
		Edge edge = pq.top();
		pq.pop();
		if(used[edge.to]) {
			continue;
		}
		used[edge.to] = true;
		for(auto&& e : graph[edge.to]) {
			Distance alt = edge.cost + e.cost;
			if(alt < distances[e.to]) {
				distances[e.to] = alt;
				pq.push(Edge(e.to, alt));
			}
		}
	}

	return distances;
}