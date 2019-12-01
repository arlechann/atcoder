#include <limits>
#include <queue>
#include <utility>
#include <vector>

const int INF = 2e9;

// コピペここから

using Weight = int;

struct Edge {
	size_t from;
	size_t to;
	Weight cost;
	Edge(size_t t, Weight c) : to(t), cost(c) {}
	Edge(size_t f, size_t t, Weight c) : from(f), to(t), cost(c) {}
	bool operator<(const Edge& rhs) const { return this->cost > rhs.cost; }
};

using Graph = std::vector<std::vector<Edge>>;

std::vector<Weight> dijkstra(const Graph& graph, const size_t s) {
	size_t n = graph.size();
	std::vector<bool> used(n, false);
	std::vector<Weight> distances(n, INF);

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
			Weight alt = edge.cost + e.cost;
			if(alt < distances[e.to]) {
				distances[e.to] = alt;
				pq.push(Edge(e.to, alt));
			}
		}
	}

	return distances;
}

std::pair<Graph, Weight> prim(const Graph& graph, size_t s) {
	size_t n = graph.size();
	std::vector<bool> used(n, false);
	Weight total = 0;
	Graph mst(n);

	std::priority_queue<Edge> pq;
	pq.push(Edge(-1, s, 0));
	while(!pq.empty()) {
		Edge edge = pq.top();
		pq.pop();
		if(used[edge.to]) {
			continue;
		}
		used[edge.to] = true;
		total += edge.cost;
		if(edge.from != -1) {
			mst[edge.from].push_back(edge);
		}
		for(auto&& e : graph[edge.to]) {
			if(!used[e.to]) {
				pq.push(e);
			}
		}
	}

	return std::pair<Graph, Weight>(mst, total);
}
