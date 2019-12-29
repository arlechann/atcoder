#include <limits>
#include <queue>
#include <utility>
#include <vector>

const int INF = 2e9;

// prim ここから
// dijkstra ここから

using Weight = int;

// 辺
struct Edge {
	size_t from;
	size_t to;
	Weight cost;
	Edge(size_t t, Weight c) : to(t), cost(c) {}
	Edge(size_t f, size_t t, Weight c) : from(f), to(t), cost(c) {}
	bool operator<(const Edge& rhs) const { return this->cost > rhs.cost; }
};

// グラフ G=(V,E)
struct Graph {
	size_t node;
	std::vector<std::vector<Edge>> edges;

	Graph(size_t n) : node(n), edges(n) {}
};

// 最短経路探索(非負閉路)
std::vector<Weight> dijkstra(const Graph& graph, const size_t s) {
	size_t n = graph.node;
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
		for(auto&& e : graph.edges[edge.to]) {
			Weight alt = edge.cost + e.cost;
			if(alt < distances[e.to]) {
				distances[e.to] = alt;
				pq.push(Edge(e.to, alt));
			}
		}
	}
	return distances;
}
// dijkstra ここまで

// 最小全域木
std::pair<Weight, Graph> prim(const Graph& graph, size_t s) {
	size_t n = graph.node;
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
			mst.edges[edge.from].push_back(edge);
		}
		for(auto&& e : graph.edges[edge.to]) {
			if(!used[e.to]) {
				pq.push(e);
			}
		}
	}

	return std::pair<Weight, Graph>(total, mst);
}
// prim ここまで