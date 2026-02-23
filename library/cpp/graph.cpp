#include <algorithm>
#include <limits>
#include <queue>
#include <string>
#include <utility>
#include <vector>

template <typename W>
struct Edge {
	std::size_t from;
	std::size_t to;
	W weight;

	Edge() {}
	Edge(std::size_t from, std::size_t to, W weight)
		: from(from), to(to), weight(weight) {}
	Edge(std::size_t from, std::size_t to)
		: from(from), to(to), weight(static_cast<W>(1)) {}
};

template <typename W>
struct ListGraph {
	using Weight = W;

	inline static constexpr Weight INF = std::numeric_limits<Weight>::max() / 2;

	private:
	std::size_t _size;

	public:
	std::vector<std::vector<Edge<Weight>>> edges;

	ListGraph(std::size_t size) : _size(size), edges(size) {}

	std::size_t size() const { return this->_size; }

	void add_edge(std::size_t from, std::size_t to, Weight weight) {
		this->edges[from].push_back(Edge(from, to, weight));
	}

	void add_edge(std::size_t from, std::size_t to) {
		this->edges[from].push_back(Edge<Weight>(from, to));
	}

	bool has_edge(std::size_t from, std::size_t to) const {
		for(const auto& e : this->edges[from]) {
			if(e.to == to) {
				return true;
			}
		}
		return false;
	}

	Weight weight(std::size_t from, std::size_t to) const {
		Weight weight = this->INF;
		for(const auto& e : this->edges[from]) {
			if(e.to == to) {
				weight = min(weight, e.weight);
			}
		}
		return weight;
	}

	std::vector<Edge<Weight>> all_edges() const {
		std::vector<Edge<Weight>> edges;
		for(auto&& ns : this->edges) {
			for(auto&& e : ns) {
				edges.push_back(e);
			}
		}
		return edges;
	}

	std::vector<Edge<Weight>> neighbors(std::size_t n) const {
		return this->edges[static_cast<std::size_t>(n)];
	}

	std::vector<std::size_t> indegrees() const {
		std::vector<std::size_t> indegrees(this->_size, 0);
		for(auto&& ns : this->edges) {
			for(auto&& e : ns) {
				indegrees[e.to]++;
			}
		}
		return indegrees;
	}
};

template <typename W>
struct MatrixGraph {
	using Weight = W;

	inline static constexpr Weight INF = std::numeric_limits<Weight>::max() / 2;

	private:
	std::size_t _size;

	public:
	std::vector<std::vector<bool>> edges;
	std::vector<std::vector<Weight>> weights;

	MatrixGraph(std::size_t size)
		: _size(size), edges(size, std::vector(size, false)),
		  weights(size, std::vector(size, static_cast<Weight>(0))) {}

	std::size_t size() const { return this->_size; }

	void add_edge(std::size_t from, std::size_t to, Weight weight) {
		this->edges[from][to] = true;
		this->weights[from][to] = weight;
	}

	void add_edge(std::size_t from, std::size_t to) {
		this->edges[from][to] = true;
		this->weights[from][to] = static_cast<Weight>(1);
	}

	void remove_edge(std::size_t from, std::size_t to) {
		this->edges[from][to] = false;
		this->weights[from][to] = static_cast<Weight>(0);
	}

	bool has_edge(std::size_t from, std::size_t to) const {
		return this->edges[from][to];
	}

	Weight weight(std::size_t from, std::size_t to) const {
		return this->weights[from][to];
	}

	std::vector<Edge<Weight>> all_edges() const {
		std::vector<Edge<Weight>> edges;
		for(std::size_t i = 0; i < this->_size; i++) {
			for(std::size_t j = 0; j < this->_size; j++) {
				if(this->edges[i][j]) {
					edges.push_back(Edge(i, j, this->weights[i][j]));
				}
			}
		}
		return edges;
	}

	std::vector<Edge<Weight>> neighbors(std::size_t n) const {
		std::vector<Edge<Weight>> neighbors;
		for(std::size_t i = 0; i < this->_size; i++) {
			if(this->edges[n][i]) {
				neighbors.push_back(Edge(n, i, this->weights[n][i]));
			}
		}
		return neighbors;
	}

	std::vector<std::size_t> indegrees() const {
		std::vector<std::size_t> indegrees(this->_size, 0);
		for(std::size_t i = 0; i < this->_size; i++) {
			for(std::size_t j = 0; j < this->_size; j++) {
				if(this->edges[i][j]) {
					indegrees[j]++;
				}
			}
		}
		return indegrees;
	}
};

struct Undirected {
	template <typename G>
	void add_edge(G& graph, std::size_t u, std::size_t v) const {
		graph.add_edge(u, v);
		graph.add_edge(v, u);
	}

	template <typename G, typename W>
	void add_edge(G& graph, std::size_t u, std::size_t v, W weight) const {
		graph.add_edge(u, v, weight);
		graph.add_edge(v, u, weight);
	}

	bool is_directed() const { return false; }
};

struct Directed {
	template <typename G>
	void add_edge(G& graph, std::size_t from, std::size_t to) const {
		graph.add_edge(from, to);
	}

	template <typename G, typename W>
	void add_edge(G& graph, std::size_t from, std::size_t to, W weight) const {
		graph.add_edge(from, to, weight);
	}

	bool is_directed() const { return true; }
};

template <typename G, typename D>
struct Graph {
	using Weight = typename G::Weight;

	inline static constexpr Weight INF = std::numeric_limits<Weight>::max() / 2;

	D directed;
	G graph;

	Graph(std::size_t n) : graph(n) {}
	Graph(std::size_t n, std::vector<int> u, std::vector<int> v) : graph(n) {
		std::size_t m = u.size();
		for(std::size_t i = 0; i < m; i++) {
			this->directed.add_edge(this->graph, u[i], v[i]);
		}
	}
	Graph(std::size_t n,
		  std::vector<int> u,
		  std::vector<int> v,
		  std::vector<Weight> c)
		: graph(n) {
		std::size_t m = u.size();
		for(std::size_t i = 0; i < m; i++) {
			this->directed.add_edge(this->graph, u[i], v[i], c[i]);
		}
	}

	size_t size() const { return this->graph.size(); }

	void add_edge(std::size_t u, std::size_t v) {
		this->directed.add_edge(this->graph, u, v);
	}

	void add_edge(std::size_t u, std::size_t v, Weight c) {
		this->directed.add_edge(this->graph, u, v, c);
	}

	bool has_edge(std::size_t from, std::size_t to) const {
		return this->graph.has_edge(from, to);
	}

	Weight weight(std::size_t from, std::size_t to) const {
		return this->graph.weight(from, to);
	}

	std::vector<Edge<Weight>> edges() const { return this->graph.all_edges(); }

	std::vector<Edge<Weight>> neighbors(std::size_t n) const {
		return this->graph.neighbors(n);
	}

	std::vector<std::size_t> indegrees() const {
		return this->graph.indegrees();
	}
};

template <typename G, typename D>
struct Dot {
	using Weight = typename G::Weight;
	Graph<G, D> graph;

	Dot(Graph<G, D> graph) : graph(graph) {}

	void write(string name) const {
		if(graph.directed.is_directed()) {
			cout << "digraph " << name << " {" << endl;
		} else {
			cout << "graph " << name << " {" << endl;
		}

		std::size_t size = this->graph.size();
		for(int i = 0; i < size; i++) {
			if(i != 0) {
				cout << ", ";
			}
			cout << to_string(i);
		}
		cout << ";" << endl;

		std::vector<Edge<Weight>> edges = graph.edges();
		for(auto&& e : edges) {
			if(graph.directed.is_directed()) {
				cout << e.from << " -> " << e.to << " [" << endl;
			} else {
				if(e.from > e.to) {
					continue;
				}
				cout << e.from << " -- " << e.to << " [" << endl;
			}
			cout << "label = " << e.weight << "," << endl;
			cout << "weight = " << e.weight << endl;
			cout << "];" << endl;
		}

		cout << "}" << endl;
	}
};

// 幅優先探索(重み無しグラフ)
template <typename G>
std::vector<typename G::Weight> bfs(const G& graph, const size_t s) {
	using Weight = typename G::Weight;
	std::size_t n = graph.size();
	std::vector<bool> used(n, false);
	std::vector<Weight> distances(n, G::INF);

	distances[s] = 0;
	used[s] = true;
	std::queue<std::size_t> que;
	que.push(s);
	while(!que.empty()) {
		std::size_t node = que.front();
		que.pop();
		for(auto&& edge : graph.neighbors(node)) {
			if(used[edge.to]) {
				continue;
			}
			distances[edge.to] = distances[node] + 1;
			used[edge.to] = true;
			que.push(edge.to);
		}
	}
	return distances;
}

// トポロジカルソート
template <typename G>
std::vector<std::size_t> topological_sort(const Graph<G, Directed>& graph) {
	int n = graph.size();
	std::vector<std::size_t> indegrees = graph.indegrees();
	std::vector<std::size_t> sorted;
	std::vector<bool> is_used(n, false);

	std::queue<std::size_t> que;
	for(std::size_t i = 0; i < n; i++) {
		if(indegrees[i] == 0) {
			que.push(i);
		}
	}

	while(!(que.empty() || sorted.size() > n)) {
		std::size_t node = que.front();
		que.pop();
		sorted.push_back(node);
		is_used[node] = true;
		for(auto&& edge : graph.neighbors(node)) {
			indegrees[edge.to]--;
			if(!is_used[edge.to] && indegrees[edge.to] == 0) {
				que.push(edge.to);
			}
		}
	}
	return sorted;
}

// 最短経路探索(非負閉路)
template <typename G>
std::vector<typename G::Weight>
dijkstra(const G& graph, const std::size_t s, const std::size_t g) {
	using Weight = typename G::Weight;
	using Node = std::pair<Weight, std::size_t>;
	std::size_t n = graph.size();
	std::vector<bool> used(n, false);
	std::vector<Weight> distances(n, G::INF);

	distances[s] = 0;
	std::priority_queue<Node, vector<Node>, greater<Node>> pq;
	pq.push(make_pair(static_cast<Weight>(0), s));
	while(!pq.empty()) {
		auto [distance, node] = pq.top();
		pq.pop();
		if(used[node]) {
			continue;
		}
		if(node == g) {
			break;
		}
		used[node] = true;
		for(auto&& e : graph.neighbors(node)) {
			Weight alt = e.weight + distance;
			if(alt < distances[e.to]) {
				distances[e.to] = alt;
				pq.push(make_pair(alt, e.to));
			}
		}
	}
	return distances;
}

// 最小全域木
template <typename G>
std::pair<typename G::Weight, G> prim(const G& graph, size_t s) {
	using Weight = typename G::Weight;
	using Node = std::pair<Weight, std::size_t>;
	size_t n = graph.size();
	std::vector<bool> used(n, false);
	Weight total = 0;
	G mst(n);

	auto compare = [](Edge<Weight> a, Edge<Weight> b) {
		return a.weight > b.weight;
	};

	std::priority_queue<Edge<Weight>, vector<Edge<Weight>>, decltype(compare)>
		pq(compare);
	pq.push(Edge(-1, s, 0));
	while(!pq.empty()) {
		Edge edge = pq.top();
		pq.pop();
		if(used[edge.to]) {
			continue;
		}
		used[edge.to] = true;
		total += edge.weight;
		if(edge.from != -1) {
			mst.add_edge(edge.from, edge.to, edge.weight);
		}
		for(auto&& e : graph.neighbors(edge.to)) {
			if(!used[e.to]) {
				pq.push(e);
			}
		}
	}

	return std::pair<Weight, G>(total, mst);
}

template <typename G>
std::vector<std::vector<typename G::Weight>> warshall_floyd(const G& graph) {
	using Weight = typename G::Weight;
	int n = graph.size();
	std::vector<std::vector<Weight>> distances(
		n, std::vector<Weight>(n, G::INF / 2));
	for(int i = 0; i < n; i++) {
		for(int j = 0; j < n; j++) {
			if(graph.has_edge(i, j)) {
				distances[i][j] = graph.weight(i, j);
			}
		}
	}
	for(int i = 0; i < n; i++) {
		distances[i][i] = 0;
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

using GraphWeight = int;
using DirectedListGraph = Graph<ListGraph<GraphWeight>, Directed>;
using UndirectedListGraph = Graph<ListGraph<GraphWeight>, Undirected>;
using DirectedMatrixGraph = Graph<MatrixGraph<GraphWeight>, Directed>;
using UndirectedMatrixGraph = Graph<ListGraph<GraphWeight>, Undirected>;
