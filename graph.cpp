#include <queue>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < (i##_MACRO); i++)

using namespace std;

using VI = vector<int>;
using VI2D = vector<vector<int>>;

// コピペここから

using Graph = VI2D;
using Node = int;
using Weight = int;
using Distance = weight;

Distance dijkstra(const Graph& graph, const Node s, const Node g) {
	size_t size = graph.size();
	vector<Distance> dist(size, INF);
	dist[s] = 0;

	priority_queue<pair<Weight, Node>> pq;
	pq.push(pair<Weight, Node>(0, s));

	while(!pq.empty()) {
		const pair<Weight, Node> t = pq.top();
		pq.pop();
		if(dist[t.second] < t.first) {
			continue;
		}

		REP(i, graph[t.second].size()) {
			if(graph[i] > 0 && i != t.second) {
				if(dist[i] > dist[t.second] + graph[t.second][i]) {
					dist[i] = dist[t.second] + graph[t.second][i];
					pq.push(pair<Weight, Node>(dist[i], i));
				}
			}
		}
	}

	return dist[g];
}