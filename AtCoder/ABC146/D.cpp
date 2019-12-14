#include <algorithm>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iostream>
#include <list>
#include <numeric>
#include <queue>
#include <sstream>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < (i##_MACRO); i++)
#define RANGE(i, a, b) for(int i = (a); i < (b); i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) (a).begin(), (a).end()
#define AALL(a, n) (a), ((a) + (n))
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define MODNUM (static_cast<int>(1e9 + 7))
#define MOD(x) ((x) % MODNUM)

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

const int INF = 2e9;
const double EPS = 1e-10;
const double PI = acos(-1.0);

const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, -1, 0, 1};

inline int toInt(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

typedef struct {
	int to_node;
	int color;
} edge;

typedef struct {
	vector<edge> next_nodes;
} node;

int color_num;

void color_tree(vector<node>& nodes, int node, int prev_color) {
	int c = 1;
	EACH(e, nodes[node].next_nodes) {
		c = c % color_num + 1;
		if(c == prev_color) {
			c = c % color_num + 1;
		}
		e.color = c;
		color_tree(nodes, e.to_node, e.color);
	}
}

int main() {
	int n;
	scanf("%d", &n);
	VI a(n - 1);
	VI b(n - 1);
	VI edge_num(n - 1, 0);
	vector<node> nodes(n);
	edge tmp;
	REP(i, n - 1) {
		scanf("%d %d", &a[i], &b[i]);
		edge_num[a[i] - 1]++;
		tmp.to_node = b[i] - 1;
		tmp.color = 0;
		nodes[a[i] - 1].next_nodes.push_back(tmp);
	}
	edge_num[0]--;
	color_num = *max_element(ALL(edge_num)) + 1;

	color_tree(nodes, 0, -1);

	VI edge_count(n - 1, 0);
	printf("%d\n", color_num);
	REP(i, n - 1) {
		printf("%d\n", nodes[a[i] - 1].next_nodes[edge_count[a[i] - 1]].color);
		edge_count[a[i] - 1]++;
	}
	return 0;
}
