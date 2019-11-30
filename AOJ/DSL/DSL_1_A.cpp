#include <algorithm>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iostream>
#include <limits>
#include <list>
#include <numeric>
#include <queue>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < i##_MACRO; i++)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
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

class UnionFind {
	vector<size_t> parents;
	vector<size_t> rank;

	public:
	UnionFind(size_t size) : parents(size), rank(size, 0) {
		iota(parents.begin(), parents.end(), 0);
	}

	bool merge(size_t a, size_t b) {
		size_t aroot = this->root(a);
		size_t broot = this->root(b);
		if(aroot == broot) {
			return false;
		}
		if(rank[a] > rank[b]) {
			swap(a, b);
		}
		if(rank[a] == rank[b]) {
			rank[b]++;
		}
		return this->parents[aroot] = broot;
	}

	bool unite(size_t a, size_t b) { return this->merge(a, b); }

	bool is_same(size_t a, size_t b) { return this->root(a) == this->root(b); }
	bool is_union(size_t a, size_t b) { return this->is_same(a, b); }

	private:
	size_t root(int n) {
		if(this->parents[n] == n) {
			return n;
		}

		return this->root(this->parents[n]);
	}
};

int main() {
	int n, q;
	scanf("%d %d", &n, &q);
	VI s(n);
	VI com(q);
	VI x(q);
	VI y(q);
	REP(i, q) { scanf("%d %d %d", &com[i], &x[i], &y[i]); }

	UnionFind uf(n);
	REP(i, q) {
		if(com[i]) {
			printf("%d\n", static_cast<int>(uf.is_same(x[i], y[i])));
		} else {
			uf.merge(x[i], y[i]);
		}
	}
	return 0;
}