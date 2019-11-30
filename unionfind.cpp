#include <numeric>
#include <vector>

using namespace std;

class UnionFind {
	vector<size_t> parents;
	vector<size_t> rank;

	public:
	UnionFind(size_t size) : parents(size), rank(size, 0) {
		iota(this->parents.begin(), this->parents.end(), 0);
	}

	bool merge(size_t a, size_t b) {
		size_t aroot = this->root(a);
		size_t broot = this->root(b);
		if(aroot == broot) {
			return false;
		}
		if(this->rank[a] > this->rank[b]) {
			swap(a, b);
		}
		if(this->rank[a] == this->rank[b]) {
			this->rank[b]++;
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