#include <numeric>
#include <vector>

using namespace std;

class UnionFind {
	vector<size_t> parents;

	public:
	UnionFind(size_t size) : parents(size) {
		iota(parents.begin(), parents.end(), 0);
	}

	bool merge(size_t a, size_t b) {
		size_t aroot = this->root(a);
		size_t broot = this->root(b);
		if(aroot == broot) {
			return false;
		}
		return parents[aroot] = broot;
	}

	bool is_same(size_t a, size_t b) { return root(a) == root(b); }

	private:
	size_t root(int n) {
		if(this->parents[n] == n) {
			return n;
		}

		return root(this->parents[n]);
	}
};