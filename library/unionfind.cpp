#include <boost/optional.hpp>
#include <numeric>
#include <vector>

class UnionFind {
	std::vector<size_t> parents;
	std::vector<size_t> rank;

	public:
	UnionFind(size_t size) : parents(size), rank(size, 0) {
		std::iota(this->parents.begin(), this->parents.end(), 0);
	}

	bool merge(size_t a, size_t b) {
		size_t ar = this->root(a);
		size_t br = this->root(b);
		if(ar == br) {
			return false;
		}
		if(this->rank[ar] < this->rank[br]) {
			std::swap(ar, br);
		}
		if(this->rank[ar] == this->rank[br]) {
			this->rank[ar]++;
		}
		return this->parents[br] = ar;
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

template <typename T = int>
class WeightedUnionFind {
	std::vector<size_t> parents;
	std::vector<size_t> rank;
	std::vector<T> diff_weight;
	T identity;

	public:
	WeightedUnionFind(size_t size, T id = static_cast<T>(0))
		: parents(size), rank(size, 0), diff_weight(size, id), identity(id) {
		std::iota(this->parents.begin(), this->parents.end(), 0);
	}

	bool merge(size_t a, size_t b, T w) {
		size_t ar = this->root(a);
		size_t br = this->root(b);
		T dw = w + weight(a) - weight(b);
		if(ar == br) {
			return false;
		}
		if(this->rank[ar] < this->rank[br]) {
			std::swap(ar, br);
			dw = -dw;
		}
		if(this->rank[ar] == this->rank[br]) {
			this->rank[ar]++;
		}
		this->diff_weight[br] = dw;
		this->parents[br] = ar;
		return true;
	}
	bool unite(size_t a, size_t b, T w) { return this->merge(a, b, w); }

	bool is_same(size_t a, size_t b) { return this->root(a) == this->root(b); }
	bool is_union(size_t a, size_t b) { return this->is_same(a, b); }

	boost::optional<T> diff(size_t a, size_t b) {
		if(!this->is_same(a, b)) {
			return boost::none;
		}
		return boost::optional<T>(this->weight(b) - this->weight(a));
	}

	private:
	size_t root(int n) {
		if(this->parents[n] == n) {
			return n;
		}

		size_t r = this->root(this->parents[n]);
		this->diff_weight[n] += this->diff_weight[this->parents[n]];
		this->parents[n] = r;
		return r;
	}

	T weight(size_t n) {
		root(n);
		return this->diff_weight[n];
	}
};