#include <boost/optional.hpp>
#include <numeric>
#include <vector>

// 重み付きUnionFind
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

	// 併合
	bool merge(size_t a, size_t b, T w) {
		size_t ar = this->root(a);
		size_t br = this->root(b);
		if(ar == br) {
			return false;
		}
		T dw = w + this->weight(a) - this->weight(b);
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

	// 同集合か判定
	bool is_same(size_t a, size_t b) { return this->root(a) == this->root(b); }
	bool is_union(size_t a, size_t b) { return this->is_same(a, b); }

	// 二要素感の距離(同集合に属していなければboost::none)
	optional<T> diff(size_t a, size_t b) {
		if(!this->is_same(a, b)) {
			return nullopt;
		}
		return optional<T>(this->weight(b) - this->weight(a));
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
