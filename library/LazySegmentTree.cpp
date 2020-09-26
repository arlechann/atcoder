#include <functional>
#include <vector>

int roundup_pow2(int n) {
	if(!(n & (n - 1))) {
		return n;
	}

	int ret = 1;
	while(n > ret) {
		ret <<= 1;
	}
	return ret;
}

template <typename T, typename U>
class LazySegmentTree {
	using F = std::function<T(T, T)>;
	using G = std::function<T(T, U)>;
	using H = std::function<U(U, U)>;

	std::vector<T> tree;
	std::vector<U> lazy;
	F merge;
	G mapping;
	H composition;
	T id1;
	U id2;
	std::size_t size;

	public:
	LazySegmentTree(const vector<T>& a,
					const F f,
					const T id1,
					const G g,
					const H h,
					const U id2)
		: tree(roundup_pow2(a.size()) * 2 - 1, id1),
		  lazy(roundup_pow2(a.size()) * 2 - 1, id2), merge(f), id1(id1),
		  mapping(g), composition(h), id2(id2), size(roundup_pow2(a.size())) {
		int offset = this->size - 1;
		for(int i = 0; i < a.size(); i++) {
			this->tree[i + offset] = a[i];
		}
		for(int i = offset - 1; i >= 0; i--) {
			this->tree[i] =
				this->merge(this->tree[i * 2 + 1], this->tree[i * 2 + 2]);
		}
	}

	void debug_print() {
		cout << "tree: ";
		REP(i, this->size * 2 - 1) {
			if(i == this->size - 1) {
				cout << "| ";
			}
			cout << this->tree[i] << " ";
		}
		cout << "\nlazy: ";
		REP(i, this->size * 2 - 1) {
			if(i == this->size - 1) {
				cout << "| ";
			}
			cout << this->lazy[i] << " ";
		}
		cout << endl;
	}

	void
	update(const std::size_t left, const std::size_t right, const U value) {
		this->update_impl(left, right, 0, 0, this->size, value);
	}

	T find(const std::size_t left, const std::size_t right) {
		return this->find_impl(left, right, 0, 0, this->size);
	}

	private:
	void update_impl(std::size_t query_left,
					 std::size_t query_right,
					 std::size_t node,
					 std::size_t node_left,
					 std::size_t node_right,
					 U value) {
		this->force(node);
		if(node_right <= query_left || query_right <= node_left) {
			return;
		}
		if(query_left <= node_left && node_right <= query_right) {
			this->lazy[node] = this->composition(this->lazy[node], value);
			this->force(node);
			return;
		}
		this->update_impl(query_left,
						  query_right,
						  node * 2 + 1,
						  node_left,
						  node_left + (node_right - node_left) / 2,
						  value);
		this->update_impl(query_left,
						  query_right,
						  node * 2 + 2,
						  node_left + (node_right - node_left) / 2,
						  node_right,
						  value);
	}

	T find_impl(size_t query_left,
				size_t query_right,
				size_t node,
				size_t node_left,
				size_t node_right) {
		this->force(node);
		if(node_right <= query_left || query_right <= node_left) {
			return this->id1;
		}
		if(query_left <= node_left && node_right <= query_right) {
			return this->tree[node];
		}

		return this->merge(find_impl(query_left,
									 query_right,
									 node * 2 + 1,
									 node_left,
									 node_left + (node_right - node_left) / 2),
						   find_impl(query_left,
									 query_right,
									 node * 2 + 2,
									 node_left + (node_right - node_left) / 2,
									 node_right));
	}

	void force(std::size_t node) {
		if(this->lazy[node] == this->id2) {
			return;
		}
		if(node * 2 + 1 < this->size * 2 - 1) {
			this->lazy[node * 2 + 1] =
				this->composition(lazy[node * 2 + 1], lazy[node]);
			this->lazy[node * 2 + 2] =
				this->composition(lazy[node * 2 + 2], lazy[node]);
		}
		this->tree[node] = this->mapping(this->tree[node], lazy[node]);
		this->lazy[node] = this->id2;
	}
};