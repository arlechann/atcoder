#include <functional>
#include <vector>

int roundup_pow2(int n) {
	if(!(n & (n - 1))) {
		return n;
	}

	int i = 1;
	while((n >> i) != 0) {
		i++;
	}
	return 1 << i;
}

// セグメント木(一点更新、区間取得)
template <typename T>
class SegmentTree {
	using F = function<T(T, T)>;

	// 演算
	F merge;
	// 単位元
	T identity;
	vector<T> tree;
	size_t size;

	public:
	SegmentTree(const vector<T>& a, const F f, const T id)
		: tree(roundup_pow2(a.size()) * 2 - 1, id),
		  size(roundup_pow2(a.size())), merge(f), identity(id) {
		int offset = this->size - 1;
		for(int i = 0; i < a.size(); i++) {
			this->tree[i + offset] = a[i];
		}
		for(int i = offset - 1; i >= 0; i--) {
			this->tree[i] = this->apply(i);
		}
	}
	// モノイド(Z,+)
	SegmentTree(const vector<T> a)
		: SegmentTree(a, [](T a, T b) { return a + b; }, 0) {}

	// 更新
	// 関数の指定がなければ置き換え
	void update(const size_t index, const T value, const F f = [](T a, T b) {
		return b;
	}) {
		size_t i = index + size - 1;
		this->tree[i] = f(this->tree[i], value);
		while(i > 0) {
			i = (i - 1) / 2;
			this->tree[i] = this->apply(i);
		}
	}

	// 一点取得
	T find(const size_t index) { return this->tree[index + size - 1]; }

	// 区間取得
	T find(const size_t query_left, const size_t query_right) const {
		return this->find_impl(query_left, query_right, 0, 0, this->size);
	}

	private:
	T apply(size_t index) const {
		return this->merge(this->tree[index * 2 + 1],
						   this->tree[index * 2 + 2]);
	}

	T find_impl(size_t query_left,
				size_t query_right,
				size_t node,
				size_t node_left,
				size_t node_right) const {
		if(node_right <= query_left || query_right <= node_left) {
			return this->identity;
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
};
