#include <algorithm>
#include <boost/optional.hpp>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <numeric>
#include <queue>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < i##_MACRO; i++)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) std::begin(a), std::end(a)
#define RALL(a) std::rbegin(a), std::rend(a)
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define INT(x) (static_cast<int>(x))

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

constexpr int INF = 2e9;
constexpr double EPS = 1e-10;
constexpr double PI = acos(-1.0);

constexpr int dx[] = {-1, 0, 1, 0};
constexpr int dy[] = {0, -1, 0, 1};

template <typename T>
constexpr int sign(T x) {
	return x < 0 ? -1 : x > 0 ? 1 : 0;
}

template <>
constexpr int sign(double x) {
	return x < -EPS ? -1 : x > EPS ? 1 : 0;
}

template <typename T, typename U>
constexpr void chmax(T& m, U x) {
	m = max(m, x);
}

template <typename T, typename U>
constexpr void chmin(T& m, U x) {
	m = min(m, x);
}

template <typename T>
constexpr T square(T x) {
	return x * x;
}

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

int to_bitset(char c) {
	return 1 << (c - 'a');
}

int main() {
	int n;
	string s;
	cin >> n >> s;
	vector<int> v(n);
	REP(i, n) { v[i] = to_bitset(s[i]); }
	SegmentTree<int> st(v, [](int a, int b) { return a | b; }, 0);
	int q;
	cin >> q;
	VI results;
	REP(i, q) {
		int type;
		cin >> type;
		if(type == 1) {
			int iq;
			char c;
			cin >> iq >> c;
			st.update(iq - 1, to_bitset(c));
		} else {
			int l, r;
			cin >> l >> r;
			results.push_back(__builtin_popcount(st.find(l - 1, r)));
		}
	}
	EACH(e, results) { cout << e << endl; }
	return 0;
}
