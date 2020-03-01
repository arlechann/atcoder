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

// UnionFind(disjoint set)
class UnionFind {
	std::vector<size_t> parents;
	std::vector<size_t> rank;
	std::vector<size_t> tree_size;

	public:
	UnionFind(size_t size) : parents(size), rank(size, 0), tree_size(size, 1) {
		std::iota(this->parents.begin(), this->parents.end(), 0);
	}

	// 併合
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
		this->tree_size[ar] = this->tree_size[br] =
			this->tree_size[ar] + this->tree_size[br];
		return this->parents[br] = ar;
	}
	bool unite(size_t a, size_t b) { return this->merge(a, b); }

	// 同集合か判定
	bool is_same(size_t a, size_t b) { return this->root(a) == this->root(b); }
	bool is_union(size_t a, size_t b) { return this->is_same(a, b); }
	size_t size(size_t n) {
		this->root(n);
		return this->tree_size[n];
	}

	private:
	size_t root(int n) {
		if(this->parents[n] == n) {
			return n;
		}

		size_t r = this->root(this->parents[n]);
		this->parents[n] = r;
		this->tree_size[n] = this->tree_size[r];
		return r;
	}
};

int main() {
	int n, m, k;
	cin >> n >> m >> k;
	VI a(m);
	VI b(m);
	VI2D friends(n);
	REP(i, m) {
		cin >> a[i] >> b[i];
		friends[a[i] - 1].push_back(b[i] - 1);
		friends[b[i] - 1].push_back(a[i] - 1);
	}
	VI c(k);
	VI d(k);
	VI2D blocks(n);
	REP(i, k) {
		cin >> c[i] >> d[i];
		blocks[c[i] - 1].push_back(d[i] - 1);
		blocks[d[i] - 1].push_back(c[i] - 1);
	}
	UnionFind uf(n);
	REP(i, m) { uf.merge(a[i] - 1, b[i] - 1); }
	REP(i, n) {
		int size = uf.size(i) - 1;
		EACH(e, friends[i]) {
			if(uf.is_same(i, e)) {
				size--;
			}
		}
		EACH(e, blocks[i]) {
			if(uf.is_same(i, e)) {
				size--;
			}
		}
		cout << size << ' ';
	}
	cout << endl;
	return 0;
}
