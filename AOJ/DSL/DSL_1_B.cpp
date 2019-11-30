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

int main() {
	int n, q;
	scanf("%d %d", &n, &q);
	VI s(n);
	VI com(q);
	VI x(q);
	VI y(q);
	VI z(q);
	REP(i, q) {
		scanf("%d", &com[i]);
		scanf("%d %d", &x[i], &y[i]);
		if(!com[i]) {
			scanf("%d", &z[i]);
		}
	}

	WeightedUnionFind<int> wuf(n);
	REP(i, q) {
		if(com[i]) {
			if(const boost::optional<int> d = wuf.diff(x[i], y[i])) {
				printf("%d\n", *d);
			} else {
				puts("?");
			}
		} else {
			wuf.merge(x[i], y[i], z[i]);
		}
	}
	return 0;
}