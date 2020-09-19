#include <cmath>
#include <vector>

template <typename T>
class Doubling {
	std::size_t n;
	std::size_t k;
	std::vector<std::vector<T>> doubling;

	public:
	template <typename F>
	Doubling(long long m, std::size_t n, F next)
		: n(n), k(([=]() {
			  std::size_t k = 1;
			  while((1LL << k) < m) {
				  k++;
			  }
			  return k;
		  })()),
		  doubling(k + 1, std::vector<T>(n)) {
		for(int i = 0; i < this->n; i++) {
			this->doubling[0][i] = next(i);
		}
		for(int i = 0; i < this->k; i++) {
			for(int j = 0; j < this->n; j++) {
				this->doubling[i + 1][j] =
					this->doubling[i][this->doubling[i][j]];
			}
		}
	}

	T operator()(std::size_t x, std::size_t p = 0) {
		for(std::size_t i = 0; i < this->k; i++) {
			if(x & (1LL << i)) {
				p = this->doubling[i][p];
			}
		}
		return p;
	}
};