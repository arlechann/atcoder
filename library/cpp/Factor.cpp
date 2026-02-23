#include <numeric>
#include <unordered_map>
#include <vector>

// 高速素因数分解
class Factor {
	std::vector<int> divisors;

	public:
	Factor(int n) : divisors(n, 0) {
		std::iota(this->divisors.begin(), this->divisors.end(), 0);
		for(int i = 2; i * i < n; i++) {
			if(this->divisors[i] != i) {
				continue;
			}
			this->divisors[i] = i;
			for(int j = i * i; j < n; j += i) {
				if(this->divisors[j] == j) {
					this->divisors[j] = i;
				}
			}
		}
	}

	std::unordered_map<int, int> operator()(int n) {
		std::unordered_map<int, int> factors;
		while(n != 1) {
			int divisor = this->divisors[n];
			while(n % divisor == 0) {
				factors[divisor]++;
				n /= divisor;
			}
		}
		return factors;
	}
};