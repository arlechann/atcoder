#include <algorithm>
#include <iostream>
#include <limits>
#include <map>
#include <utility>
#include <vector>

// コピペここから

template <typename T>
std::vector<T> prime_table(T n) {
	std::vector<T> prime(n, 0);

	for(int i = 2; i < n; i++) {
		prime[i] = i;
	}

	T i = 2;
	while(i * i < n) {
		if(prime[i]) {
			for(T j = i * i; j < n; j += i) {
				prime[j] = 0;
			}
		}
		i++;
	}

	return prime;
}

template <typename T>
std::vector<T> prime_list(T n) {
	std::vector<T> prime = prime_table(n);
	prime.erase(std::remove(prime.begin(), prime.end(), 0), prime.end());
	return prime;
}

constexpr bool is_prime(int n) {
	if(n % 2 == 0) {
		return false;
	}

	for(int i = 3; i * i <= n; i += 2) {
		if(n % i == 0) {
			return false;
		}
	}
	return true;
}

std::map<long long, int> factor(long long n) {
	std::map<long long, int> factor;

	{
		int i = 0;
		while(n % 2 == 0) {
			n /= 2;
			i++;
		}
		if(i != 0) {
			factor[2] = static_cast<long long>(i);
		}
	}
	for(long long i = 3; i <= n; i += 2) {
		int j = 0;
		while(n % i == 0) {
			n /= i;
			j++;
		}
		if(j != 0) {
			factor[i] = j;
		}
	}

	return factor;
}

std::map<long long, int> factor(long long n, std::vector<long long> prime) {
	std::map<long long, int> factor;

	for(int i = 0; static_cast<long long>(prime[i]) <= n; i++) {
		int j = 0;
		while(n % prime[i] == 0) {
			n /= prime[i];
			j++;
		}
		if(j != 0) {
			factor[prime[i]] = j;
		}
	}

	return factor;
}

// コピペここまで

int main() {
	for(auto&& e : prime_list(100)) {
		std::cout << e << std::endl;
	}
	for(auto&& e : factor(1000000000LL - 1LL)) {
		std::cout << e.first << "^" << e.second << std::endl;
	}
	for(auto&& e : factor(1000000007LL)) {
		std::cout << e.first << "^" << e.second << std::endl;
	}
	std::cout << std::boolalpha << is_prime(1000000007LL) << std::endl;

	return 0;
}