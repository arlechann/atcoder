#include <algorithm>
#include <iostream>
#include <limits>
#include <map>
#include <unordered_map>
#include <utility>
#include <vector>

// 約数列挙
template <typename T>
std::vector<T> divisor(T n) {
	std::vector<T> divisors;
	for(T i = 1; i * i <= n; i++) {
		if(n % i != 0) {
			continue;
		}
		divisors.push_back(i);
		if(i * i != n) {
			divisors.push_back(n / i);
		}
	}
	return divisors;
}

// エラトステネスの篩
template <typename T>
std::vector<T> sieve_of_eratosthenes(T n) {
	std::vector<T> sieve(n, 0);

	for(int i = 2; i < n; i++) {
		sieve[i] = i;
	}

	T i = 2;
	while(i * i < n) {
		if(sieve[i]) {
			for(T j = i * i; j < n; j += i) {
				sieve[j] = 0;
			}
		}
		i++;
	}

	return sieve;
}

// 素数リスト
template <typename T>
std::vector<T> prime_list(T n) {
	std::vector<T> primes = sieve_of_eratosthenes(n);
	primes.erase(std::remove(primes.begin(), primes.end(), 0), primes.end());
	return primes;
}

// 素数判定
constexpr bool is_prime(int n) {
	if(n == 2) {
		return true;
	}

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

// 素因数分解
std::unordered_map<long long, int> factor(long long n) {
	std::unordered_map<long long, int> factors;
	{
		int i = 0;
		while(n % 2 == 0) {
			n /= 2;
			i++;
		}
		if(i != 0) {
			factors[2] = static_cast<long long>(i);
		}
	}
	for(long long i = 3; i * i <= n; i += 2) {
		int j = 0;
		while(n % i == 0) {
			n /= i;
			j++;
		}
		if(j != 0) {
			factors[i] = j;
		}
	}
	if(n != 1) {
		factors[n] = 1;
	}
	return factors;
}