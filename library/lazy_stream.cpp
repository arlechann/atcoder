#include <functional>
#include <iostream>
#include <tuple>
#include <vector>

template <typename T>
struct lazy_conscell {
	T car;
	std::function<lazy_conscell<T>*()>** cdr;
};

template <typename T>
using lazy_stream = std::function<lazy_conscell<T>*()>;

template <typename T>
lazy_stream<T>* lazy_cons(T car, lazy_stream<T>** cdr) {
	lazy_conscell<T>** cons = new lazy_conscell<T>*;
	*cons = new lazy_conscell<T>;
	(*cons)->car = car;
	(*cons)->cdr = cdr;
	return new lazy_stream<T>([=] { return *cons; });
}

template <typename T>
T lazy_car(lazy_stream<T>* stream) {
	return (*stream)()->car;
}

template <typename T>
lazy_stream<T>* lazy_cdr(lazy_stream<T>* stream) {
	return *(*stream)()->cdr;
}

template <typename T>
T lazy_nth(size_t n, lazy_stream<T>* stream) {
	if(n == 0) {
		return lazy_car(stream);
	}

	return lazy_nth(n - 1, lazy_cdr(stream));
}

template <typename T>
std::vector<T> lazy_take_rec(std::vector<T> acc, size_t n,
							 lazy_stream<T>* stream) {
	if(n == 0) {
		return acc;
	}

	acc.push_back(lazy_car(stream));
	return lazy_take_rec(acc, n - 1, lazy_cdr(stream));
}

template <typename T>
std::vector<T> lazy_take(size_t n, lazy_stream<T>* stream) {
	return lazy_take_rec(std::vector<T>(), n + 1, stream);
}

template <typename T, typename S>
lazy_stream<T>* lazy_map(auto f, lazy_stream<S>** stream) {
	return new lazy_stream<T>([=]() {
		lazy_stream<T>** cdr = new lazy_stream<T>*;
		*cdr = lazy_map<T, S>(f, (**stream)()->cdr);
		return (*lazy_cons<T>(f(lazy_car(*stream)), cdr))();
	});
}

template <typename T, typename S, typename R>
lazy_stream<T>* lazy_map(auto f, lazy_stream<S>** stream1,
						 lazy_stream<R>** stream2) {
	return new lazy_stream<T>([=]() {
		lazy_stream<T>** cdr = new lazy_stream<T>*;
		*cdr = lazy_map<T, S, R>(f, (**stream1)()->cdr, (**stream2)()->cdr);
		return (
			*lazy_cons<T>(f(lazy_car(*stream1), lazy_car(*stream2)), cdr))();
	});
}

template <typename T>
lazy_stream<T>* lazy_filter(auto cond, lazy_stream<T>** stream) {
	return new lazy_stream<T>([=]() {
		lazy_stream<T>** cdr = new lazy_stream<T>*;
		lazy_conscell<T>* cons = (**stream)();
		T car = cons->car;
		std::cerr << "lazy_filter" << car << std::endl;
		if(cond(car)) {
			*cdr = lazy_filter<T>(cond, cons->cdr);
			return (*lazy_cons<T>(car, cdr))();
		} else {
			*cdr = lazy_filter<T>(cond, cons->cdr);
			return (**cdr)();
		}
	});
}

template <typename T>
lazy_stream<T>* lazy_const_stream(T n) {
	lazy_stream<T>** cdr = new lazy_stream<T>*;
	*cdr = lazy_cons(n, cdr);
	return *cdr;
}

template <typename T>
lazy_stream<T>* lazy_iota(T a, T d) {
	lazy_stream<T>** const_stream = new lazy_stream<T>*;
	*const_stream = lazy_const_stream(d);
	lazy_stream<T>** cons = new lazy_stream<T>*;
	lazy_stream<T>** cdr = new lazy_stream<T>*;
	*cdr = lazy_map<T>([=](T x, T y) { return x + y + a; }, const_stream, cons);
	*cons = lazy_cons(d, cdr);
	return lazy_cons(a, cons);
}

template <typename T>
lazy_stream<T>* lazy_rec_stream(T a, auto f) {
	lazy_stream<T>** cons = new lazy_stream<T>*;
	lazy_stream<T>** cdr = new lazy_stream<T>*;
	*cdr = new lazy_stream<T>([=]() { return (*lazy_rec_stream(f(a), f))(); });
	*cons = lazy_cons(a, cdr);
	return *cons;
}

lazy_stream<long long>* lazy_fib() {
	lazy_stream<long long>** first = new lazy_stream<long long>*;
	lazy_stream<long long>** second = new lazy_stream<long long>*;
	lazy_stream<long long>** third = new lazy_stream<long long>*;
	*third = lazy_map<long long, long long>(
		[](long long a, long long b) { return a + b; }, first, second);
	*second = lazy_cons(1LL, third);
	*first = lazy_cons(1LL, second);
	return *first;
}

template <typename T>
lazy_stream<T>* lazy_even() {
	lazy_stream<T>** seq = new lazy_stream<T>*;
	*seq = lazy_iota(0, 1);
	return lazy_filter([](T a) { return a % 2 == 0; }, seq);
}

template <typename T>
lazy_stream<T>* lazy_odd() {
	lazy_stream<T>** seq = new lazy_stream<T>*;
	*seq = lazy_iota(0, 1);
	return lazy_filter([](T a) { return a % 2 == 1; }, seq);
}

int main() {
	int n;
	std::cin >> n;

	std::cout << "lazy_rec_stream:" << std::endl;
	for(auto&& e :
		lazy_take(n, lazy_rec_stream(1, [](int a) { return a * 2; }))) {
		std::cout << e << std::endl;
	}

	std::cout << "lazy_iota:" << std::endl;
	for(auto&& e : lazy_take(n, lazy_iota(0, 1))) {
		std::cout << e << std::endl;
	}

	std::cout << "lazy_fib:" << std::endl;
	for(auto&& e : lazy_take(n, lazy_fib())) {
		std::cout << e << std::endl;
	}

	std::cout << "lazy_even:" << std::endl;
	for(auto&& e : lazy_take(n, lazy_even<int>())) {
		std::cout << e << std::endl;
	}

	std::cout << "lazy_odd:" << std::endl;
	for(auto&& e : lazy_take(n, lazy_odd<int>())) {
		std::cout << e << std::endl;
	}

	return 0;
}