#include<cstdio>

int a, b;

int solve(int n);
int sum(int n);

int main(void){
	int n;

	scanf("%d %d %d", &n, &a, &b);

	printf("%d\n", solve(n));

	return 0;
}

int solve(int n){
	if(!n){
		return 0;
	}

	int x = sum(n);

	if(x >= a && x <= b){
		return n + solve(n - 1);
	}

	return solve(n - 1);
}

int sum(int n){
	return n / 10000 + n % 10000 / 1000 + n % 1000 / 100 + n % 100 / 10 + n % 10;
}