#include <cstdio>

int solve(int a, int b, int c, int x);

int main(void){
	int a, b, c, x;

	scanf("%d\n", &a);
	scanf("%d\n", &b);
	scanf("%d\n", &c);
	scanf("%d\n", &x);

	printf("%d\n", solve(a, b, c, x));
	return 0;
}

int solve(int a, int b, int c, int x){
	int result = 0;

	if (x < 0){
		return 0;
	}

	if (!x){
		return 1;
	}

	if (a){
		for (int i = 0; i <= a; i++){
			result += solve(0, b, c, x - 500 * i);
		}
		return result;
	}

	if (b){
		for (int i = 0; i <= b; i++){
			result += solve(0, 0, c, x - 100 * i);
		}
		return result;
	}

	if (c){
		for (int i = 0; i <= c; i++){
			result += solve(0, 0, 0, x - 50 * i);
		}
		return result;
	}

	return 0;
}