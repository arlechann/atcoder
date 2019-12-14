#include<cstdio>

int n;
int a[100];

int solve(void);

int main(void){
	scanf("%d\n", &n);
	for(int i = 0; i < n; i++){
		scanf("%d", a + i);
	}

	printf("%d\n", solve());

	return 0;
}

int solve(void){
	bool used[100];
	int alice = 0;
	int bob = 0;
	int index_of_max;

	for(int i = 0; i < n; i++){
		used[i] = false;
	}

	for(int i = 0; i < n; i++){
		index_of_max = -1;
		for(int card = 0; card < n; card++){
			if(used[card]){
				continue;
			}

			if(index_of_max == -1){
				index_of_max = card;
			}

			if(a[index_of_max] < a[card]){
				index_of_max = card;
			}
		}

		if(i % 2){
			bob += a[index_of_max];
		}else{
			alice += a[index_of_max];
		}
		used[index_of_max] = true;
	}

	return alice - bob;
}