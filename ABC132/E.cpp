#include <cstdio>
#include <ctime>
#include <queue>

#define TIMEUP                   \
	if(clock() - start > 1800) { \
		printf("%d\n", -1);      \
		return 0;                \
	}

using namespace std;

int edge[100001][100001];

int main(void) {
	int n, m;
	int u, v;
	int s, t;

	clock_t start = clock();

	scanf("%d %d", &n, &m);
	for(int i = 0; i < m; i++) {
		scanf("%d %d", &u, &v);
		edge[u][v] = 1;
	}
	scanf("%d %d", &s, &t);

	queue<int> q;
	q.push(s);

	int count = 0;
	for(;;) {
		TIMEUP;
		if(q.empty()) {
			printf("%d\n", -1);
			return 0;
		}

		int node = q.front();
		q.pop();

		// printf("node: %d\n", node);

		if(node == t) {
			break;
		}

		count++;

		{
			queue<int> q2[4];
			q2[0].push(node);

			for(int i = 0; i < 3; i++) {
				while(!q2[i].empty()) {
					int now = q2[i].front();
					q2[i].pop();
					// printf("now: %d\n", now);
					for(int j = 1; j <= n; j++) {
						if(edge[now][j] == 1) {
							// printf("q2[%d+1].push(j)\n", i);
							q2[i + 1].push(j);
						}
					}
				}
			}

			while(!q2[3].empty()) {
				// printf("q.push(q2.front())\n");
				q.push(q2[3].front());
				q2[3].pop();
			}
		}
	}

	printf("%d\n", count);

	return 0;
}
