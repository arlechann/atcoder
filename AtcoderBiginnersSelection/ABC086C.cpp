#include<cstdio>
#include<cstdlib>

typedef struct{
    int t;
    int x;
    int y;
}Plan;

int n;
Plan plan[100000];

bool solve(void);
int manhattan_distance(int x1, int y1, int x2, int y2);

int main(void){
    scanf("%d", &n);
    for(int i = 0; i < n; i++){
        scanf("%d %d %d", &plan[i].t, &plan[i].x, &plan[i].y);
    }

    printf("%s\n", solve() ? "Yes" : "No");

    return 0;
}

bool solve(void){
    int t = 0;
    int x = 0;
    int y = 0;
    
    for(int next = 0; next < n; next++){
        if(plan[next].t - t < manhattan_distance(plan[next].x, plan[next].y,x, y)){
            return false;
        }

        if((plan[next].t - t) % 2 != manhattan_distance(x, y, plan[next].x, plan[next].y) % 2){
            return false;
        }

        t = plan[next].t; x = plan[next].x; y = plan[next].y;
    }

    return true;
}

int manhattan_distance(int x1, int y1, int x2, int y2){
    return abs(x1 - x2) + abs(y1 - y2);
}