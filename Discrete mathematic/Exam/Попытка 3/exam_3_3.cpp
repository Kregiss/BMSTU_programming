// не моё

#include <iostream>
#include <vector>

using namespace std;

void mn(int q01, int q02, vector<vector<int>> &M, vector<vector<int>> &L,
        vector<vector<int>> &R, int &minstr, int now, vector<vector<bool>> &pred, int Mx){
    if (pred[q01][q02])
        return;
    if (now > minstr && minstr != -1)
        return;
    if (R[q02][0] == 1){
        minstr = min(minstr, now);
        if (minstr == -1)
            minstr = now;
        return;
    }
    pred[q01][q02] = true;
    for (int i = 0; i < Mx; i++){
        mn(M[q01][i], R[q02][L[q01][i]], M, L, R, minstr, now + 1, pred, Mx);
    }
}

int main(){
    int N1, Mx, q01, N2, My, q02;
    cin>>N1>> Mx>> q01;
    vector<vector<int>> M(N1, vector<int>(Mx));
    for (int i = 0; i < N1; i++){
        for (int j = 0; j < Mx; j++){
            std::cin>>M[i][j];
        }
    }
    vector<vector<int>> L(N1, vector<int>(Mx));
    for (int i = 0; i < N1; i++){
        for (int j = 0; j < Mx; j++){
            char c;
            cin>>c;
            L[i][j] = c - 'a' + 1;
        }
    }
    cin>>N2>>My>>q02;
    vector<vector<int>> R(N2, vector<int>(My + 1));
    for (int i = 0; i < N2; i++){
        char c;
        cin>>c;
        R[i][0] = (c=='+');
        for (int j = 0; j < My; j++){
            cin>>R[i][j + 1];
        }
    }
    int minStr = -1;
    vector<vector<bool>> pred(N1, vector<bool>(N2, false));
    if (Mx <= My)
        mn(q01, q02, M, L, R, minStr, 0, pred, Mx);
    if (minStr == -1)
        cout<<"none";
    else
        cout<<minStr;
}
