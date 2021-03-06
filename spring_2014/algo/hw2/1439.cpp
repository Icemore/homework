// run 5525017

#include <iostream>
#include <cmath>
using namespace std;


struct node {
    node(int val, node * left = 0, node * right = 0) 
        : val(val), left(left), right(right)
    {
        fix();
    }

    void fix() {
        h = 0;
        cnt = 0;

        if(left) {
            h = max(h, left->h);
            cnt += left->cnt;
        }
        if(right) {
            h = max(h, right->h);
            cnt += right->cnt;
        }

        ++h;
        ++cnt;
    }

    int val;
    int h;
    int cnt;
    node* left;
    node* right;
};

int n, m;
node* root;

int getH(node* x) {
    return x ? x->h : 0;
}

int getCnt(node* x) {
    return x ? x->cnt : 0;
}

node* rotateR(node* x) {
    node* y = x->left;

    x->left = y->right;
    y->right = x;
    
    x->fix();
    y->fix();
    
    return y;
}

node* rotateL(node* y) {
    node* x = y->right;

    y->right = x->left;
    x->left = y;

    y->fix();
    x->fix();
    
    return x;
}

node* rotateBigR(node* x) {
    x->left = rotateL(x->left);
    return rotateR(x);
}

node* rotateBigL(node* x) {
    x->right = rotateR(x->right);
    return rotateL(x);
}

node* balanceNode(node* x) {
    int hl = getH(x->left);
    int hr = getH(x->right);

    if(abs(hl-hr) <= 1) {
        return x;
    }

    if(hl > hr) {
        if(getH(x->left->right) > getH(x->left->left)) {
            return rotateBigR(x);
        }
        else {
            return rotateR(x);
        }
    }
    else {
        if(getH(x->right->left) > getH(x->right->right)) {
            return rotateBigL(x);
        }
        else {
            return rotateL(x);
        }
    }
}

node* add(node* cur, node* x) {
    if(!cur) {
        return x;
    }

    if(x->val < cur->val) {
        cur->left = add(cur->left, x);
    }
    else {
        cur->right = add(cur->right, x);
    }

    cur->fix();
    return balanceNode(cur);
}

void add(int val) {
    node* newNode = new node(val);
    root = add(root, newNode);
}

int getRealNumber(int x, node* cur = root, int shift = 0) {
    if(!cur) {
        return x + shift;
    }

    if(x < cur->val - shift - getCnt(cur->left)) {
        return getRealNumber(x, cur->left, shift);
    }
    else {
        return getRealNumber(x, cur->right, shift + getCnt(cur->left) + 1);
    }
}

int main() {
    int n, m;

    cin >> n >> m;
    for(int i = 0; i < m; ++i) {
        char c;
        int x;

        cin >> c >> x;

        if(c == 'D') {
            add(getRealNumber(x));
        }
        else {
            cout << getRealNumber(x) << "\n";
        }

    }

    return 0;
}