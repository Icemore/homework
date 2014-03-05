// run 5540307

#include <iostream>
using namespace std;

struct node {
    node(int val, node* left = 0, node* right = 0)
        : val(val), left(left), right(right)
    {
        fix();
    }

    void fix() {
        sum = 0;
        cnt = 0;

        if(left) {
            sum += left->sum;
            cnt += left->cnt;
        }
        if(right) {
            sum += right->sum;
            cnt += right->cnt;
        }
        
        ++cnt;
        sum += val;
    }

    int val;
    long long sum;
    int cnt;
    node* left;
    node* right;
};

int getCnt(node* x) {
    return x ? x->cnt : 0;
}

long long getSum(node* x) {
    return x ? x->sum : 0;
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

node* splay(node* x, int val, int cur, int &mark) {
    if(!x || cur + getCnt(x->left) == val) {
        mark = 0;
        return x;
    }

    if(val < cur + getCnt(x->left)) {
        x->left = splay(x->left, val, cur, mark);
        x = rotateR(x);
        ++mark;

        if(mark > 1) {
            x->right = rotateR(x->right);
            mark = 0;
        }
    }
    else {
        x->right = splay(x->right, val, cur + 1 + getCnt(x->left), mark);
        x = rotateL(x);
        --mark;

        if(mark < -1) {
            x->left = rotateL(x->left);
            mark = 0;
        }
    }

    return x;
}

void splay(node*& x, int pos) {
    int mark = 0;
    x = splay(x, pos, 0, mark);
}

node* merge(node*& l, node*& r) {
    if(!l) return r;

    splay(l, l->cnt - 1);

    l->right = r;
    l->fix();

    return l;
}

void add(node*& x, int val) {
    node* newNode = new node(val);

    if(!x) {
        x = newNode;
    }
    else {
        merge(x, newNode);
    }
}


int main() {
    ios_base::sync_with_stdio(false); cin.tie(0);
    
    int n, k;
    node* root = 0;

    cin >> n >> k;

    for(int i = 0; i < n; ++i) {
        add(root, i);
    }

    int last = 0;
    int left = n;
    --k;

    for(int i = 0; i < n; ++i) {
        int cur = (last + k) % root->cnt;

        splay(root, cur);

        cout << root->val + 1 << " ";

        root = merge(root->left, root->right);
        last = cur;
    }

    return 0;
}