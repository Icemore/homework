#include<iostream>
#include<ctime>
using namespace std;

int** CreateN_1(size_t n, size_t m)
{
    int** arr = new int* [n];
    for(size_t i = 0; i != n; ++i)
        arr[i] = new int [m];

    arr[0][0]=0;
    return arr;
}

void FreeN_1(int** arr, size_t n, size_t m)
{
    for(size_t i = 0; i != n; ++i)
        delete[] arr[i];
    delete[] arr;
}

int* Create1(size_t n, size_t m)
{
    int* arr = new int [n * m];
    arr[0]=0;
    return arr;
}

void Free1(int* arr, size_t n, size_t m)
{
    delete[] arr;
}

int& get(int* arr, int i, int j, size_t n, size_t m)
{
    return *(arr + m * i + j);
}

int** Create2(size_t n, size_t m)
{
    int** arr = new int* [n];
    arr[0] = new int [n * m];
    for(size_t i = 1; i != n; ++i)
        arr[i] = arr[i-1] + m;

    arr[0][0]=0;
    return arr;
}

void Free2(int** arr, size_t n, size_t m)
{
    delete[] arr[0];
    delete[] arr;
}

int main()
{
    size_t n = 90000;
    size_t m = 10000;
    
    {
        clock_t start = clock();
            int** arr = CreateN_1(n, m);
        clock_t end = clock();
        cout << "Create N-1: " << (double)(end - start) / CLOCKS_PER_SEC << endl;
        FreeN_1(arr, n, m);
    }

    {
        clock_t start = clock();
            int** arr = Create2(n, m);
        clock_t end = clock();
        cout << "Create 2: " << (double)(end - start) / CLOCKS_PER_SEC << endl;
        Free2(arr, n, m);
    }

    {
        clock_t start = clock();
            int* arr = Create1(n, m);
        clock_t end = clock();
        cout << "Create 1: " << (double)(end - start) / CLOCKS_PER_SEC << endl;
        Free1(arr, n, m);
    }

    return 0;
}
