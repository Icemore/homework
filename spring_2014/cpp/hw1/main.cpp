#include <iostream>
#include <vector>
#include <string>
#include <limits>
#include <memory.h>

int dx[] = {0, 1, 1, 1, 0, -1, -1, -1};
int dy[] = {-1, -1, 0, 1, 1, 1, 0, -1};

const int rows = 6;
const int cols = 7;
const char marks[] = {'x', 'o'};
const int lookaheadDepth = 5;
const double threshold = 500;
const int me = 0;

struct field {
    char a[rows][cols];

    field() {
        memset(a, 0, sizeof(a));
    }

    char get(int i, int j) const {
        if(i < 0 || j < 0 || i >= rows || j >= cols) {
            return -1;
        }

        return a[i][j];
    }

    bool dropDisk(char mark, int col) {
        int r = 0;

        if(get(r, col) != 0) {
            return false;
        }

        while(get(r + 1, col) == 0) {
            ++r;
        }

        a[r][col] = mark;
        return true;
    }

    int getStripLen(int x, int y, int t, char mark, bool countBlank = false) const {
        int res = 0;

        while(get(x, y) == mark || get(x, y) == 0) {
            if(!countBlank && get(x, y) == 0) {
                break;
            }

            ++res;
            x += dx[t];
            y += dy[t];
        }

        return res;
    }
};


std::vector<int> countStrips(field const & f, char mark) {
    std::vector<int> res(4, 0);

    for(int x = 0; x < rows; ++x) {
        for(int y = 0; y < cols; ++y) {
            if(f.get(x, y) != mark) {
                continue;
            }

            for(int t = 0; t < 8; ++t) {
                if(f.get(x - dx[t], y - dy[t]) == mark) {
                    continue;
                }

                int forward = f.getStripLen(x, y, t, mark, false);
                int back = f.getStripLen(x, y, (t + 4) % 8, mark, true);

                //if can be length of 4
                if(back + forward - 1 >= 4) {
                    ++res[std::min(forward, 4) - 1];
                }
            }
        }
    }

    return res;
}

double estimateCurrentField(field const & f, char mark) {
    std::vector<int> c = countStrips(f, mark);

    if(c[3] > 0) {
        return std::numeric_limits<double>::infinity();
    }
    else {
        return 0.1 * c[0] + 0.25 * c[1] + 0.9 * c[2];
    }
}

double estimateStep(field const & f, int player, int depth);

std::pair<int, double> chooseStep(field const & f, int player, int depth) {
    double bestGain = 0;
    int bestCol = -1;

    for(int col = 0; col < cols; ++col) {
        field cur = f;
        
        if(!cur.dropDisk(marks[player], col)) {
            continue;
        }

        double curGain = estimateStep(cur, player, depth);
        if(bestCol == -1 || curGain > bestGain) {
            bestCol = col;
            bestGain = curGain;
        }
    }

    return std::make_pair(bestCol, bestGain);
}

double estimateStep(field const & f, int player, int depth) {
    double myProfit = estimateCurrentField(f, marks[player]);

    if(depth == 1 || myProfit > threshold) {
        return myProfit;
    }

    double enemyProfit = chooseStep(f, 1 - player, depth - 1).second;
    return (1/enemyProfit);
}

void play(int first = -1) {
    field f;

    if(first != -1) {
        f.dropDisk(marks[1 - me], first);
    }

    while(true) {
        int move = chooseStep(f, me, lookaheadDepth).first;
        f.dropDisk(marks[me], move);
        std::cout << move << std::endl;

        std::cin >> move;
        f.dropDisk(marks[1 - me], move);
    }
}

int main() {
    std::string str;
    std::cin >> str;
    
    if(str == "Go") {
        play();
    }
    else {
        play(stoi(str));
    }

    return 0;
}
