#include <iostream>
#include <queue>
#include <vector>
#include <string>
#include <sstream>
using namespace std;

struct task {
    string name;
    int start, end;
    queue< pair<int, int> > iops;
    int time_runned;

    task(string line) : time_runned(0) {
        stringstream ss(line);
        
        ss >> name;
        ss >> start >> end;

        int io_start, io_len;
        while(ss >> io_start >> io_len) {
            iops.push(make_pair(io_start, io_len));
        }
    }

    int timeToRun() const {
        if(!iops.empty()) {
            return iops.front().first - time_runned;
        }
        else {
            return end - time_runned;
        }
    }
};

struct startTimeCmp {
    bool operator() (task const & first, task const & second) {
        return first.start > second.start;
    }
};

struct timeToRunCmp{
    bool operator() (task const & first, task const & second) {
        return first.timeToRun() > second.timeToRun();
    }
};

int quantLength, currentTime;
priority_queue<task, vector<task>, startTimeCmp> waitingTasks;
priority_queue<task, vector<task>, timeToRunCmp> readyTasks;

void readInput() {
    cin >> quantLength;

   string line;

   //read the end of line
   getline(cin, line);

   while(getline(cin, line)) {
       waitingTasks.push(task(line));
   }

   currentTime = 0;
}

void processTask(task & curTask) {
    if(curTask.timeToRun() > 0) {
        readyTasks.push(curTask);
        return;
    }

    if(!curTask.iops.empty()) {
        int ioLength = curTask.iops.front().second;
        curTask.time_runned += ioLength;
        curTask.start = currentTime + ioLength;

        curTask.iops.pop();

        waitingTasks.push(curTask);
    }
}

void makeMove() {
    while(!waitingTasks.empty() && waitingTasks.top().start <= currentTime)  {
        readyTasks.push(waitingTasks.top());
        waitingTasks.pop();
    }

    if(readyTasks.empty()) {
        cout << currentTime << " IDLE" << endl;
        currentTime = waitingTasks.top().start;
        return;
    }

    task curTask = readyTasks.top();
    readyTasks.pop();

    cout << currentTime << " " << curTask.name << endl;

    int activeTime = min(quantLength, curTask.timeToRun());
    curTask.time_runned += activeTime;
    currentTime += activeTime;

    processTask(curTask); 
}

int main() {
    readInput();

    while(!waitingTasks.empty() || !readyTasks.empty()){
        makeMove();
    }

    return 0;
}
