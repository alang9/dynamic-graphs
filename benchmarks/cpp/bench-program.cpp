#include <iostream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>
#include <sys/time.h>

#include "16lawrencel-dynamic-graphs/fullDynamic.h"

using namespace std;

enum InstructionTag {
    INSERT = 0,
    LINK = 1,
    DELETE = 2,
    CUT = 3,
    CONNECTED = 4
};

typedef struct {
    InstructionTag tag;
    int x;
    int y;
    bool expect;
} Instruction;

void parse_instructions(istream &in, vector<Instruction> &instructions) {
    string word;

    while (in >> word) {
        Instruction instr;
        if (word == "insert") {
            instr.tag = INSERT;
            in >> instr.x;
        } else if (word == "link") {
            instr.tag = LINK;
            in >> instr.x;
            in >> instr.y;
        } else if (word == "delete") {
            instr.tag = DELETE;
            in >> instr.x;
        } else if (word == "cut") {
            instr.tag = CUT;
            in >> instr.x;
            in >> instr.y;
        } else if (word == "connected") {
            instr.tag = CONNECTED;
            string b;
            in >> instr.x;
            in >> instr.y;
            in >> b;
            instr.expect = b == "true" ? true : false;
        } else {
            cerr << "Unknown instruction: " << word;
            throw runtime_error("bail");
        }

        instructions.push_back(instr);
    }
}

void run(const vector<Instruction> &instructions) {
    FullDynamic graph = FullDynamic();
    map<int, set<int>*> edges;

    for (size_t i = 0; i < instructions.size(); i ++) {
        Instruction instr = instructions[i];
        if (instr.tag == INSERT) {
            graph.add(instr.x);
            edges[instr.x] = new set<int>();

        } else if(instr.tag == LINK) {
            graph.link(instr.x, instr.y);
            edges[instr.x]->insert(instr.y);
            edges[instr.y]->insert(instr.x);

        } else if(instr.tag == CUT) {
            graph.cut(instr.x, instr.y);
            edges[instr.x]->erase(instr.y);
            edges[instr.y]->erase(instr.x);

        } else if(instr.tag == DELETE) {
            set<int> *neighbours = edges[instr.x];
            set<int>::iterator it;
            for (it = neighbours->begin(); it != neighbours->end(); ++it) {
                int y = *it;
                graph.cut(y, instr.x);
                edges[y]->erase(instr.x);
            }

            edges[instr.x]->clear();

        } else if(instr.tag == CONNECTED) {
            bool c = graph.conn(instr.x, instr.y);
            if (c != instr.expect) {
                cerr <<
                    "Unexpected results at instr " << i << ": " <<
                    "Expected " << instr.x << "-" << instr.y <<
                    " conn=" << instr.expect << " but got conn=" << c << endl;
                throw runtime_error("bail");
            }

        } else {
            throw runtime_error("Unknown tag");
        }
    }
}


/* Benchmarking utils */
typedef unsigned long long timestamp_t;
static timestamp_t get_timestamp() {
    struct timeval now;
    gettimeofday (&now, NULL);
    return  now.tv_usec + (timestamp_t)now.tv_sec * 1000000;
}

int main(int argc, char **argv) {
    vector<Instruction> instructions;
    parse_instructions(cin, instructions);
    cerr << "Running " << instructions.size() << " instructions..." << endl;

    /* Warmup */
    run(instructions);

    double secs_total = 0;
    int samples = 5;
    for (int i = 0; i < samples; i++) {
        timestamp_t t0 = get_timestamp();
        run(instructions);
        timestamp_t t1 = get_timestamp();
        double secs = (t1 - t0) / 1000000.0L;
        secs_total += secs;
    }

    cout << (secs_total / (double) samples) << " s" << endl;
    return 0;
}
