#include <iostream>
#include <string>
#include <sstream>
#include <stdexcept>
#include <sys/time.h>

#include "dropbox-json11/json11.hpp"
#include "16lawrencel-dynamic-graphs/fullDynamic.h"

using namespace std;
using namespace json11;

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

Json json_from_stream(istream &in) {
    stringstream buffer;
    string line;

    while (getline(in, line)) {
        buffer << line << endl;
    }

    string input = buffer.str();

    string err;
    Json json = Json::parse(input, err);
    return json;
}

vector<Instruction> instructions_from_json(Json json) {
    vector<Instruction> instructions;

    vector<Json> json_array = json.array_items();
    for (size_t i = 0; i < json_array.size(); i++) {
        map<string, Json> json_object = json_array[i].object_items();
        Instruction instr;
        instr.tag = static_cast<InstructionTag>(json_object["tag"].int_value());
        instr.x = json_object["x"].int_value();
        instr.y = json_object["y"].int_value();
        instr.expect = json_object["expect"].bool_value();
        instructions.push_back(instr);
    }

    return instructions;
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
    Json json = json_from_stream(cin);
    vector<Instruction> instructions = instructions_from_json(json);

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
