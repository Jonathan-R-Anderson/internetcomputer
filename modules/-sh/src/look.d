module look;

import mstd.stdio;
import mstd.string : toLower, strip;
import mstd.file : readText;
import mstd.stdio : File;
import mstd.algorithm : filter;
import mstd.ascii : isAlphaNum;

void lookCommand(string[] tokens)
{
    bool dict = false;
    bool ignoreCase = false;
    dchar term;
    bool useTerm = false;

    size_t idx = 1;
    while(idx < tokens.length && tokens[idx].startsWith("-")) {
        auto t = tokens[idx];
        if(t == "-d") dict = true;
        else if(t == "-f") ignoreCase = true;
        else if(t.startsWith("-t")) {
            if(t.length > 2)
                term = t[2];
            else if(idx + 1 < tokens.length)
                term = tokens[++idx][0];
            useTerm = true;
        } else if(t == "--") { idx++; break; }
        idx++;
    }

    if(idx >= tokens.length) { writeln("look: missing string"); return; }
    string prefix = tokens[idx++];
    string file = idx < tokens.length ? tokens[idx] : "/usr/share/dict/words";

    File f;
    try { f = File(file, "r"); } catch(Exception) { writeln("look: cannot read ", file); return; }

    foreach(line; f.byLine()) {
        auto l = line.chomp;
        string cmpLine = l;
        string cmpPref = prefix;
        if(useTerm) {
            auto p = cmpLine.indexOf(term);
            if(p >= 0) cmpLine = cmpLine[0 .. p+1];
            p = cmpPref.indexOf(term);
            if(p >= 0) cmpPref = cmpPref[0 .. p+1];
        }
        if(dict) {
            cmpLine = cmpLine.filter!(c => isAlphaNum(c)).idup;
            cmpPref = cmpPref.filter!(c => isAlphaNum(c)).idup;
        }
        if(ignoreCase) {
            cmpLine = toLower(cmpLine);
            cmpPref = toLower(cmpPref);
        }
        if(cmpLine.startsWith(cmpPref))
            writeln(l);
    }
}

