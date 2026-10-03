#include "simdjson.h"
#include <chrono>
#include <cstdio>
#include <string>

using namespace simdjson;

static size_t sink = 0;

template <class F> static double best_seconds(int iters, F f) {
  double best = 1e30;
  for (int i = 0; i < iters; i++) {
    auto t0 = std::chrono::steady_clock::now();
    f();
    double s = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
    if (s < best) best = s;
  }
  return best;
}

static void report(const std::string &name, size_t bytes, double seconds) {
  std::printf("cpp   %s: %.3f GB/s\n", name.c_str(), bytes / seconds / 1e9);
}

int main(int argc, char **argv) {
  if (argc < 2) { std::fprintf(stderr, "usage: bench <corpus dir>\n"); return 1; }
  std::string dir = argv[1];
  for (const char *name : {"twitter", "canada", "citm_catalog"}) {
    padded_string json = padded_string::load(dir + "/" + name + ".json");
    dom::parser parser;
    report(std::string(name) + " parse", json.size(), best_seconds(20, [&] {
      dom::element doc = parser.parse(json);
      sink += doc.is_object();
    }));
    if (std::string(name) == "twitter") {
      ondemand::parser op;
      report("twitter typed (ondemand)", json.size(), best_seconds(20, [&] {
        auto doc = op.iterate(json);
        uint64_t total = 0;
        for (auto s : doc["statuses"]) {
          total += s["id"].get_uint64().value();
          total += std::string_view(s["text"]).size();
          auto user = s["user"];
          total += user["id"].get_uint64().value();
          total += std::string_view(user["screen_name"]).size();
          total += user["followers_count"].get_uint64().value();
          total += user["friends_count"].get_uint64().value();
          total += user["verified"].get_bool().value();
          total += s["retweet_count"].get_uint64().value();
          total += s["favorite_count"].get_uint64().value();
          total += std::string_view(s["lang"]).size();
        }
        sink += total;
      }));
    }
    if (std::string(name) == "canada") {
      ondemand::parser op;
      report("canada typed (ondemand)", json.size(), best_seconds(20, [&] {
        auto doc = op.iterate(json);
        double total = 0;
        for (auto f : doc["features"]) {
          for (auto ring : f["geometry"]["coordinates"]) {
            for (auto point : ring.get_array()) {
              for (double x : point.get_array()) total += x;
            }
          }
        }
        sink += (size_t)total;
      }));
    }
    dom::element doc = parser.parse(json);
    std::string out = simdjson::to_string(doc);
    report(std::string(name) + " write", out.size(), best_seconds(20, [&] {
      sink += simdjson::to_string(doc).size();
    }));
  }
  return sink == 1 ? 1 : 0;
}
