#include <cstdint>
#include <cstdio>
#include <meta>
#include <string>
#include <string_view>
#include <type_traits>

void write_json(std::string& out, bool v) { out += v ? "true" : "false"; }

void write_json(std::string& out, std::string_view v) {
    out += '"';
    out += v;
    out += '"';
}

template <class T>
    requires std::is_integral_v<T>
void write_json(std::string& out, T v) {
    if constexpr (std::is_signed_v<T>) out += std::to_string(static_cast<long long>(v));
    else out += std::to_string(static_cast<unsigned long long>(v));
}

constexpr uint32_t mix(uint32_t h, std::string_view s) {
    for (char c : s) h = (h ^ static_cast<unsigned char>(c)) * 16777619u;
    return h;
}

template <class T>
consteval uint32_t fingerprint() {
    uint32_t h = 2166136261u;
    for (auto m : std::meta::nonstatic_data_members_of(^^T, std::meta::access_context::unchecked())) {
        auto t = std::meta::dealias(std::meta::type_of(m));
        h = mix(mix(mix(mix(h, std::meta::identifier_of(m)), ":"), t == ^^bool ? "bool" : std::meta::is_integral_type(t) ? "int" : "str"), ",");
    }
    return h;
}

template <class T>
    requires std::is_class_v<T>
void write_json(std::string& out, const T& v) {
    out += '{';
    static constexpr uint32_t schema = fingerprint<T>();
    out += "\"schema\":";
    out += std::to_string(schema);
    out += ',';
    bool first = true;
    template for (constexpr auto m : std::define_static_array(
                      std::meta::nonstatic_data_members_of(^^T, std::meta::access_context::unchecked()))) {
        if (!first) out += ',';
        first = false;
        out += '"';
        out += std::meta::identifier_of(m);
        out += "\":";
        write_json(out, v.[:m:]);
    }
    out += '}';
}

template <class T>
size_t json_len(const T& v) {
    std::string out;
    write_json(out, v);
    return out.size();
}

size_t serialize_all();

int main() { printf("%zu\n", serialize_all()); }
