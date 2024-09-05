/*
judge usage: j path/to/solution.cpp
j会读取同名文件后缀为".case"的测试样例文件进行判题，如path/to/solution.case。

## *.case 测试样例语法规则：
例子：该测试样例文件包含两个测试点，第一个未启用，第二个启用

``` jcase
[user_defined_test_name1]{enable=false}: basic
40 12
a * 0 2
a * 1 2
---
2

[user_defined_test_name2]{enable=true}: description of basic2
40 11
a * 0 2
a * 1 2
---
1
```

## 每个测试点规则如下：
+----------------------------------+
| `[<name>]{<config>, ...}: <hint>`|
+----------------------------------+
| <input>                          |
+----------------------------------+
| ---                              |
+----------------------------------+
| <output>                         |
+----------------------------------+
第一行是测试点的title，包含测试点名称name、设置config和描述信息hint;
第二行是输入文本input；
第三行是分隔符，以"---"开头;
第四行是输出文本output。

输入/输出文本前后的空白字符会自动去除，然后通过管道输入给答题程序，
结果通过管道读取并去除前后空白字符，
如果输入输出的比较结果相同则表示测试通过，对于输出的中间结果，需要注意末尾是否有空格，可能会误判。

## 测试结果包含：AC, WA, RE(输出正确但是返回值错误), TLE, MLE。

## 以 `#` 开头的行表示注释，会自动跳过注释。
注释不能出现在<input>和<output>段，否则会将其作为输入输出进行读取和结果比较。

*/

#include <cassert>
#include <cstring>
#include <filesystem>
#include <future>
#include <iostream>
#include <fstream>
#include <numeric>
#include <cmath>
#include <string>
#include <algorithm>
#include <format>
#include <sys/resource.h>
#include <sys/wait.h>
#include <unistd.h>
#include <utility>
#include <vector>
#include <regex>
#include <cstdlib>
#include <ranges>
#include <iterator>
#include <thread>

namespace strutil {

    auto TrimLeft(std::string_view str) -> std::string_view
    {
        const auto* start = std::find_if_not(str.begin(), str.end(), [](char ch) static noexcept { return std::isspace(ch); });
        return { start, str.end() };
    }

    auto TrimRight(std::string_view str) -> std::string_view
    {
        auto last = str.find_last_not_of(" \n\t\r");
        return { str.begin(), str.begin() + (last == std::string::npos ? last : last + 1) };
    }

    auto Trim(std::string_view str) -> std::string_view
    {
        const auto* start = std::find_if_not(str.begin(), str.end(), [](char ch) static noexcept { return std::isspace(ch); });
        const auto* end = str.end();
        while (end != start) {
            if (not std::isspace(*(end - 1))) {
                break;
            }
            --end;
        }
        return { start, end };
    }

    auto NextLine(std::string_view str, std::string_view::size_type& pos) -> std::string_view
    {
        auto next = str.substr(0, str.find_first_of('\n'));
        pos += next.size();
        return next;
    }

    auto NextLineSkipStartWith(std::string_view str, std::string_view::size_type& pos, char c = '#') -> std::string_view
    {
        while (not str.empty()) {
            auto next = NextLine(str, pos);
            if (auto line = TrimLeft(next); not line.starts_with(c)) {
                return line;
            }

            str = str.substr(next.size());
        }
        return {};
    }

    template <typename T>
    struct HashStringLiteralParameters { };

    template <>
    struct HashStringLiteralParameters<uint32_t> {
        static constexpr auto prime = 0x01000193;
        static constexpr auto basis = 0x811c9dc5;
    };

    template <>
    struct HashStringLiteralParameters<uint64_t> {
        static constexpr auto prime = 0x100000001b3;
        static constexpr auto basis = 0xcbf29ce484222325;
    };

    template <typename T = uint64_t>
    struct HashStringLiteral {
        using parameters = HashStringLiteralParameters<T>;

        template <size_t N>
        static constexpr auto operator()(const char (&s)[N + 1]) noexcept -> T
        {
            T hash = parameters::basis;
            for (auto c : s) {
                hash = (hash ^ c) * parameters::prime;
            }
            return hash;
        }

        static constexpr auto operator()(std::string_view s) noexcept -> T
        {
            T hash = parameters::basis;
            for (auto c : s) {
                hash = (hash ^ c) * parameters::prime;
            }
            return hash;
        }
    };

    auto ToBool(std::string_view str) noexcept -> bool
    {
        bool ans = str == "true";
        if (not ans and str != "false") {
            std::cerr << "[WARNING]: use false if not true\n";
        }
        return ans;
    }
} // namespace strutil

auto ReadFile(std::string_view filepath, std::error_code& ec) -> std::string
{
    if (not std::filesystem::is_regular_file(filepath)) {
        ec = std::make_error_code(std::errc::no_such_file_or_directory);
        return {};
    }

    auto stream = std::ifstream { filepath.data(), std::ios::in | std::ios::binary };
    if (not stream) {
        return {};
    }

    // TODO(shmxce): add limitation of file size
    auto buf = std::stringstream {};
    buf << stream.rdbuf();
    return std::move(buf).str();
}

// 注释对应到j.conf中的字段名
struct {
    // timeout
    float timeout = 3.0;
    // memlimit
    uint32_t memlimit = 65536;
    // show_time_spent_each
    bool showTimeEach = true;
    // show_time_spent_all
    bool showTimeAll = true;
    // ignore_failed
    bool ignoreFailed = true;
    // executable_root
    std::string executableRoot { "." };
    // cases_root
    std::string casesRoot { "." };
    // compiler
    std::string compiler { "/usr/bin/g++" };
    // cxflags
    std::string cxflags;
    // after_create_commands
    std::string afterCreateCommands;

    auto readConfig(std::string_view conf = "j.conf")
    {
        using svregex_iterator = std::regex_iterator<std::string_view::iterator>;
        // using svmatch = std::match_results<std::string_view::iterator>;

        auto ec = std::error_code {};
        auto confContent = ReadFile(conf, ec);
        if (ec != std::error_code {}) {
            if (ec == std::errc::file_exists) {
                std::cout << "no j.conf, default config\n";
                return;
            }
            std::cout << "Read config file error: " << ec.message() << std::endl;
            std::exit(1);
        }

        // static const auto pattern = std::regex { R"((\w+)\s*=\s*([\w./-]+))" };
        static const auto pattern = std::regex { R"(([\w/.]+)\s*=\s*(\S.*\S)\s*)" };
        auto              it = svregex_iterator(confContent.data(), confContent.data() + confContent.size(), pattern);
        auto              end = svregex_iterator();

        auto shash = strutil::HashStringLiteral<> {};

#define CHECK_MATCH(target)                                                                                                    \
    if (key != (target))                                                                                                       \
    goto UNKNOWN_KEY_DEFAULT_CASE

        for (; it != end; ++it) {
            const auto& matchs = *it;

            auto line = std::string_view { matchs[0].first, matchs[0].second };
            auto key = std::string_view { matchs[1].first, matchs[1].second };
            auto value = std::string_view { matchs[2].first, matchs[2].second };

            switch (shash(key)) {
                case shash("timeout"): {
                    CHECK_MATCH("timeout");

                    auto [_, ec] = std::from_chars(value.begin(), value.end(), timeout);
                    if (ec == std::errc::invalid_argument) {
                        std::cout << std::format("invalid argument: {}\n\t\"{}\" is not a float", line, value);
                        std::exit(2);
                    } else if (ec == std::errc::result_out_of_range) {
                        std::cout << std::format("out of range: {}\n\t\"{}\" out of range of float", line, value);
                        std::exit(2);
                    }
                } break;

                case shash("memlimit"): {
                    CHECK_MATCH("memlimit");

                    auto [_, ec] = std::from_chars(value.begin(), value.end(), memlimit);
                    if (ec == std::errc::invalid_argument) {
                        std::cout << std::format("invalid argument: {}\n\t\"{}\" is not a integer", line, value);
                        std::exit(2);
                    } else if (ec == std::errc::result_out_of_range) {
                        std::cout << std::format("out of range: {}\n\t\"{}\" out of range of integer", line, value);
                        std::exit(2);
                    }
                } break;

                case shash("show_time_spent_each"): {
                    CHECK_MATCH("show_time_spent_each");
                    showTimeEach = strutil::ToBool(value);
                } break;

                case shash("show_time_spent_all"): {
                    CHECK_MATCH("show_time_spent_all");
                    showTimeAll = strutil::ToBool(value);
                } break;

                case shash("ignore_failed"): {
                    CHECK_MATCH("ignore_failed");
                    ignoreFailed = strutil::ToBool(value);
                } break;

                case shash("executable_root"): {
                    CHECK_MATCH("executable_root");
                    executableRoot = value;
                } break;

                case shash("cases_root"): {
                    CHECK_MATCH("cases_root");
                    casesRoot = value;
                } break;

                case shash("compiler"): {
                    CHECK_MATCH("compiler");
                    compiler = value;
                } break;

                case shash("cxflags"): {
                    CHECK_MATCH("cxflags");
                    cxflags = value;
                } break;

                case shash("after_create_commands"): {
                    CHECK_MATCH("after_create_commands");
                    afterCreateCommands = value;
                } break;

                default:
                UNKNOWN_KEY_DEFAULT_CASE:
#ifdef SHOW_UNKNOWN_ARGUMENTS
                    std::cerr << "unknown argument, ignored: " << line << std::endl;
#endif
                    break;
            }
        }
#undef CHECK_MATCH
    }
} OJConfig {};

struct TestCase {
    bool             enable;
    std::string_view hint;
    std::string_view input;
    std::string_view output;
};

// struct TestCaseContainer {
//     std::vector<TestCase> cases;
//     std::vector<std::string_view>
// };

// string_view-like viewer, with string function
class Tokenizer {
    std::string_view::iterator it_;
    std::string_view::iterator end_;

public:
    // NOLINTNEXTLINE
    constexpr Tokenizer(std::string_view source)
        : it_(source.begin())
        , end_(source.end())
    {
    }

    [[nodiscard]] auto begin() const noexcept
    {
        return it_;
    }

    [[nodiscard]] auto end() const noexcept
    {
        return end_;
    }

    [[nodiscard]] auto size() const noexcept
    {
        return std::distance(it_, end_);
    }

    [[nodiscard]] auto empty() const noexcept -> bool
    {
        return it_ == end_;
    }

    [[nodiscard]] auto str() const noexcept -> std::string_view
    {
        return { it_, end_ };
    }

    void trim() noexcept
    {
        while (it_ != end_ and (std::isspace(*it_) != 0)) {
            ++it_;
        }
    }

    auto nextlinend() noexcept -> std::string_view::iterator
    {
        while (it_ != end_ and *it_ != '\n') {
            ++it_;
        }

        const auto* ans = it_;
        skip();
        return ans;
    }

    // next line without comment: no last '\n', skip current '\n'
    auto nextlineWithoutComment() noexcept -> std::string_view
    {
        const auto* start = it_;
        auto        ans = strutil::TrimLeft({ start, nextlinend() });
        return (ans.empty() and not empty()) or ans.starts_with('#') ? nextlineWithoutComment() : ans;
    }

    // skip no checking
    void skip() noexcept
    {
        if (it_ != end_) {
            ++it_;
        }
    }
};

auto ReadTestCases(Tokenizer tokenizer, std::vector<TestCase>* testCases) -> Tokenizer
{
    using svmatch = std::match_results<std::string_view::iterator>;
    using svregex_iterator = std::regex_iterator<std::string_view::iterator>;

    // TODO(shmxce):
    // 1. support multiple key-value
    // 2. support run a test by name

    // 1:name, 2:kvs, 3:hint
    // <title>         -> "[<name>]<settings>:<hint>"
    //      <name>     -> ANY_STRING | EMPTY_STRING
    //      <settings> -> {<kvs>}
    //                  | EMPTY_STRING
    //      <hint>     -> ANY_STRING | EMPTY_STRING
    //      <kvs>      -> IDENGIFIER = [\w./]+
    //                  | EMPTY_STRING
    //
    // using regular expressions to implement the above syntax rules
    static const auto titlePattern = std::regex { R"(^\[(.*?)\](?:\{(.*?)\})?:\s*(.*?)\s*$)" };
    static const auto kvPattern = std::regex { R"([ \t,]*([a-zA-Z_]\w*)+[ \t]*=[ \t]*([\w./]+)[ \t,]*)" };
    svmatch           matchs;

    while (not tokenizer.empty()) {
        bool             enable = true;
        std::string_view hint, input, output;

        // title line
        {
            tokenizer.trim();
            auto line = tokenizer.nextlineWithoutComment();

            if (std::regex_match(line.begin(), line.end(), matchs, titlePattern)) {
                hint = { matchs[3].first, matchs[3].second };

                auto kvstring = std::string_view { matchs[2].first, matchs[2].second };
                auto next = svregex_iterator { kvstring.begin(), kvstring.end(), kvPattern };
                auto end = svregex_iterator {};

                while (next != end) {
                    const auto& kv = *next;
                    if (kv[1].compare("enable") == 0) {
                        enable = kv[2].compare("true") == 0;
                        if (not enable and kv[2].compare("false") != 0) {
                            std::cout << std::format(
                                "enable value must be 'true' or 'false', but got '{}'\n",
                                std::string_view { kv[2].first, kv[2].second });
                            std::exit(-1);
                        }
                    } else {
                        std::cout << "Warning: skip unknown test case configuration properties\n";
                    }
                    ++next;
                }

            } else {
                break;
            }
        }

        // input
        {
            tokenizer.trim();
            input = tokenizer.nextlineWithoutComment();
            const auto* inputEnd = input.end();

            while (not tokenizer.empty() and not tokenizer.str().starts_with("---")) {
                inputEnd = tokenizer.nextlinend();
            }
            input = std::string_view { input.begin(), inputEnd == tokenizer.end() ? inputEnd : inputEnd + 1 };
        }

        // output
        {
            // skip "---"
            tokenizer.nextlinend();
            const auto* outputBegin = tokenizer.begin();
            const auto* outputEnd = outputBegin;

            while (not tokenizer.empty() and not strutil::TrimLeft(tokenizer.str()).starts_with('#')
                   and not std::regex_match(
                       tokenizer.begin(), std::find(tokenizer.begin(), tokenizer.end(), '\n'), titlePattern)) {
                outputEnd = tokenizer.nextlinend();
            }
            output = std::string_view { outputBegin, outputEnd == tokenizer.end() ? outputEnd : outputEnd + 1 };
        }

        testCases->emplace_back(enable, strutil::Trim(hint), strutil::Trim(input), strutil::Trim(output));
    }

    return tokenizer;
}

struct PipeCommunicator {
    int pid;
    int input;
    int output;
};

/**
 * @brief bidirection-style popen
 *
 * @tparam Args
 */
template <typename... Args>
    requires(std::is_same_v<Args, const char*> and ...)
auto Popen(const char* command, Args... args) -> PipeCommunicator
{
    auto ret = PipeCommunicator { -1, -1, -1 };
    int  c2p[2];
    int  p2c[2];

    // 创建管道
    // 0: read, 1: write
    if (pipe(c2p) == -1) {
        return ret;
    }
    ret.output = c2p[0];

    if (pipe(p2c) == -1) {
        return ret;
    }
    ret.input = p2c[1];

    ret.pid = fork();
    if (ret.pid < 0) {
        close(c2p[0]);
        close(c2p[1]);
        close(p2c[0]);
        close(p2c[1]);
        return ret;
    }

    if (ret.pid == 0) {
        close(p2c[1]);
        close(c2p[0]);
        dup2(p2c[0], STDIN_FILENO);
        dup2(c2p[1], STDOUT_FILENO);

        // 在子进程中执行命令
        execlp(command, command, args..., nullptr);
        std::exit(-1);
    } else {
        // 父进程
        close(p2c[0]);
        close(c2p[1]);
    }
    return ret;
}

enum class JudgeResult {
    // AC, CE(compile error), WA, RE(runtime error), TLE, MLE
    AC,
    WA,
    RE,
    TLE,
    MLE
};

auto Judgeing(const std::string& executable, const std::vector<TestCase>& cases)
{
    const auto casesCount = cases.size();
    const char lineStartChar = OJConfig.showTimeEach ? '\n' : '\r';

    rusage usage;
    size_t passCount = 0, disabledCount = 0;
    auto   timeused = std::vector<float> {};
    timeused.reserve(casesCount);
    auto memused = std::vector<float> {};
    memused.reserve(casesCount);
    // auto       timeuse = std::valarray<float>();

    for (const auto [i, cs] : std::views::enumerate(cases)) {
        auto sstream = std::stringstream {};
        // sstream.clear();
        // std::cout << std::format(
        //     "\ncase [{}]:\n"
        //     "enable: {}\n"
        //     "hint: \"{}\"\n"
        //     "input: \"{}\"\n"
        //     "output: \"{}\"\n\n",
        //     i, cs.enable, cs.hint, cs.input, cs.output);

        auto judgeResult = JudgeResult::RE;
        int  returnCode = 1;

        if (not cs.enable) {
            ++disabledCount;
            continue;
        }

        auto process = Popen(executable.c_str());
        assert(process.pid > 0 and process.input > 0 and process.output > 0);

        // send input
        {
            int         total = cs.input.size();
            const char* p = cs.input.begin();
            int         nw = 0;
            while (total > 0 and (nw = write(process.input, p, total)) > 0) {
                p += nw;
                total -= nw;
            }
            if (nw == -1) {
                // NOLINTNEXTLINE
                goto ERROR_EXIT;
            }
            close(process.input);
            process.input = -1;
        }

        // recv output and wait
        {
            auto timeoutTask
                = std::async(std::launch::async, [&sstream, pid = process.pid, output = process.output, pusage = &usage] {
                      char buf[1024];
                      int  nr;
                      while ((nr = read(output, buf, sizeof(buf))) > 0) {
                          sstream.write(buf, nr);
                      }
                      if (nr == -1) {
                          std::cerr << std::format("\nJudgeing runtime error: {}\n", strerror(errno));
                          std::exit(3);
                      }
                      close(output);

                      int retCode;
                      // wait3(retCode, 0, usage);
                      if (wait4(pid, &retCode, 0, pusage) <= 0) {
                          std::cerr << std::format("\nJudgeing runtime error: {}\n", strerror(errno));
                          std::exit(3);
                      }
                      return retCode;
                  });

            if (timeoutTask.wait_for(std::chrono::milliseconds { static_cast<int>(OJConfig.timeout * 1000 + 100) })
                == std::future_status::timeout) {
                kill(process.pid, SIGKILL);
                close(process.output);
                judgeResult = JudgeResult::TLE;
                // NOLINTNEXTLINE
                goto SUMMARY_EACH;
            }

            returnCode = timeoutTask.get();
        }

        if (cs.output == strutil::Trim(sstream.view())) {
            if (returnCode == 0) {
                if (static_cast<float>(usage.ru_utime.tv_sec) + static_cast<float>(usage.ru_utime.tv_usec) / 1e6f
                    > OJConfig.timeout) {
                    judgeResult = JudgeResult::TLE;
                } else if (usage.ru_maxrss > OJConfig.memlimit) {
                    judgeResult = JudgeResult::MLE;
                } else {
                    judgeResult = JudgeResult::AC;
                }
            } else {
                judgeResult = JudgeResult::RE;
                // NOLINTNEXTLINE
                // goto SUMMARY;
            }
        } else {
            judgeResult = JudgeResult::WA;
        }

    SUMMARY_EACH:
        // clang-format off
        switch (judgeResult) {
            case JudgeResult::AC:
                std::cout << std::format("{}case [{}/{}]: AC  ------ {}s {:.2f}ms {:.2f}MB",
                    lineStartChar, i + 1, casesCount, usage.ru_utime.tv_sec,
                    static_cast<float>(usage.ru_utime.tv_usec) / 1e3f, static_cast<float>(usage.ru_maxrss) / (1 << 10)
                );
                ++passCount;
                timeused.push_back(static_cast<float>(usage.ru_utime.tv_sec) * 1000 + (usage.ru_utime.tv_usec / 1e3f));
                memused.push_back(static_cast<float>(usage.ru_maxrss));
                break;

            case JudgeResult::WA:
                std::cout << std::format("\ncase [{}/{}]: WA  ------ {}s {:.2f}ms {:.2f}MB",
                    i + 1, casesCount, usage.ru_utime.tv_sec,
                    usage.ru_utime.tv_usec / 1e3f, static_cast<float>(usage.ru_maxrss) / (1 << 10)
                );
                std::cout << "\n\tInput: \n\"" << cs.input
                          << "\"\n\tExpected: \n\"" << cs.output
                          << "\"\n\tGot: \n\"" << strutil::Trim(sstream.view())
                          << "\"\n";
                break;

            case JudgeResult::RE:
                std::cout << std::format("\ncase [{}/{}]: RE  ------ {}s {:.2f}ms {:.2f}MB",
                    i + 1, casesCount, usage.ru_utime.tv_sec,
                    usage.ru_utime.tv_usec / 1e3f, static_cast<float>(usage.ru_maxrss) / (1 << 10)
                );
                break;

            case JudgeResult::TLE:
                std::cout << std::format("\ncase [{}/{}]: TLE ------ >{}s -MB", i + 1, casesCount, OJConfig.timeout);
                break;

            case JudgeResult::MLE:
                std::cout << std::format("\ncase [{}/{}]: MLE ------ {}s {:.2f}ms {:.2f}MB",
                    i + 1, casesCount, usage.ru_utime.tv_sec,
                    usage.ru_utime.tv_usec / 1e3f, static_cast<float>(usage.ru_maxrss) / (1 << 10)
                );
                break;
        }
        // clang-format on

        if (judgeResult != JudgeResult::AC and not OJConfig.ignoreFailed) {
            std::cerr << "Early stop, because some tests are not passed\n";
            break;
        }

        std::flush(std::cout);
        std::flush(std::cerr);
    }

    // summary all
    std::cout << std::format(
        "{}==========================================\n"
        "Summary: total {}, disabled {}, passed {}({:.2f}%)\n",
        lineStartChar, casesCount, disabledCount, passCount,
        static_cast<float>(passCount) / (casesCount - disabledCount) * 100);

    // only for passed
    if (OJConfig.showTimeAll and timeused.size() > 0) {
        auto timeMean = std::accumulate(timeused.begin(), timeused.end(), 0.f) / static_cast<float>(timeused.size());
        // clang-format off
        auto timeStd = std::sqrt(
            std::transform_reduce(timeused.begin(), timeused.end(), 0.f, std::plus<> {},
                [timeMean](float cur) { return std::pow(cur - timeMean, 2); 
            }) / static_cast<float>(timeused.size())
        );
        // clang-format on
        auto timeMin = *std::min_element(timeused.begin(), timeused.end());
        auto timeMax = *std::max_element(timeused.begin(), timeused.end());

        auto memMean = std::accumulate(memused.begin(), memused.end(), 0.f) / static_cast<float>(memused.size());
        // clang-format off
        auto memStd = std::sqrt(
            std::transform_reduce(memused.begin(), memused.end(), 0.f, std::plus<> {},
                [memMean](float cur) { return std::pow(cur - memMean, 2); 
            }) / static_cast<float>(memused.size())
        );
        // clang-format on
        auto memMin = *std::min_element(memused.begin(), memused.end());
        auto memMax = *std::max_element(memused.begin(), memused.end());

        // Avg/std/min/max: \n
        // NOLINTBEGIN
        std::cout << std::format(
            "time: {:.2f}/{:.2f}/{:.2f}/{:.2f} mem: {:.2f}/{:.2f}/{:.2f}/{:.2f}\n", timeMean, timeStd, timeMin, timeMax,
            memMean / (1 << 10), memStd / (1 << 10), memMin / (1 << 10), memMax / (1 << 10));
        // NOLINTEND
    }

    return;

ERROR_EXIT:
    std::cerr << std::format("Judgeing runtime error: {}\n", strerror(errno));
    std::exit(3);
}

auto MakeExecutable(std::string_view phonyTarget, bool compile) -> std::string_view
{
    if (not phonyTarget.ends_with(".cpp")) {
        return phonyTarget;
    }
    if (not std::filesystem::is_directory(OJConfig.executableRoot)) {
        auto ec = std::error_code{};
        std::filesystem::create_directories(OJConfig.executableRoot, ec);
        if (ec) {
            std::cout << R"(Create directory ")" << OJConfig.executableRoot << R"(" error: )" << ec.message() << '\n';
            std::exit(1);
        }
    }

    auto seppos = phonyTarget.find_last_of(std::filesystem::path::preferred_separator);
    auto target
        = seppos == std::string_view::npos ? phonyTarget : phonyTarget.substr(seppos + 1, phonyTarget.size() - seppos - 5);
    OJConfig.casesRoot = phonyTarget.substr(0, seppos);

    auto executablePath = std::filesystem::path(OJConfig.executableRoot) / target;

    if (std::filesystem::exists(executablePath) and not compile) {
        return target;
    }

    auto compileCommands
        = std::format("{} {} {} -o {}", OJConfig.compiler, OJConfig.cxflags, phonyTarget, executablePath.native());
    std::cout << "[j] Info: Compiling ...\n";
    auto ret = std::system(compileCommands.c_str());
    if (ret != 0) {
        std::cerr << "[j] Error: compile failed with return code " << ret << '\n';
        std::exit(ret);
    }
    std::cout << "[j] Info: Compile output: " << executablePath.native() << "\n\n";

    return target;
}

/**
 * @brief 通过命令行传入要执行solution程序名，以<程序名>.case作为输入样例并比较结果。
 * j通过读取当前执行目录下的j.conf文件作为配置文件。
 *
 * @todo feat:
 * 1. 为executable参数支持可添加参数的执行程序，例如 `j "shortest-path <dijkstra/spfa>"` [-cv];
 * 2. 为测试用例支持通过另一个程序(check)验证solution程序的正确性：
 * 测试点通过设定参数 `{checkby="<commands>"}`
 * 设置验证程序，<output>段填写check程序验证后的输出内容（可以无输出，那么将只检查返回值是否为0），
 * j将验证check程序的输出结果是否一致/返回值是否正确来判断测试通过与否。
 *
 * @bug
 * 1. improvement: 测试用例输出需要和程序输出结果完全相同才能判定为正确，但是程序在输出一个列表时末尾可能会带有一个空格，
 * 测试用例文件中很容易忽略行尾的空格，导致输出结果完全一样却判定为错误。正确行为：忽略行尾空格。扩展：将output完全匹配作为case选项。
 *
 * @param argc
 * @param argv
 * @return int
 */
int main(int argc, const char** argv)
{
    std::string_view target;
    bool             verbose = false;
    bool             needCompile = true;
    bool             dontCreateIfNotExists = false;

    if (argc > 1) {
        target = argv[1];
        if (not target.ends_with(".cpp")) {
            std::cout << "Unsupported file type. Only c++ is supported.\n";
            return 1;
        }
        for (int i = 2; i < argc; ++i) {
            verbose |= std::strcmp(argv[i], "-v") == 0;
            needCompile &= std::strcmp(argv[i], "-r") != 0;
            dontCreateIfNotExists |= std::strcmp(argv[i], "-d") == 0;
        }
    } else {
        puts("No executable given\n");
        puts("\nUsage: j <executable/source-file-path> [options]\n"
             "options:\n"
             "\t-v: show config\n"
             "\t-r: dry-run if executable exists, dont compile source code\n"
             "\t-d: not create <executable>.cpp and <executable>.case if they are not exists\n");
        return -1;
    }

    OJConfig.readConfig();
    if (verbose) {
        std::cout << std::format(
            "solution: {}\n"
            "timeout: {}\n"
            "show_time_spent_each: {}\n"
            "show_time_spent_all: {}\n"
            "ignore_failed: {}\n"
            "executable_root: {}\n"
            "executable_root.recommend: build\n"
            "cases_root: {}\n"
            "compiler: {}\n"
            "cxflags: {}\n\n"
            "cxflags.recommend: -fcolor-diagnostics -Qunused-arguments -g -Wall -Wextra -O0 -std=c++14 -fuse-ld=lld "
            "-fsanitize=address -fsanitize=leak -fsanitize=undefined\n\n",
            target, OJConfig.timeout, OJConfig.showTimeEach, OJConfig.showTimeAll, OJConfig.ignoreFailed,
            OJConfig.executableRoot, OJConfig.casesRoot, OJConfig.compiler, OJConfig.cxflags);
    }

    if (not std::filesystem::exists(target)) {
        if (dontCreateIfNotExists) {
            std::cout << "Target \"" << target << "\" not found\n";
            return 0;
        }
        auto executableName = std::string_view { target.begin(), target.end() - 4 };
        auto caseFilename = std::string { executableName } + ".case";

        {
            std::filesystem::create_directories(std::filesystem::path(target).parent_path());
            std::ofstream { target.data() };
            std::ofstream { caseFilename, std::ios::app };
            std::printf("Creating a new solution: \n\t%s\n\t%s\n", target.data(), caseFilename.c_str());
        }

        if (not OJConfig.afterCreateCommands.empty()) {
            auto                   cmd = std::string {};
            std::string::size_type prev = 0;
            for (auto pos = OJConfig.afterCreateCommands.find("${EXECUTABLE}"); pos < OJConfig.afterCreateCommands.size();
                 pos = OJConfig.afterCreateCommands.find("${EXECUTABLE}", prev)) {
                cmd.append(OJConfig.afterCreateCommands.substr(prev, pos - prev));
                prev = pos + sizeof("${EXECUTABLE}") - 1;

                cmd.append(executableName);
            }
            cmd.append(OJConfig.afterCreateCommands.substr(prev));

            std::system(cmd.c_str());
        }
        return 0;
    }

    target = MakeExecutable(target, needCompile);

    auto ec = std::error_code {};
    auto caseFilepath = std::filesystem::path(OJConfig.casesRoot) / std::format("{}.case", target);
    auto content = ReadFile(caseFilepath.native(), ec);
    if (ec != std::error_code {}) {
        std::cout << "Error: reading case file(" << caseFilepath << ") error: " << ec.message() << std::endl;
        std::exit(1);
    }

    auto cases = std::vector<TestCase> {};
    cases.reserve(4);
    {
        auto tokenizer = Tokenizer { content };
        tokenizer = ReadTestCases(tokenizer, &cases);
        if (not tokenizer.empty()) {
            std::cerr << "Warning: some test cases are discarded, please check " << caseFilepath.native() << '\n';
        }
    }

    Judgeing((std::filesystem::path(OJConfig.executableRoot) / target).native(), cases);

    return 0;
}
