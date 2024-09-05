# j(judge) - A simple OJ tool.

A simple native OJ tool. One command line compile and run the problem solving code, and judge the pass rate according to the test case file. The statistics show which examples did not pass, as well as the elapsed time and memory consumption of the solution. The name `j` is just for convenience.

At present, the main support for linux platform, the `j` uses c++23, mainly the use of `std::format` and `std::range` without other dependencies.

# Usage

## Compile and install

compile with cmake:

``` bash
mkdir build
cmake -B build .
cmake --build build

# or
g++ -std=c++23 -O2 j.cpp -o j
```

This is just a standalone executable, you don't need to install it. If you want to make it easier to use, you can put this executable in a folder on your PATH, so it can be used directly from the terminal.

## Run

One-line command compilation execution: If the file does not exist, the j will automatically creates the file (.cpp) and creates a test case file (.case) with the file name.

``` bash
./j path/to/solution.cpp [-rdvh]
```

`j` 会读取同名文件后缀为".case"的测试样例文件进行判题，如path/to/solution.case。

## *.case 测试样例规则

每个测试点规则如下：

```
+----------------------------------+
| `[<name>]{<config>, ...}: <hint>`|
+----------------------------------+
| <input>                          |
+----------------------------------+
| ---                              |
+----------------------------------+
| <output>                         |
+----------------------------------+
```

第一行是测试点的title，包含测试点名称name、设置config和描述信息hint;
第二行是输入文本input;
第三行是分隔符，以"---"开头;
第四行是输出文本output。

- 输入/输出文本前后的空白字符会自动去除，然后通过管道输入给答题程序。
- 结果通过管道读取并去除前后空白字符。
- 如果输入输出的比较结果相同则表示测试通过，对于输出的中间结果，需要注意末尾是否有空格，可能会误判。
- 测试结果包含：AC, WA, RE, TLE, MLE。
- 以 `#` 开头的行表示注释，会自动跳过注释。
- 注释不能出现在 `<input>` 和 `<output>` 段，否则会将其作为输入输出进行读取和结果比较。

## 例子

该测试样例文件包含两个测试点，第一个未启用，第二个启用

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

## 配置文件

`j` 会读取当前路径下的 `j.conf` 配置文件，含义如字段名称所述。

``` Properties
timeout = 1.0
memlimit = 65536
show_time_spent_each = true
ignore_failed = true
after_create_commands = code ${EXECUTABLE}.c*
executable_root = build/bin/debug
cases_root = category/graph
compiler = /usr/bin/g++
cxflags = -g -Wall -Wextra -O0 -std=c++23 -fuse-ld=lld -fsanitize=address -fsanitize=undefined -fno-omit-frame-pointer
```