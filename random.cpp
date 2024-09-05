#include <charconv>
#include <cstdio>
#include <cstring>
#include <random>

int main(int argc, const char** argv)
{
    if (argc < 2) {
        perror("error");
        return 1;
    }

    int n = 0;
    std::from_chars<int>(argv[1], argv[1] + std::strlen(argv[1]), n);

    auto d = std::random_device{};
    auto rng = std::mt19937{d()};
    auto dist = std::uniform_int_distribution<int>{};

    printf("%d\n", n);
    while (n--) {
        printf("%d ", dist(rng));
    }
    printf("\n");
}