#include "channel.hpp"
#include <array>
#include <future>
#include <iostream>
#include <chrono>

using array_type = std::array<char, 32 * 1024 * 1024>;
using payload_type = std::unique_ptr<array_type>;
using pout_type = std::pair<int, payload_type>;

goroutine reader(std::shared_ptr<channel<pout_type>> chan,
                 std::shared_ptr<channel<payload_type>> pool, int count) {
  await_channel_writer<pout_type> writer{chan};
  await_channel_reader<payload_type> pool_reader{pool};
  for (int i = 0; i < count; ++i) {
    auto res = co_await pool_reader.read();
    co_await writer.write({i, std::move(res.second)});
  }
  chan->close();
}

auto make_processor(std::shared_ptr<channel<pout_type>> inchan) {
  auto processor =
      [](std::shared_ptr<channel<pout_type>> inchan,
         std::shared_ptr<channel<pout_type>> outchan) -> goroutine {
    await_channel_reader<pout_type> reader{inchan};
    await_channel_writer<pout_type> writer{outchan};
    for (;;) {
      auto res = co_await reader.read();
      if (res.first == false) {
        outchan->close();
        return;
      }
      std::this_thread::sleep_for(std::chrono::seconds{1});
      co_await writer.write({res.second.first, std::move(res.second.second)});
    }
  };
  auto outchan = std::make_shared<channel<pout_type>>();
  processor(inchan, outchan);
  return outchan;
}

goroutine writer(std::vector<std::shared_ptr<channel<pout_type>>> inchans,
                 std::shared_ptr<channel<payload_type>> pool,
                 std::shared_ptr<channel<bool>> done_chan) {
  await_channel_writer<bool> done_writer{done_chan};
  std::vector<await_channel_reader<pout_type>> readers{inchans.begin(),
                                                       inchans.end()};

  await_channel_writer<payload_type> pool_writer{pool};

  std::vector<pout_type> buffer;
  buffer.reserve(readers.size());
  int current = 0;
  for (;;) {
    if (readers.empty() && buffer.empty()) {
      co_await done_writer.write(true);
      return;
    }
    auto res = co_await select_range(readers, [&](auto &i) {
      buffer.push_back(std::move(i));
      std::push_heap(buffer.begin(), buffer.end(),
                     [](auto &a, auto &b) { return a.first > b.first; });
    });
    if (res.first == false) {
      readers.erase(res.second);
    }

    if (!buffer.empty() && buffer.front().first == current) {
      std::cout << buffer.front().first << "\n";
      ++current;
      std::pop_heap(buffer.begin(), buffer.end(),
                    [](auto &a, auto &b) { return a.first > b.first; });
      co_await pool_writer.write(std::move(buffer.back().second));
      buffer.pop_back();
    }
  }
}

int main() {
  int threads;
  int count;
  std::cout << "\nEnter count\n";
  std::cin >> count;
  std::cout << "\nEnter threads\n";
  std::cin >> threads;

  std::vector<channel_runner> runners(threads);

  auto start = std::chrono::steady_clock::now();

  auto inchan = std::make_shared<channel<pout_type>>();
  auto pool = std::make_shared<channel<payload_type>>(threads);
  auto done_chan = std::make_shared<channel<bool>>();

  reader(inchan, pool, count);

  std::vector<std::shared_ptr<channel<pout_type>>> pout_chans;
  for (int i = 0; i < threads; ++i) {
    pout_chans.push_back(make_processor(inchan));
  }

  writer(pout_chans, pool, done_chan);

  thread_suspender sus;
  sync_channel_reader<bool, thread_suspender> done_reader{done_chan, sus};
  sync_channel_writer<payload_type, thread_suspender> pool_writer{pool, sus};

  for (int i = 0; i < threads; ++i) {
    payload_type payload =
        std::make_unique<array_type>();
    pool_writer.write(std::move(payload));
  }

  done_reader.read();
  auto end = std::chrono::steady_clock::now();

  std::cout << "Took "
            << std::chrono::duration_cast<std::chrono::milliseconds>(end -
                                                                     start)
                   .count()
            << " ms\n";
}