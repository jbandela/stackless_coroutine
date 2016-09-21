#include "channel.hpp"
#include <future>
#include <iostream>

goroutine reader(std::shared_ptr<channel<int>> chan, int count) {
  await_channel_writer<int> writer{chan};
  for (int i = 0; i < count; ++i) {
    co_await writer.write(i);
  }
  chan->close();
}

auto make_processor(std::shared_ptr<channel<int>> inchan) {
  auto processor = [](std::shared_ptr<channel<int>> inchan,
                      std::shared_ptr<channel<int>> outchan) -> goroutine {
    await_channel_reader<int> reader{inchan};
    await_channel_writer<int> writer{outchan};
    for (;;) {
      auto res = co_await reader.read();
      if (res.first == false) {
        outchan->close();
        return;
      }
      std::this_thread::sleep_for(std::chrono::seconds{3});
      co_await writer.write(res.second * res.second);
    }
  };
  auto outchan = std::make_shared<channel<int>>();
  processor(inchan, outchan);
  return outchan;
}

goroutine writer(std::vector<std::shared_ptr<channel<int>>> inchans,
                 std::shared_ptr<channel<bool>> done_chan) {
  await_channel_writer<bool> done_writer{done_chan};
  std::vector<await_channel_reader<int>> readers{ inchans.begin(),inchans.end() };
  for (;;) {
    if (readers.empty()) {
      co_await done_writer.write(true);
      return;
    }
    auto res =
        co_await select_range(readers, [&](auto i) { std::cout << i << "\n"; });
    if (res.first == false) {
      readers.erase(res.second);
    }
  }
}

#include <chrono>
#include <list>

int main() {
  int threads;
  int count;
  std::cout << "\nEnter count\n";
  std::cin >> count;
  std::cout << "\nEnter threads\n";
  std::cin >> threads;

  std::vector<channel_runner> runners(threads);

  auto start = std::chrono::steady_clock::now();

  auto inchan = std::make_shared<channel<int>>();
  auto done_chan = std::make_shared<channel<bool>>();

  reader(inchan, count);

  std::vector<std::shared_ptr<channel<int>>> pout_chans;
  for (int i = 0; i < threads; ++i) {
    pout_chans.push_back(make_processor(inchan));
  }

  writer(pout_chans, done_chan);

  thread_suspender sus;
  sync_channel_reader<bool, thread_suspender> done_reader{done_chan, sus};

  done_reader.read();
  auto end = std::chrono::steady_clock::now();

  std::cout << "Took "
            << std::chrono::duration_cast<std::chrono::milliseconds>(end -
                                                                     start)
                   .count()
            << " ms\n";
}