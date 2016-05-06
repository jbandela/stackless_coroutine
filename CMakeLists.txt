cmake_minimum_required(VERSION 3.0)
set(Boost_USE_STATIC_LIBS ON)
find_package(Boost 1.60 REQUIRED COMPONENTS coroutine context thread system )
include_directories(${Boost_INCLUDE_DIRS})
find_package(Threads)
add_executable(unit_test test.cpp test2.cpp)
set_property(TARGET unit_test PROPERTY CXX_STANDARD 14)
target_link_libraries(unit_test ${CMAKE_THREAD_LIBS_INIT} stdc++fs)

add_executable(async_http_client async_http_client.cpp)
set_property(TARGET async_http_client PROPERTY CXX_STANDARD 14)
target_link_libraries(async_http_client ${Boost_LIBRARIES}   ${CMAKE_THREAD_LIBS_INIT})

add_executable(async_http_client_no_exceptions async_http_client_no_exceptions.cpp)
set_property(TARGET async_http_client_no_exceptions PROPERTY CXX_STANDARD 14)
target_link_libraries(async_http_client_no_exceptions ${Boost_LIBRARIES}   ${CMAKE_THREAD_LIBS_INIT})
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang" OR CMAKE_CXX_COMPILER_ID MATCHES "GNU")
	target_compile_options(async_http_client_no_exceptions PRIVATE -fno-exceptions)
endif()

add_executable(example example.cpp)
set_property(TARGET example PROPERTY CXX_STANDARD 14)
target_link_libraries(example ${Boost_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})

add_executable(perf_writer perf_writer.cpp)
set_property(TARGET perf_writer PROPERTY CXX_STANDARD 14)
target_link_libraries(perf_writer ${Boost_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})

add_executable(stackfull_coroutines_perf stackfull_coroutines_perf.cpp)
set_property(TARGET stackfull_coroutines_perf PROPERTY CXX_STANDARD 14)
target_link_libraries(stackfull_coroutines_perf ${Boost_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})

add_executable(stackless_coroutines_perf stackless_coroutines_perf.cpp)
set_property(TARGET stackless_coroutines_perf PROPERTY CXX_STANDARD 14)
target_link_libraries(stackless_coroutines_perf ${Boost_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})

