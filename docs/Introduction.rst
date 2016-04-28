Introduction
============

Overview
--------

Stackless_coroutine provides a portable, self-contained, library only, C++14 emulation of stackless coroutines without macros.

Features
--------

* Single header with no dependencies
* No Macros
* Can be used with exception support disabled
* Can be used without dynamic memory allocation

Motivation
----------
Asynchronous programming is becomming more common in Modern C++. However, the callback based model, whether through the use of explicit callbacks or through futures with completion callbacks, 
can result in code that is difficult to write and read especially when trying to call an asynchronous function in a loop.

There have been several attempts to mitigate this

	* Stackfull coroutines for example Boost.Coroutine 
		http://www.boost.org/doc/libs/1_60_0/libs/coroutine/doc/html/index.html 

	* Stackless coroutines for example co_await proposed by Microsoft 
		https://isocpp.org/blog/2015/04/n4402

	* Various macro based stackless coroutine libraries for example 
		http://www.boost.org/doc/libs/1_60_0/doc/html/boost_asio/overview/core/coroutine.html
		https://github.com/jamboree/co2 


However, there are issues that prevent universal adoption of these solutions

	* Stackfull coroutines - Require linking to a separate library. In addition, there are concerns with how much stack space a coroutine needs. Too much stackspace wastes memory, too little risks stack overflow.
	  Segmented stacks can help with this, but may not be supported on all compilers/platforms.

	* Stackless coroutines - So far only implemented in Visual C++. Lack of consensus at the Standards meeting caused it to be put into a TS rather than into the standard

	* Macro based libraries - Macro magic :(


This library is another alternative that uses C++14 generic lambdas to implement stackless coroutines without macros. The library is a single header file and has no dependencies other than C++14.


