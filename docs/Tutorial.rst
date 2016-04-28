Tutorial
========

Prerequisites
-------------

This tutorial assumes that you are familiar with Boost.Asio and asynchronous programming using callbacks.

Goal
----

We are going to use Boost.Asio to build a function that will print out the resource at an http url with the headers

Our function will have this signature

.. literalinclude:: ../example.cpp
	:language: cpp
	:lines: 11-13

Coroutine variables
-------------------

We need some way of persisting variables across invocations of async functions. To do this we will define a structure to hold these variables.

.. literalinclude:: ../example.cpp
	:language: cpp
	:lines: 16-23


Coroutine block
---------------

The logic in a coroutine is defined by the block (or compound statement), which is a sequence of lambdas (or function objects). We open our block like this

.. literalinclude:: ../example.cpp
	:language: cpp
	:lines: 25

First lambda
------------

We now define the first lambda of the block

.. literalinclude:: ../example.cpp
	:language: cpp
	:lines: 30-48

All lambdas in a block take ``auto& context, auto& variables``. Some lamdas also take other parameters. The first lamda in a coroutine can take additional parameters that must be passed when
invoking the coroutine. In this case it takes ``const std::string &host, const std::string &path``. These are used to construct the request stream that will be used later.

Notice how we access the members of ``coroutine_variables_t`` using ``variables``

We then set up the ``query`` object and call ``async_resolve``, passing in ``context`` for the callback.

The last statement

.. literalinclude:: ../example.cpp
	:language: cpp
	:lines: 47

Tells stackless_coroutine that an async operation has been performed, and that, the coroutine should be suspended.


Handling callbacks
------------------

In the previous section, we saw how to pass ``context`` as a callback

.. literalinclude:: ../example.cpp
	:language: cpp
	:lines: 47

In this section,  we will see how see how we access the results of the callback.

Here is the next lambda

.. literalinclude:: ../example.cpp
	:language: cpp
	:lines: 53-65


As mentioned earlier, lamdas can other parameters in addition to ``auto &context, auto &variables``. When a lambda follows a lambda that returns ``context.do_async()``, it takes as parameters 
the results of the callback, which in this case are the error code and iterator.

We examine the error_code ec, and if it is set, we throw an exception. Note that the exception will exit the coroutine.

If we had no errors, we call ``async_connect`` passing in the iterator, and ``context`` as the callback.

We return ``context.do_async()`` signaling stackless_coroutine to suspend the coroutine.

Sending the http request
------------------------

We use ``async_write`` to send the http request

.. literalinclude:: ../example.cpp
	:language: cpp
	:lines: 67-76

We then check for errors and throw an exception if there is an error

.. literalinclude:: ../example.cpp
	:language: cpp
	:lines: 77-82

Looping
-------

Writing loops is one of the harder parts of async programming with callbacks. Stackless_coroutines makes this much easier. In this session we will see see how to do loops.

To create a loop, where you would use a lambda, you use ``make_while_true`` like this.

.. literalinclude:: ../example.cpp
	:language: cpp
	:lines: 86

Then as arguments for ``make_while_true`` you pass in a sequence of lambdas. These lambdas will be executed repeatedly until one of the following happens
to exit the loop

	 * Call to ``context.do_break``
	 * Call to ``context.do_async_break``



A loop can also be exited by one of the following which will exit the whole coroutine

	 * Call to ``context.do_return``
	 * Call to ``context.do_async_return``
	 * An exception escapes a lamda


Let us now pass in the first lambda to ``make_while_true``

.. literalinclude:: ../example.cpp
	:language: cpp
	:lines: 87-95

This lambda, apart from from being inside ``make_while_true`` is just like the other other coroutine lambdas.

First, we resize ``variables.current`` to have size 20. In real life we would set the buffer size to much bigger. For this example, we made it small to make it more
likely that we run this loop multiple times.

We make the call to  ``async_read_some`` just like the other async calls in the coroutine, passing in ``context`` as the callback, and returning ``context.do_async`` from the lambda.



In the next lambda we receive the results of ``async_read_some```

.. literalinclude:: ../example.cpp
	:language: cpp
	:lines: 96-117




We check if there is a an error. If there is an error, if it is not eof, we output the error. We also call ``context.do_break`` to break the loop.

If there is not an error, we call ``context.do_continue`` continue the loop

With this we are finished with our call to ``make_while_true`` and ``make_block``



Creating the coroutine
----------------------

To create the coroutine we call ``make_coroutine``


.. literalinclude:: ../example.cpp
	:language: cpp
	:lines: 123-124


``make_coroutine`` takes as a template parameter the type of the coroutine variables. It also takes the block, and the function to call upon completion of the coroutine. It also takes, arguments to
the coroutine variables type's constructor.


Calling the coroutine
----------------------

We call the coroutine ``co`` passing in the arguments to the first lambda of the block - ``host`` and ``path``

We then return from the ``print_url`` function

.. literalinclude:: ../example.cpp
	:language: cpp
	:lines: 129-130



int main()
-----------

Here is our	``main`` function that uses ``print_url``

.. literalinclude:: ../example.cpp
	:language: cpp
	:lines: 133-151


Pay particular attention to the lambda we pass to ``print_url`` as that is the callback function for the coroutine

.. literalinclude:: ../example.cpp
	:language: cpp
	:lines: 142-148

The callback function takes a reference to the coroutine variables, an ``std::exception_ptr`` containing any exception that escaped any of the coroutine block lambdas, and ``stackless_coroutine::operation`` which
tells whether the coroutine exited via an early return. You can usually ignore this last parameter


Complete example.cpp
--------------------

.. literalinclude:: ../example.cpp
	:language: cpp































































