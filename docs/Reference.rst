Reference
=========

Signatures
----------

.. cpp:function:: BlockLambda(auto& context, auto& variables)

	Lambda that is passed to ``stackless_coroutine::make_block`` or ``stackless_coroutine::make_while_true``

	:param context: :cpp:class:`stackless_coroutine::context_type`
	:param variables: A reference to the coroutine variables class for this coroutine

.. cpp:function:: FirstBlockLambda(auto& context, auto& variables, auto&&... coroutine_parameters)


	First lambda that is passed to ``stackless_coroutine::make_block`` 

	:param context: :cpp:class:`stackless_coroutine::context_type`
	:param variables: A reference to the coroutine variables class for this coroutine
	:param coroutine_parameters: Parameters that are passed to the coroutine

.. cpp:function:: BlockLambdaAfterAsync(auto& context, auto& variables, auto&&... async_handler_parameters)


	Lambda that follows a call to a async function

	:param context: :cpp:class:`stackless_coroutine::context_type`
	:param variables: A reference to the coroutine variables class for this coroutine
	:param parameters: Parameters that are passed to the async handler

.. cpp:function:: CoroutineCompletionFunction(auto& variables, std::exception_ptr ep, stackless_coroutine::operation op)

	Lambda or function that is passed in to ``make_coroutine`` and is called exactly once upon completion of the coroutine

	:param variables: A reference to the coroutine variables class for this coroutine
	:param ep: An exception_ptr that has any exception that escaped a ``BlockLambda``. If no exception occurred, will be empty
	:param op: Enumeration of ``stackless_coroutine::operation`` which tells how the coroutine finished
		
		* ``stackless_coroutine::_done`` - Coroutine finished via exiting the last ``BlockLambda`` without any exceptions
		* ``stackless_coroutine::_return`` - Coroutine finished via ``context.do_return()`` or ``context.do_async_return()``. This indicates an early return.
		* ``stackless_coroutine::_exception`` - Coroutine finished via an exception that escaped a ``BlockLambda``. The exception will be in ``ep``.



 
Functions
---------

All functions mentioned are in ``namespace stackless_coroutine``




.. cpp:namespace:: stackless_coroutine



.. cpp:function :: template<class... BlockFunction> auto make_block(BlockFunction&& ... blockfunctions)

			Makes a block which is analogous to a block or compound statement in C++. This consists of a sequence of BlockFunctions

			:param blockfunctions: Sequence of ``BlockFunctions``

			:return: Returns a block which then may be passed to ``make_coroutine``

.. cpp:function :: template <class CoroutineVariables, class Block, class CoroutineCompletionFunction, class... ConstructorArguments> 	auto make_coroutine(Block block, CoroutineCompletionFunction f, ConstructorArguments &&... arguments) 

			Makes a coroutine.

			:param block: block created by call to ``make_block``

			:param f: ``CoroutineCompletionFunction`` which will be called when coroutine finishes

			:param arguments: Arguments to the construct of ``CoroutineVariables``

			:return: Returns a coroutine.

.. cpp:function :: template<class... BlockFunction> auto make_while_true(BlockFunction&& ... blockfunctions)

			Makes the equivalent of ``while(true){...}`
			``blockfunctions`` will be executed repeatedly until one of the follow happens

				 * Call to ``context.do_return``
				 * Call to ``context.do_async_return``
				 * An exception escapes a ``BlockLambda``

			:param blockfunctions: Sequence of ``BlockFunctions``

			:return: Returns a ``BlockLambda`` functor which may be used anywhere a ``BlockLambda`` is used. This allows ``make_while_true`` to be nested



Classes
-------

All functions mentioned are in ``namespace stackless_coroutine``

.. cpp:class:: context_type
	
	This is more a concept than a class. This is the type of ``context`` in ``BlockLambda``


	.. cpp:function:: template <class... T> void operator()(T &&... t)
		
		This will call the ``BlockLambdaAfterAsync`` passing ``t`` as the arguments after ``context`` and ``variables``
		Note this is only available if the ``BlockLambda`` has ``return context.do_async()``

	.. cpp:function::   operation do_return()

		Does an early return from the coroutine

	.. cpp:function::   async_result do_async_return()

		Same as ``do_return``, but used in a ``BlockLambda`` that calls ``context.do_async()`` in one or more branches of the lambda



	.. cpp:function::   operation do_break()

		Analogous to ``break``
		Breaks out of a ``make_while_true`` loop

	.. cpp:function::   async_result do_async_break()

		Same as ``do_break``, but used in a ``BlockLambda`` that calls ``context.do_async()`` in one or more branches of the lambda


	.. cpp:function::   operation do_continue()

		Analogous to ``continue``
		Goes to the top of a ``make_while_true`` loop

	.. cpp:function::   async_result do_async_continue()

		Same as ``do_continue``, but used in a ``BlockLambda`` that calls ``context.do_async()`` in one or more branches of the lambda

	.. cpp:function::   operation do_next()

		Goes to next ``BlockLambda`` in the sequence

	.. cpp:function::   async_result do_async()

		Signals to stackless_coroutine that an async function with ``context`` as the callback has been called.
		The coroutine will be suspended until the async function calls ``context`` resulting in the ``BlockLambdaAfterAsync`` (the ``BlockLambda`` following this ``BlockLambda``)
		being called with the arguments the async function passed to the callback



	.. cpp:function::  static coroutine_context_t get_context(void *v)

		Creates a ``coroutine_context_t`` from ``void*`` which points to the ``variables`` passed to ``BlockLamda``. The purpose of this static function is to
		enable easy interop with legacy C functions that take a function pointer and a ``void*`` that will be passed to the function callback







  


