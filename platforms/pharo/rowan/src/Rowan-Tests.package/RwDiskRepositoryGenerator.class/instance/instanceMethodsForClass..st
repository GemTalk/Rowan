definitions
instanceMethodsForClass: className

	^ self
		methodDefsFromSpec:
			{{#'testInstanceMethod1'.
			'accessing'.
			'testInstanceMethod1 ^ instVar1'}.
			{#'testInstanceMethod2'.
			'accessing'.
			'testInstanceMethod2 ^ classInstVar1'}}