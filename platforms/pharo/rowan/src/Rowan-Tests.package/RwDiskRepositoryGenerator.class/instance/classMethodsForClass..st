definitions
classMethodsForClass: className

	^ self
		methodDefsFromSpec:
			{{#'testClassMethod1'.
			'accessing'.
			'testClassMethod1 ^ ClassVar1'}.
			{#'testClassMethod2'.
			'accessing'.
			'testClassMethod2 ^ true'}}