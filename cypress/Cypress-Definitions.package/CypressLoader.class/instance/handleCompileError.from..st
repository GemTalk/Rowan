"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: loading
"
handleCompileError: aCompileError from: aPatchOperation

	| undefinedSymbolErrors otherErrors |
	undefinedSymbolErrors := aCompileError errorDetails
				select: [:each | each first = 1031].
	otherErrors := aCompileError errorDetails
				reject: [:each | each first = 1031].
	undefinedSymbolErrors do: [:each | self requirements add: each last].
	aCompileError pass
