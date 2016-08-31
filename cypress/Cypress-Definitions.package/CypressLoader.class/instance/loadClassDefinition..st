"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: operations
"
loadClassDefinition: aPatchOperation

	[ aPatchOperation loadClassDefinition: self defaultSymbolDictionaryName ]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
