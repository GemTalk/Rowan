"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: operations
"
unloadDefinition: aPatchOperation

	[aPatchOperation unloadDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
