"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: operations
"
postLoad: aPatchOperation

	[aPatchOperation postLoadDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
