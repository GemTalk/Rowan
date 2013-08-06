"
notice: This work is protected by copyright. All rights reserved.
category: operations
"
postLoad: aPatchOperation

	[aPatchOperation postLoadDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
