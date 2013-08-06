"
notice: This work is protected by copyright. All rights reserved.
category: operations
"
unloadDefinition: aPatchOperation

	[aPatchOperation unloadDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
