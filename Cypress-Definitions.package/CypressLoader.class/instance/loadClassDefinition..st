"
notice: This work is protected by copyright. All rights reserved.
category: operations
"
loadClassDefinition: aPatchOperation

	[aPatchOperation loadClassDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
