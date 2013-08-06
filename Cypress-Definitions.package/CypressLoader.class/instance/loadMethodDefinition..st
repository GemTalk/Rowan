"
notice: This work is protected by copyright. All rights reserved.
category: operations
"
loadMethodDefinition: aPatchOperation

	[aPatchOperation loadMethodDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
