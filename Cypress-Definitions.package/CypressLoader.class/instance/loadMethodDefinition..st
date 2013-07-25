operations
loadMethodDefinition: aPatchOperation

	[aPatchOperation loadMethodDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
