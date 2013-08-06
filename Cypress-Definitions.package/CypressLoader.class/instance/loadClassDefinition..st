operations
loadClassDefinition: aPatchOperation

	[aPatchOperation loadClassDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
