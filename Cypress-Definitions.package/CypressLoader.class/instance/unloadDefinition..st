operations
unloadDefinition: aPatchOperation

	[aPatchOperation unloadDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
