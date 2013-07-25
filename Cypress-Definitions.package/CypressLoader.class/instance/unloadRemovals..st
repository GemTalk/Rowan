loading
unloadRemovals: somePatchOperations
	"Removals need to be done after adding classes and methods."

	somePatchOperations
		do: [:each | self unloadDefinition: each].
