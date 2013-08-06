"
notice: This work is protected by copyright. All rights reserved.
category: loading
"
unloadRemovals: somePatchOperations
	"Removals need to be done after adding classes and methods."

	somePatchOperations
		do: [:each | self unloadDefinition: each].
