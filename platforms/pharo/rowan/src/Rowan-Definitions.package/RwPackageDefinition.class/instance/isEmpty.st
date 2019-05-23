testing
isEmpty
	"Answer true if this definition does not actually define anything."

	^super isEmpty & classDefinitions isEmpty & classExtensions isEmpty