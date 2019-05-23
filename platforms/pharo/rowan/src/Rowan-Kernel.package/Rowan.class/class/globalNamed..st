public
globalNamed: aName

	"Answer a global object with the given name.  If no object with the given name is found, returns nil."

	^ self platform globalNamed: aName