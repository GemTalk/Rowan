queries
globalNamed: aString

	"Answer a global object with the given name.  If no object with the given name is found, returns nil."

	self subclassResponsibility: #'globalNamed:'