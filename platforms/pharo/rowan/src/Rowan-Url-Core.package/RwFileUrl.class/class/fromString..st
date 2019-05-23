instance creation
fromString: aString
	"Method that can be called explicitly to create a FileUrl."

	^self new privateInitializeFromText: aString
