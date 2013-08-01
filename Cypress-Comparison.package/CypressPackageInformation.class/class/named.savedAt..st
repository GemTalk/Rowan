instance creation
named: aString savedAt: aDirectory
	"Answer an instance of the receiver representing the named package.
	 If the package was saved in aDirectory, load up the saved details."

	^self new
		initializeFromName: aString andSavedLocation: aDirectory;
		yourself
