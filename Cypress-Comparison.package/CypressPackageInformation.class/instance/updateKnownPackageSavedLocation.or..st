updating
updateKnownPackageSavedLocation: aDirectory or: aBlock
	"Update the receiver to reflect it being a known package.
	 If aDirectory is unspecified, answer the result of evaluating aBlock."

	aDirectory isEmpty ifTrue: [^aBlock value].
	self
		beKnown;
		updateImageDefinitions;
		updateSavedLocation: aDirectory;
		updateChangesCount.
