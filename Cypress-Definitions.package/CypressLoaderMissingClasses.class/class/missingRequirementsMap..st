"
notice: This work is protected by copyright. All rights reserved.
category: instance creation
"
missingRequirementsMap: aDictionary
	"Answer an instance of the receiver initialized on the specified
	 missing requirements. aDictionary maps prerequisite names to
	 a collection of dependent definitions."

	^self new
		initializeRequirementsMap: aDictionary;
		yourself
