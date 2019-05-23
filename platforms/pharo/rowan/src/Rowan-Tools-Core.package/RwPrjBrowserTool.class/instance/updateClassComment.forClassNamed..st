class browsing
updateClassComment: aString forClassNamed: className 

	"update the comment of the named class"

	| loadedClass projectDefinition packageDefinition classDefinition |
	loadedClass := Rowan image 
		loadedClassNamed: className 
		ifAbsent: [
			RwPerformingUnpackagedEditNotification signal: 'Attempt to add or modify a comment for the class ', className printString, '. The modification will not be tracked by Rowan'.
			"Notification resumed, so continue with add/modify"
			^ (Rowan globalNamed: className) comment: aString ].

	projectDefinition := loadedClass loadedProject asDefinition.
	packageDefinition := projectDefinition packageNamed: loadedClass loadedPackage name.
	classDefinition := packageDefinition classDefinitions at: loadedClass name.
	classDefinition comment: aString.

	self class load loadProjectDefinition: projectDefinition.