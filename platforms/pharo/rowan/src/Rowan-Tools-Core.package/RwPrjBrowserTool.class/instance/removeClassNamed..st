class browsing
removeClassNamed: className

	| projectSetDefinition loadedClass loadedClassProjectDef  loadedClassProjectDefName loadedClassExtensions |
	projectSetDefinition := RwProjectSetDefinition new.

	loadedClass := self _loadedClassNamed: className ifAbsent: [ ].
	loadedClassExtensions := #(). "only apply if the class itself is not managed by Rowan"

	loadedClass 
		ifNil: [
			"Give developer a chance to handle and abort the removal of unpackaged class"
			RwPerformingUnpackagedEditNotification signal: 'Attempt to remove an unpackaged class ', className printString, '. The removal will not be tracked by Rowan'.
			"Notification resumed, so continue with removal ... actual removal is performed below"
				loadedClassExtensions := self _loadedClassExtensionsNamed: className ifAbsent: [ #() ] ]
		ifNotNil: [
			| loadedPackage projectDef packageDef classDef |
			loadedPackage := loadedClass loadedPackage.
			projectDef := loadedClass loadedProject asDefinition.
			packageDef := projectDef packageNamed: loadedPackage name.
			classDef := packageDef classDefinitions at: loadedClass name.
			packageDef removeClassDefinition: classDef.
			projectSetDefinition addProject: projectDef.
			loadedClassProjectDef := projectDef.
			loadedClassProjectDefName := projectDef name ].

	loadedClassExtensions
		do: [ :loadedClassExtension | 
			| loadedClassExtensionPackage ceProjectDef cePackageDef |
			loadedClassExtensionPackage := loadedClassExtension loadedPackage.
			ceProjectDef := loadedClassExtension loadedProject asDefinition.
			ceProjectDef name = loadedClassProjectDefName
				ifTrue: [
					"extensions defined in same project as the class itself, entry already exists in projectSetDefinition"
					ceProjectDef := loadedClassProjectDef ]
				ifFalse: [ projectSetDefinition addProject: ceProjectDef ].
			cePackageDef := ceProjectDef packageNamed: loadedClassExtensionPackage name.
			cePackageDef
				removeClassExtensionDefinition:
					(cePackageDef classExtensions at: loadedClassExtension name) ].

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	loadedClass 
		ifNil: [
			"need to do the actual removal of the unpackaged class after Rowan has done it's job"
			| theClass |
			theClass := Rowan image objectNamed: className.
			(GsCurrentSession currentSession symbolList dictionariesAndSymbolsOf: theClass)
				do: [:ar | | dict key |
					"brute force removal of the class from system dictionaries"
					dict := ar at: 1.	
					key := ar at: 2.
					dict removeKey: key ] ]