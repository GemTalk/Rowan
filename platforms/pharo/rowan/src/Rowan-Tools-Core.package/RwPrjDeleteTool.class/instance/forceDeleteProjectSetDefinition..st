delete project definitions
forceDeleteProjectSetDefinition: projectSetDefinitionToDelete

	"Remove the classes and methods managed by the listed project definitions."

	"The classes and methods are removed from the system"

	"If there are subclasses of classes in other projects not in the set, the delete will NOT fail."

	| loadedProjectSet classesToDelete methodsToDelete mySymbolList |
	loadedProjectSet := projectSetDefinitionToDelete deriveLoadedThings.

	classesToDelete := IdentitySet new.
	methodsToDelete := IdentityKeyValueDictionary new.

	loadedProjectSet do: [:theLoadedProject |
		theLoadedProject 
			loadedPackagesDo: [ :loadedProject :loadedPackage | "ignored" ] 
			loadedClassedDo: [ :loadedProject :loadedPackage :loadedClass | 
				"collect the classes defined in the project"
				classesToDelete add: loadedClass handle currentVersion ] 
			loadedClassExtensionsDo: [:loadedProject :loadedPackage :loadedClassExtension | "ignored" ] 
			loadedInstanceMethodsDo: [:loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod |
				loadedClassOrClassExtension isLoadedClassExtension
					ifTrue: [ 
						"collect extension instance methods"
						((methodsToDelete at: loadedClassOrClassExtension handle currentVersion ifAbsentPut: [Dictionary new]) 
							at: 'instance' ifAbsentPut: [ IdentitySet new ]) add: loadedMethod handle selector ] ] 
			loadedClassMethodsDo: [:loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
				loadedClassOrClassExtension isLoadedClassExtension
					ifTrue: [ 
						"collect extension class methods"
						((methodsToDelete at: loadedClassOrClassExtension handle currentVersion ifAbsentPut: [Dictionary new]) 
							at: 'class' ifAbsentPut: [ IdentitySet new ]) add: loadedMethod handle selector ] ] ].
	classesToDelete do: [:class | 
		"remove any classes that are going to be deleted from methodsToDelete"
		methodsToDelete removeKey: class ifAbsent: [ ] ].

	loadedProjectSet do: [:theLoadedProject |
		"remove the methods, classes, packages and projects from loaded things"
		Rowan projectTools disown disownProjectNamed: theLoadedProject name ].

	mySymbolList := System myUserProfile symbolList. "persistent symbol list"
	classesToDelete do: [:class |
		"remove classes from system"
		(mySymbolList dictionariesAndSymbolsOf: class) do: [:ar |
			GsFile gciLogServer: 'Forcibly delete class ', class name asString printString.
			(ar at: 1) removeKey: (ar at: 2) ] ].

	methodsToDelete keysAndValuesDo: [:class :selectorDict |
		| behavior |
		"remove extension methods from the classes remaining in system"
		GsFile gciLogServer: 'Forcibly remove extension methods from class ', class name asString printString.
		GsFile gciLogServer: '	instance'.
		(selectorDict at: 'instance' ifAbsent: [ #() ]) do: [:selector |
			GsFile gciLogServer: '		', selector asString printString.
			class removeSelector: selector ].
		behavior := class class.
		GsFile gciLogServer: '	class'.
		(selectorDict at: 'class' ifAbsent: [ #() ]) do: [:selector |
			GsFile gciLogServer: '		', selector asString printString.
			behavior removeSelector: selector ] ].