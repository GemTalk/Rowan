private
_doProjectSetLoad_254: projectSetDefinition instanceMigrator: instanceMigrator originalProjectSet: originalProjectSet processedClassNames: processedClassNames
	| copiedProjectSetDef theClassName theClass projectDef theLoadedProject loadedClass packageDef |
	[ ^ self
		_loadProjectSetDefinition_254: projectSetDefinition
		instanceMigrator: instanceMigrator ]
			on: RwExistingVisitorAddingExistingClassNotification
			do: [:ex | 
				theClassName := ex classDefinition name.
				(processedClassNames includes: theClassName) ifTrue: [ ex resume ].
				theClass := Rowan globalNamed: theClassName.
				theClass isBehavior ifFalse: [ self halt. ex pass ].
				theLoadedProject := Rowan image loadedProjectNamed: theClass rowanProjectName.
				theLoadedProject 
					ifNil: [ 
						"the loaded project should not be nil - if it is, pass the notification"
						ex pass ].
				(originalProjectSet projectNamed: theLoadedProject name ifAbsent: []) 
					ifNotNil: [
						"If the loadedProject is in the originalProjectSet, then is likely to be a class move - resume and let the chips fall where they may"
						ex resume ].
				copiedProjectSetDef := projectSetDefinition copy.
				"a project in the original project set is taking ownership of an already  loaded class,
					remove the class from the original project's package and attempt a reload"
				projectDef := copiedProjectSetDef 
					projectNamed: theLoadedProject name
					ifAbsent: [ 
						projectDef := theLoadedProject asDefinition.
						copiedProjectSetDef addProject: projectDef.
						projectDef ].
				loadedClass := Rowan image loadedClassNamed: theClassName.
				packageDef := projectDef packageNamed: loadedClass loadedPackage name.
				packageDef removeClassNamed: theClassName.
				processedClassNames add: theClassName ].
	"trim the stack"
	^ self _doProjectSetLoad_254: copiedProjectSetDef instanceMigrator: instanceMigrator originalProjectSet: originalProjectSet processedClassNames: processedClassNames
