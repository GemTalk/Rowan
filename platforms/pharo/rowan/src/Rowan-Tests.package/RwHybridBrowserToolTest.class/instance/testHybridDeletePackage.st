tests
testHybridDeletePackage

	"add 2 classes in 2 packages, extend each class from the other package ... remove the package"

	| normalClass projectName packageNames packageName1 packageName2 otherClass className1 className2 theLoadedProject theLoadedPackage theLoadedClassOrClassExtension classNames |
	className1 := 'SimpleHybridNormal1'.
	className2 := 'SimpleHybridOther1'.
	classNames := {className1.
	className2}.
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	normalClass := Object
		rwSubclass: className1
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	normalClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName2 asLowercase.
	self assert: normalClass new foo = 'foo'.

	otherClass := Object
		rwSubclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().
	self assert: otherClass rowanPackageName = packageName2.

	otherClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName1 asLowercase.
	self assert: otherClass new foo = 'foo'.

	Rowan projectTools browser removePackageNamed: packageName1.

	self assert: (Rowan globalNamed: className1) isNil.
	self assert: (Rowan globalNamed: className2) == otherClass.
	self should: [ otherClass new foo ] raise: MessageNotUnderstood.

	theLoadedProject := Rowan image loadedProjectNamed: projectName.	"Traverse the loaded thing structure and verify that it matches the expected structure"

	theLoadedProject
		loadedPackagesDo: [ :loadedProject :loadedPackage | 
			self assert: theLoadedProject == loadedProject.
			theLoadedPackage := loadedPackage.
			self
				assert:
					(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
			self assert: (packageNames includes: loadedPackage name) ]
		loadedClassedDo: [ :loadedProject :loadedPackage :loadedClass | 
			self assert: theLoadedProject == loadedProject.
			self assert: theLoadedPackage == loadedPackage.
			self
				assert:
					(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
			theLoadedClassOrClassExtension := loadedClass.

			loadedPackage name = packageName1
				ifTrue: [ 
					self
						assert: false
						description:
							'unexpected class ' , loadedClass name , ' in package ' , loadedPackage name ]
				ifFalse: [ 
					loadedPackage name = packageName2
						ifTrue: [ self assert: loadedClass name = className2 ]
						ifFalse: [ self assert: false description: 'unexpected package ' , loadedPackage name ] ] ]
		loadedClassExtensionsDo: [ :loadedProject :loadedPackage :loadedClassExtension | 
			self assert: false description: 'no class extensions expected in package '.
			loadedPackage name ]
		loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
			self assert: false description: 'no instance methods expected in class'.
			loadedClassOrClassExtension name ]
		loadedClassMethodsDo: [ :loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
			self assert: false description: 'no class methods expected in class'.
			loadedClassOrClassExtension name ]