Class {
	#name : 'RwGsMethodAdditionSymbolDictPatchV2',
	#superclass : 'RwGsMethodPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'deleting' }
RwGsMethodAdditionSymbolDictPatchV2 >> deleteMethodNewClassesSymbolList: createdClasses andExistingClasses: tempSymbols deletedMethodPatches: deletedMethods [
	"this method is being called because the reciver is adding a method that MAY conflict with an existing method ... so we are proactively deleting any old method versions BEFORE the new method version is added. Note that if the loaded method does not exist then it is truly a new method"

	| loadedClass sel loadedMethod theBehavior |
	(self dynamicInstVarAt: #'hasCompileError')
		ifNil: [ 
			self
				error:
					'Unexpected use of RwGsMethodAdditionSymbolDictPatchV2>>deleteMethodNewClassesSymbolList:andExistingClasses:' ].
	theBehavior := self dynamicInstVarAt: #originalClassVersion.
	self
		primeBehaviorNewClassesSymbolList: createdClasses
		andExistingClasses: tempSymbols.
	theBehavior
		ifNil: [ 
			"class is being deleted, nothing else to do"
			^ self ].
	loadedClass := Rowan image loadedClassNamed: theBehavior name ifAbsent: [  ].
	sel := methodDefinition selector.
	loadedMethod := self isMeta
		ifTrue: [ loadedClass classMethodDefinitions at: sel ifAbsent: [  ] ]
		ifFalse: [ loadedClass instanceMethodDefinitions at: sel ifAbsent: [  ] ].
	loadedMethod
		ifNotNil: [ 
			deletedMethods isEmpty
				ifFalse: [ self halt ].
			(deletedMethods detect: [ :each | each selector asSymbol = sel ] ifNone: [  ])
				ifNotNil: [ 
					"if I have a method delete scheduled, get out of dodge?"
					self halt: 'do not have a test case for this ... yet ... in search of one though:)'  ] ]
		ifNil: [ 
			"the loaded method does not exist, so the compile error that lead us here is in a new method"
			^ self ].
	 self
		symbolDictionaryRegistry
		deleteMethod: sel
		for: theBehavior
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'deleting' }
RwGsMethodAdditionSymbolDictPatchV2 >> deleteNewVersionMethodNewClassesSymbolList: createdClasses andExistingClasses: tempSymbols [
	"remove the method from deleted things"

	| cm |
	(cm := self compiledMethod)
		ifNil: [ 
			"if no method for this selector exists in the class, then it  is an added method with a compile error that has been handled ... see RwCompileErrorCompilingMethodsForNewClassVersionNotification"
			^ self ].
	self symbolDictionaryRegistry
		_doDeleteCompiledMethodFromLoadedThings: cm
		for: (self dynamicInstVarAt: #'originalClassVersion')
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'deleting' }
RwGsMethodAdditionSymbolDictPatchV2 >> installMethod [

(self dynamicInstVarAt: #hasCompileError) ifNotNil: [:msg | ^self ].
	self symbolDictionaryRegistry
		addNewCompiledMethod: compiledMethod
		for: behavior
		protocol: self propertiesProtocolName
		toPackageNamed: self packageName
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2.
	selector := compiledMethod selector
]
