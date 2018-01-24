Class {
	#name : 'RwPackageSymbolDictionary',
	#superclass : 'SymbolDictionary',
	#instVars : [
		'packageRegistry'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwPackageSymbolDictionary class >> newNamed: aSymbol [

	^ self new
		name: aSymbol;
		yourself
]

{ #category : 'private' }
RwPackageSymbolDictionary >> _initializeWithoutClear: newSize [

	super _initializeWithoutClear: newSize.
	packageRegistry := KeyValueDictionary new.
	^ self
]

{ #category : 'private' }
RwPackageSymbolDictionary >> _loadedClassFor: class do: aBlock [

	| loadedClass classKey |
	(class isKindOf: Class)
		ifFalse: [ self error: 'internal error - expected a class' ].
	self packageRegistry
		detect: [ :loadedPackage | 
			loadedClass := loadedPackage loadedClassForClass: class.
			loadedClass ~~ RwLoadedThing absentToken ]
		ifNone: [ 
			self
				error:
					'internal error - No package found for the class ' , class name printString ].
	classKey := loadedClass key asSymbol.
	self
		at: classKey
		ifPresent: [ :theClass | 
			theClass == class
				ifTrue: [ ^ aBlock value: loadedClass ]
				ifFalse: [ 
					self
						error:
							'internal error - the value associated with the key '
								, classKey asString printString , ' is not identical to the given class' ] ].

	self
		error:
			'internal error - there is no assocation present in the receiver for the given class '
				, classKey asString printString
]

{ #category : 'class' }
RwPackageSymbolDictionary >> addClassAssociation: assoc toPackageNamed: packageName [

	"a class association is being added to the named package in the receiver"

	| package class loadedClass |
	class := assoc value.
	(class isKindOf: Class)
		ifFalse: [ self error: 'internal error - expected a class association' ].
	package := self packageRegistry
		at: packageName
		ifAbsent: [ 
			self packageRegistry
				at: packageName
				put: (RwGSLoadedSymbolDictPackage newNamed: packageName) ].
	self
		at: assoc key
		ifPresent: [ 
			self
				error:
					'internal error - there is an existing assocition with the same key: '
						, assoc key printString ].
	package loadedClasses
		at: class name
		ifAbsent: [ 
			loadedClass := RwGsLoadedSymbolDictClass newForClass: class.
			package addLoadedClass: loadedClass.
			package loadedClassExtensions
				at: class name
				ifPresent: [ :loadedClassExtension | 
					"I think we need to subsume the loadedClassExtension methods into a loadedClass ..."
					self halt ] ].
	self add: assoc
]

{ #category : 'accessing' }
RwPackageSymbolDictionary >> at: key ifPresent: oneArgBlock ifAbsent: absentBlock [

	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the oneArgBlock with the value associated with the key, otherwise answer the value of absentBlock."

	self at: key ifPresent: [ :v | ^ oneArgBlock value: v ].
	^ absentBlock value
]

{ #category : 'class' }
RwPackageSymbolDictionary >> deleteClassFromPackage: class [

	"a class association is being deleted from the receiver remove it from the loaded things"

	self
		_loadedClassFor: class
		do: [ :loadedClass | 
			"association for class is present, finish delete operation"
			loadedClass removeFromLoaded.
			self removeKey: loadedClass key asSymbol ]
]

{ #category : 'private' }
RwPackageSymbolDictionary >> migrateFrom: anotherObject instVarMap: otherivi [

	"skip migration until the class definition settles down a bit"

	
]

{ #category : 'accessing' }
RwPackageSymbolDictionary >> packageRegistry [

   ^packageRegistry

]

{ #category : 'accessing' }
RwPackageSymbolDictionary >> packageRegistry: anObject [

   packageRegistry := anObject

]

{ #category : 'class' }
RwPackageSymbolDictionary >> updateClassProperties: class [

	"class properties have changed, update the clas properties of the loaded things"

	(class isKindOf: Class)
		ifFalse: [ self error: 'internal error - expected a class' ].
	self
		_loadedClassFor: class
		do: [ :loadedClass | 
			"association for class is present, update the loaded thing"
			loadedClass updatePropertiesFromClassFor: self ]
]
