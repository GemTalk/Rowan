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
RwPackageSymbolDictionary >> _loadedClassFor: class noNewVersion: noNewVersionBlock [

	self
		_loadedClassFor: class
		noNewVersion: noNewVersionBlock
		newVersion: [ :loadedClass | 
			self
				error:
					'internal error - the value associated with the key ' , class name printString
						, ' is not identical to the given class' ]
]

{ #category : 'private' }
RwPackageSymbolDictionary >> _loadedClassFor: class noNewVersion: noNewVersionBlock newVersion: newVersionBlock [

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
		associationAt: classKey
		ifPresent: [ :assoc | 
			assoc value == class
				ifTrue: [ ^ noNewVersionBlock cull: loadedClass cull: assoc ]
				ifFalse: [ ^ newVersionBlock cull: loadedClass cull: assoc ] ].

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

{ #category : 'class' }
RwPackageSymbolDictionary >> addNewClassVersionToAssociation: newClass [

	"a new class version is being added to the association in the receiver previously occupied by the original class"

	self
		_loadedClassFor: newClass
		noNewVersion: [ 
			"newClass is actually not a new class version of existing class, that's okay... update properties anyway"
			self updateClassProperties: newClass.
			^ self ]
		newVersion: [ :loadedClass :assoc | 
			"association for class is present, install in association and update the loadedthing"
			assoc value: newClass.
			loadedClass handleClassDeletionOrNewVersion
				ifTrue: [ loadedClass updatePropertiesFromClassFor: self ]
				ifFalse: [ self error: 'internal error - new version of class not properly installed' ] ]
]

{ #category : 'accessing' }
RwPackageSymbolDictionary >> associationAt: aKey ifPresent: oneArgBlock [

	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the oneArgBlock with the symbol association associated with the key, otherwise answer nil."

	| anAssoc |
	anAssoc := self associationAt: aKey otherwise: nil.
	anAssoc ifNotNil: [ ^ oneArgBlock value: anAssoc ].
	^ nil
]

{ #category : 'accessing' }
RwPackageSymbolDictionary >> associationAt: key ifPresent: oneArgBlock ifAbsent: absentBlock [

	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the oneArgBlock with the symbol association associated with the key, otherwise answer the value of absentBlock."

	self associationAt: key ifPresent: [ :v | ^ oneArgBlock value: v ].
	^ absentBlock value
]

{ #category : 'accessing' }
RwPackageSymbolDictionary >> at: key ifPresent: aBlock [

	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v := self at: key ifAbsent: [ ^ nil ].
	^ aBlock value: v
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
		noNewVersion: [ :loadedClass | 
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
		noNewVersion: [ :loadedClass | 
			"association for class is present, update the loaded thing"
			loadedClass updatePropertiesFromClassFor: self ]
]
