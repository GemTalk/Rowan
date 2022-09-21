"
Keeps track of what LoadedPackages are in the GemStone repository, and is the gateway for operations to query or change the image.
"
Class {
	#name : 'RwGsImage',
	#superclass : 'Object',
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'private' }
RwGsImage class >> _loadedProjectRegistry [

	^ self _loadedProjectRegistryForUserId: self currentUserId
]

{ #category : 'private' }
RwGsImage class >> _loadedProjectRegistryForUserId: aUserId [

	| userProfile userPlatformDict |
	userProfile := AllUsers userWithId: aUserId.
	(userProfile defaultObjectSecurityPolicy ifNil: [true] ifNotNil: [:x |x currentUserCanRead])
		ifFalse: [
			"do not have permissions to read objects created by <aUserId>"
			^ nil ].
	userPlatformDict := RwPlatform _userPlatformDictionaryForUser: aUserId.
	^ userPlatformDict
		at: #'RwGsLoadedProjectRegistry'
		ifAbsent: [ 
			(self currentUserId = aUserId)
				ifFalse: [ ^ nil ].
			userPlatformDict at: #'RwGsLoadedProjectRegistry' put: StringKeyValueDictionary new ]
]

{ #category : 'querying' }
RwGsImage class >> _removeLoadedProject: aRwLoadedProject [

	"note that it is not safe to just remove a loaded project from the registry, however this method is supplied as a convenience for tests"

	^ self 
		_removeLoadedProject: aRwLoadedProject 
		ifAbsent: [ self error: 'The specified project was not found in the loaded project registry.' ]
]

{ #category : 'querying' }
RwGsImage class >> _removeLoadedProject: aRwLoadedProject ifAbsent: absentBlock [

	"note that it is not safe to just remove a loaded project from the registry, however this method is supplied as a convenience for tests"

	| projectName projectRegistry |
	projectName := aRwLoadedProject name.
	projectRegistry := self
		_loadedProjectRegistryForUserId: aRwLoadedProject projectOwnerId.
	(projectRegistry 
		at: projectName 
		ifAbsent: [ ^ absentBlock value ]) == aRwLoadedProject
			ifFalse: [ 
				self
					error:
						'The specified project is not identical to the loaded project in the registry.' ].
	projectRegistry removeKey: projectName
]

{ #category : 'querying' }
RwGsImage class >> addLoadedProject: aRwLoadedProject [

	"Register a project"

	| projectName projectRegistry |
	aRwLoadedProject projectOwnerId: self currentUserId.
	projectName := aRwLoadedProject name.
	projectRegistry := self
		_loadedProjectRegistryForUserId: aRwLoadedProject projectOwnerId.
	projectRegistry
		at: projectName
		ifAbsent: [ ^ projectRegistry at: projectName put: aRwLoadedProject ].
	self
		error:
			'There is already a project named ' , projectName printString , ' registered'

]

{ #category : 'testing' }
RwGsImage class >> canWrite: anObject [

	^ System canWrite: anObject
]

{ #category : 'querying' }
RwGsImage class >> currentUserId [

	^ GsCurrentSession currentSession userProfile userId
]

{ #category : 'operations' }
RwGsImage class >> ensureSessionMethodsEnabled [

	"Ensure that session methods are enabled for the current user."

	GsPackagePolicy current enabled
		ifFalse: [ GsPackagePolicy current enable ].
]

{ #category : 'querying' }
RwGsImage class >> loadedClassExtensionsNamed: className ifFound: foundBlock ifAbsent: absentBlock [

	"scan the symbol list for a RwLoadedClassExtension instances of the given name"

	| class found |
	class := self objectNamed: className.
	class ifNil: [ ^ absentBlock value ].
	
	found := self loadedClassExtensionsForClass: class.
	found isEmpty
		ifFalse: [ ^ foundBlock value: found ].
	^ absentBlock value
]

{ #category : 'querying' }
RwGsImage class >> loadedClassNamed: className [

	^ self
		loadedClassNamed: className
		ifAbsent: [ self error: 'loaded class named ' , className printString , ' not found.' ]
]

{ #category : 'querying' }
RwGsImage class >> loadedClassNamed: className ifAbsent: absentBlock [

	^ self
		loadedClassNamed: className
		ifFound: [ :loadedClass | ^ loadedClass ]
		ifAbsent: absentBlock
]

{ #category : 'querying' }
RwGsImage class >> loadedHybridPackageNamed: aHybridName ifAbsent: absentBlock [

	"scan the symbol list a RwLoadedPackage instance of the given hybrid name"

	"a hybrid package name has a leading '*' followed by the name of a package ... 
		where the hybrid package name is not expected to preserve case"

	self symbolList
		do: [ :symbolDict | 
			symbolDict rowanSymbolDictionaryRegistry
				ifNotNil: [ :registry | 
					(registry loadedHybridPackageNamed: aHybridName ifAbsent: [  ])
						ifNotNil: [ :loadedPackage | ^ loadedPackage ] ] ].
	^ absentBlock value
]

{ #category : 'querying' }
RwGsImage class >> loadedMethodForMethod: compiledMethod [

	"scan the symbol list for a RwLoadedMethod instances for the given compiled method"

	^ self
		loadedMethodForMethod: compiledMethod
		ifAbsent: [ 
			self
				error:
					'No loadedMethod found for the compiled method ' , compiledMethod printString ]
]

{ #category : 'querying' }
RwGsImage class >> loadedPackageNamed: aString [

	^ self
		loadedPackageNamed: aString
		ifAbsent: [ self error: 'No loaded package named ' , aString printString , ' found' ]
]

{ #category : 'querying' }
RwGsImage class >> loadedPackageNamed: aName ifAbsent: absentBlock [

	"scan the symbol list a RwLoadedPackage instance of the given name"

	self symbolList
		do: [ :symbolDict | 
			symbolDict rowanSymbolDictionaryRegistry
				ifNotNil: [ :registry | 
					(registry loadedPackageNamed: aName ifAbsent: [  ])
						ifNotNil: [ :loadedPackage | ^ loadedPackage ] ] ].
	^ absentBlock value
]

{ #category : 'querying' }
RwGsImage class >> loadedProjectNamed: aString [

	^ self
		loadedProjectNamed: aString
		ifPresent: [:loadedProject | loadedProject ]
		ifAbsent: [ self error: 'No loaded project named ' , aString printString , ' found' ]
]

{ #category : 'querying' }
RwGsImage class >> loadedProjectNamed: aString ifAbsent: absentBlock [

	"Look up a loaded project in the loaded project registry"

	^ self
		loadedProjectNamed: aString
		ifPresent: [:loadedProject | loadedProject ]
		ifAbsent: absentBlock
]

{ #category : 'querying' }
RwGsImage class >> loadedProjectNamed: aString ifPresent: presentBlock ifAbsent: absentBlock [

	"Look up a loaded project in the loaded project registry"

	| loadedProject |
	loadedProject := self _loadedProjectRegistry
		at: aString
		ifAbsent: [
			| matchingProjects |
			matchingProjects := self loadedProjects select: [:each | each name = aString ].
			matchingProjects size > 1 ifTrue: [ self error: 'Multiple projects with same name available from symbol dictionaries in symbol list' ].
			matchingProjects size = 0 ifTrue: [ ^ absentBlock value ].
			matchingProjects any ].
	^ presentBlock cull: loadedProject
]

{ #category : 'querying' }
RwGsImage class >> loadedProjects [

	"Return the list of loaded projects based on the packages that are visible in the current transient symbol list."

	| loadedProjects |
	loadedProjects := (self _loadedProjectRegistryForUserId: self currentUserId) values asIdentitySet. "list of projects created by current user"
	(self symbolList select: [:symDict | 
		symDict rowanSymbolDictionaryRegistry notNil])
		do: [:symDict | 
			"visible loaded packages"
			symDict rowanSymbolDictionaryRegistry packageRegistry values 
				do: [:loadedPackage | 
					"visible loaded projects"
					loadedProjects add: loadedPackage loadedProject ] ].
	^loadedProjects

]

{ #category : 'querying' }
RwGsImage class >> loadedRegistryForPackageNamed: aName [
	"scan the symbol list for a RwLoadedPackage instance of the given name and return the registry"

	^ self
		loadedRegistryForPackageNamed: aName
		ifAbsent: [ self error: 'The package ' , aName printString , ' was not found' ]
]

{ #category : 'querying' }
RwGsImage class >> loadedRegistryForPackageNamed: aName ifAbsent: absentBlock [
	"scan the symbol list for a RwLoadedPackage instance of the given name and return the registry"

	self symbolList
		do: [ :symbolDict | 
			symbolDict rowanSymbolDictionaryRegistry
				ifNotNil: [ :registry | 
					(registry loadedPackageNamed: aName ifAbsent: [  ])
						ifNotNil: [ :loadedPackage | ^ registry ] ] ].
	^ absentBlock value
]

{ #category : 'querying' }
RwGsImage class >> newOrExistingSymbolDictionaryNamed: dictName [

	"If the current session's transient symbol list includes a dictionary with the given name, answer it.
	Otherwise, create one, add it to the beginning of both transient and persistent symbol lists, and 
	answer it."

	| symbolName  symbolList symbolDict |
	symbolName := dictName asSymbol.
	symbolList := self symbolList.
	symbolDict := symbolList
		detect: [ :each | (each at: symbolName ifAbsent: [ nil ]) == each ]
		ifNone: [ 
			| newDict size |
			newDict := SymbolDictionary new
				name: symbolName;
				objectSecurityPolicy: symbolList objectSecurityPolicy;
				yourself.
			size := System myUserProfile symbolList size.
			System myUserProfile insertDictionary: newDict at: size + 1.
			newDict ].
	symbolDict rowanSymbolDictionaryRegistry
		ifNil: [ self symbolDictionaryRegistryClass installIn: symbolDict ].
	^ symbolDict
]

{ #category : 'querying' }
RwGsImage class >> objectNamed: aSymbol [

	"Returns the first object in the current session's symbol list that has the given
 name.  If no object with the given name is found, returns nil."

	^ self symbolList objectNamed: aSymbol

]

{ #category : 'querying' }
RwGsImage class >> packageNames [

	"Return list of package names. All sytem projects are visible, but not all packages of visible projects
		are visible."

	| packageNames visiblePackageNames |
	packageNames := Set new.
	visiblePackageNames := Set new.
	(self symbolList select: [:symDict | 
		symDict rowanSymbolDictionaryRegistry notNil])
		do: [:symDict | 
			visiblePackageNames 
				addAll: symDict rowanSymbolDictionaryRegistry packageRegistry keys ].
	self loadedProjects
		do: [ :loadedProject | 
			loadedProject packageNames do: [:packageName |
				(visiblePackageNames includes: packageName) ifTrue: [ packageNames add: packageName ] ] ].
	^ packageNames asArray

]

{ #category : 'querying' }
RwGsImage class >> packageNamesForLoadedProjectNamed: projectName [

	"Return list of package names for the loaded project with the given name. All sytem projects are visible, but not all packages of visible projects
		are visible."

	| packageNames visiblePackageNames |
	packageNames := Set new.
	visiblePackageNames := Set new.
	(self symbolList select: [:symDict | 
		symDict rowanSymbolDictionaryRegistry notNil])
		do: [:symDict | 
			visiblePackageNames 
				addAll: symDict rowanSymbolDictionaryRegistry packageRegistry keys ].
	(self loadedProjectNamed: projectName) packageNames do: [:packageName |
				(visiblePackageNames includes: packageName) ifTrue: [ packageNames add: packageName ] ].
	^ packageNames asArray

]

{ #category : 'cypress (old)' }
RwGsImage class >> projectNames [

	"Return list of project names"

	^ (self loadedProjects collect: [ :loadedProject | loadedProject name ]) asArray

]

{ #category : 'querying' }
RwGsImage class >> resolveClassNamed: aName [

	"If the given name is bound to a class in the environment of the current session, 
	answer that class. Otherwise, answer nil."

	| resolved |
	resolved := self objectNamed: aName.
	^ (resolved isBehavior and: [ resolved isMeta not ])
		ifTrue: [ resolved ]
		ifFalse: [ nil ]
]

{ #category : 'querying' }
RwGsImage class >> resolveName: aName [

	"Searches the current session's symbol list for an Association whose key is equal to
 aString, and returns that Association.  If no such Association is found in the
 symbol list, returns nil. Uses the current session's transient copy of the symbol list."

	^ GsCurrentSession currentSession resolveSymbol: aName asSymbol
]

{ #category : 'querying' }
RwGsImage class >> symbolDictNamed: symbolDictName [
	"Search symbol list for a symbol dictionary with the given name"

	^ self
		symbolDictNamed: symbolDictName
		ifAbsent: [ 
			self
				error:
					'No symbol dictionary with the name ' , symbolDictName printString , ' found.' ]
]

{ #category : 'querying' }
RwGsImage class >> symbolDictNamed: symbolDictName ifAbsent: absentBlock [
	"Search symbol list for a symbol dictionary with the given name"

	| symbolDictNameSymbol |
	symbolDictNameSymbol := symbolDictName asSymbol.
	^ self symbolList
		detect: [ :each | (each at: symbolDictNameSymbol ifAbsent: [ nil ]) == each ]
		ifNone: absentBlock
]

{ #category : 'querying' }
RwGsImage class >> symbolList [

	"Answer the current session (transient) symbol list"

	^ GsCurrentSession currentSession symbolList
]

{ #category : 'validation' }
RwGsImage class >> validClassCategory: aClassCategory forClass: aClass [
	| loadedClass |
	loadedClass := self
		loadedClassForClass: aClass
		ifAbsent: [ 
			"no loaded class, no validation needed"
			^ true ].
	^ self
		validClassCategory: aClassCategory
		forLoadedPackage: loadedClass loadedPackage
]

{ #category : 'validation' }
RwGsImage class >> validClassCategory: aClassCategory forLoadedPackage: loadedPackage [
	^ self
		validClassCategory: aClassCategory
		forPackageConvention: loadedPackage loadedProject packageConvention
		andPackageNamed: loadedPackage name
]

{ #category : 'validation' }
RwGsImage class >> validClassCategory: aClassCategory forPackageConvention: packageConvention andPackageNamed: packageName [
	aClassCategory
		ifNil: [ 
			"setting category to nil is acceptable"
			^ true ].
	packageConvention = 'RowanHybrid'
		ifTrue: [ ^ aClassCategory asString = packageName ].
	packageConvention = 'Monticello'
		ifTrue: [ ^ packageName at: 1 equals: aClassCategory asString ].
	^ true	"Rowan - no convention ... any old category is fine"
]

{ #category : 'validation' }
RwGsImage class >> validClassCategory: aClassCategory forPackageNamed: packageName [
	| loadedPackage |
	loadedPackage := self
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'No loaded package named ' , packageName printString , ' found' ].
	^ self validClassCategory: aClassCategory forLoadedPackage: loadedPackage
]
