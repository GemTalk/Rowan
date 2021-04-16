Class {
	#name : 'RwProjectDefinition',
	#superclass : 'RwDefinition',
	#instVars : [
		'packages'
	],
	#category : 'Rowan-Definitions'
}

{ #category : 'private' }
RwProjectDefinition >> _addPackage: aPackageDefinition [
	^ self
		_addPackage: aPackageDefinition
		ifPresent: [ self error: 'Duplicate package' ]
]

{ #category : 'private' }
RwProjectDefinition >> _addPackage: aPackageDefinition ifPresent: presentBlock [
	| key |
	key := aPackageDefinition key.
	(packages includesKey: key)
		ifTrue: [ ^ presentBlock value ].
	^ packages at: key put: aPackageDefinition
]

{ #category : 'private' }
RwProjectDefinition >> _projectDefinition [
	^ self
]

{ #category : 'comparing' }
RwProjectDefinition >> _projectDefinitionForCompare [
	^ self _projectDefinition
]

{ #category : 'properties' }
RwProjectDefinition >> _projectDefinitionPlatformConditionalAttributes [
	^ properties
		at: RwLoadedProject _projectDefinitionPlatformConditionalAttributesKey
		ifAbsent: [  ]
]

{ #category : 'properties' }
RwProjectDefinition >> _projectDefinitionPlatformConditionalAttributes: platformConditionalAtttributesOrNil [
	platformConditionalAtttributesOrNil
		ifNil: [ 
			^ properties
				removeKey:
					RwLoadedProject _projectDefinitionPlatformConditionalAttributesKey
				ifAbsent: [  ] ].
	^ properties
		at: RwLoadedProject _projectDefinitionPlatformConditionalAttributesKey
		put: platformConditionalAtttributesOrNil
]

{ #category : 'private' }
RwProjectDefinition >> _validate: platformConfigurationAttributes componentPackageNames: componentPackageNames [
	"ensure that the data structures within the receiver contain valid information"

	"make sure that list of packages is consistent between components and project definition
		It's okay to have a definition that is not managed by a component.
		It's NOT okay to have component package that is not defined."

	| definitionPackageNames missingFromComponent errorMessage |
	definitionPackageNames := self packageNames asSet.
	missingFromComponent := componentPackageNames - definitionPackageNames.
	missingFromComponent isEmpty
		ifTrue: [ ^ true ].
	errorMessage := WriteStream on: String new.
	errorMessage
		nextPutAll: 'Component references package(s) that are not defined';
		lf.
	errorMessage
		tab;
		nextPutAll:
				'The following packages are defined, but not referenced in a component:';
		lf.
	missingFromComponent
		do: [ :packageName | 
			errorMessage
				tab;
				tab;
				nextPutAll: packageName;
				lf ].
	self error: errorMessage contents
]

{ #category : 'accessing' }
RwProjectDefinition >> addRawPackageNamed: packageName [
	"not sure I like how this is used ... the component structure needs to be kept in sync with packages, so this is not quite the route to go, unless we ensure that the component has an entry for the package"

	"see similar comment in addPackages:forComponent: and _addComponent"

	^ self _addPackage: (RwPackageDefinition newNamed: packageName)
]

{ #category : 'initialization' }
RwProjectDefinition >> initialize [
	super initialize.
	packages := Dictionary new
]

{ #category : 'testing' }
RwProjectDefinition >> isEmpty [
	"Answer true if this definition does not actually define anything."

	^ packages isEmpty and:[ super isEmpty]
]

{ #category : 'properties' }
RwProjectDefinition >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container."

	^self projectName
]

{ #category : 'querying' }
RwProjectDefinition >> packageForClassNamed: className [
	"Answer nil if no class found"

	self packages
		do: [ :package | 
			(package classDefinitions includesKey: className)
				ifTrue: [ ^ package ] ].
	^ nil
]

{ #category : 'accessing' }
RwProjectDefinition >> packageNamed: aString [

	^ self
		packageNamed: aString
		ifAbsent: [ self error: 'A package named ' , aString printString , ' was not found.' ]
]

{ #category : 'accessing' }
RwProjectDefinition >> packageNamed: aString ifAbsent: absentBlock [

	^ self packages at: aString ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwProjectDefinition >> packageNames [
  ^ self packages keys asArray
]

{ #category : 'accessing' }
RwProjectDefinition >> packages [

	^packages
]

{ #category : 'accessing' }
RwProjectDefinition >> packages: aPackageDefinitionDictionary [

	packages := aPackageDefinitionDictionary
]

{ #category : 'comparing' }
RwProjectDefinition >> packagesForCompare [
	^ self packages
]

{ #category : 'copying' }
RwProjectDefinition >> postCopy [

	| oldPackages |
	super postCopy.
	oldPackages := packages.
	packages := Dictionary new.
	oldPackages keysAndValuesDo: [:key : value | packages at: key put: value copy ] .
]

{ #category : 'properties' }
RwProjectDefinition >> projectDefinitionPlatformConditionalAttributes [
	^ properties
		at: RwLoadedProject _projectDefinitionPlatformConditionalAttributesKey
		ifAbsent: [  ]
]

{ #category : 'accessing' }
RwProjectDefinition >> projectName [
	^ self propertyAt: #'name' ifAbsent: [ nil ]
]

{ #category : 'accessing' }
RwProjectDefinition >> projectName: aString [
	self propertyAt: #'name' put: aString
]

{ #category : 'accessing' }
RwProjectDefinition >> removePackage: aPackageDefinition [
	| key |
	key := aPackageDefinition key.
	(packages includesKey: key)
		ifFalse: [ self error: 'Package not present.' ].
	^ packages removeKey: key
]

{ #category : 'accessing' }
RwProjectDefinition >> removePackageNamed: packageName [
	^ self removePackage: (self packageNamed: packageName)
]
