Class {
	#name : 'RwProjectDefinitionV2',
	#superclass : 'RwAbstractProjectDefinitionV2',
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'private' }
RwProjectDefinitionV2 >> _validate: platformConfigurationAttributes componentPackageNames: componentPackageNames [
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
RwProjectDefinitionV2 >> addRawPackageNamed: packageName [
	"not sure I like how this is used ... the component structure needs to be kept in sync with packages, so this is not quite the route to go, unless we ensure that the component has an entry for the package"

	"see similar comment in addPackages:forComponent: and _addComponent"

	^ self _addPackage: (RwPackageDefinition newNamed: packageName)
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container."

	^self projectName
]

{ #category : 'querying' }
RwProjectDefinitionV2 >> packageForClassNamed: className [
	"Answer nil if no class found"

	self packages
		do: [ :package | 
			(package classDefinitions includesKey: className)
				ifTrue: [ ^ package ] ].
	^ nil
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> projectName [
	^ self propertyAt: 'name' ifAbsent: [ nil ]
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> projectName: aString [
	self propertyAt: 'name' put: aString
]
