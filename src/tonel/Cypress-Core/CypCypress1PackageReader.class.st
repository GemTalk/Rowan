"
Responsible for reading a cypress-format disk repository and producing a package definition.
"
Class {
	#name : 'CypCypress1PackageReader',
	#superclass : 'CypPackageReader',
	#category : 'Cypress-Core'
}

{ #category : 'instance creation' }
CypCypress1PackageReader class >> withDisk: disk [

	^(self new)
		disk: disk;
		yourself
]

{ #category : 'private' }
CypCypress1PackageReader >> classDefinitionWithProperties: classPropertiesDict comment: commentString [

	| definition properties |
	definition := RwClassDefinition new.
	properties := classPropertiesDict copy.
	properties at: 'comment' put: commentString.
	definition properties: properties.
	^definition
]

{ #category : 'private' }
CypCypress1PackageReader >> isPropertiesFilePath: aPath [

	^aPath endsWith: '/properties.ston'
]

{ #category : 'private' }
CypCypress1PackageReader >> readClassCommentFromDirectoryEntries: entries [

	disk
		readStreamFor: (entries detect: [:entry | entry endsWith: '/README.md']
				ifNone: [^''])
		do: [:fileStream | ^fileStream contents]
]

{ #category : 'private' }
CypCypress1PackageReader >> readClassDefinitionFromPath: classDirectory [

	| classPropertiesDict classComment entries classDefinition |
	entries := disk directoryEntriesFrom: classDirectory.
	classPropertiesDict := self
				readClassPropertiesFromDirectoryEntries: entries.
	classComment := self readClassCommentFromDirectoryEntries: entries.
	classDefinition := self classDefinitionWithProperties: classPropertiesDict
				comment: classComment.
	self readMethodDefinitionsFor: classDefinition in: entries.
	^classDefinition
]

{ #category : 'private' }
CypCypress1PackageReader >> readClassPropertiesFromDirectoryEntries: entries [

	| properties |
	properties := disk
				readStreamFor: (entries detect: [:entry | self isPropertiesFilePath: entry]
						ifNone: [Dictionary new])
				do: [:fileStream | CypressJsonParser parseStream: fileStream].
	self sortVariablesOf: properties.
	^properties
]

{ #category : 'private' }
CypCypress1PackageReader >> readMethodDefinitionFrom: aStream [

	| properties source selector |
	properties := Dictionary new.
	(aStream peekFor: $")
		ifTrue: [aStream nextLine]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	(aStream match: 'notice: ')
		ifTrue: [properties at: 'notice' put: aStream nextLine trimSeparators]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	(aStream match: 'category: ')
		ifTrue: [properties at: 'protocol' put: aStream nextLine trimSeparators]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	(aStream peekFor: $")
		ifTrue: [aStream nextLine]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	source := aStream upToEnd.
	selector := Rowan platform parseSelectorFrom: source.
	properties at: 'selector' put: selector.
	^RwMethodDefinition withProperties: properties source: source
]

{ #category : 'private' }
CypCypress1PackageReader >> readMethodDefinitionsFor: classDefinition in: entries [

	entries do: 
			[:entry |
			| isMeta |
			(entry endsWith: '/instance') ifTrue: [isMeta := false].
			(entry endsWith: '/class') ifTrue: [isMeta := true].
			isMeta
				ifNotNil: 
					[((disk directoryEntriesFrom: entry) select: [:each | each endsWith: '.st'])
						do: 
							[:methodEntry |
							disk readStreamFor: methodEntry
								do: 
									[:fileStream |
									| methodDefinition |
									methodDefinition := self readMethodDefinitionFrom: fileStream.
									isMeta
										ifTrue: [classDefinition addClassMethodDefinition: methodDefinition]
										ifFalse: [classDefinition addInstanceMethodDefinition: methodDefinition]]]]]
]

{ #category : 'public' }
CypCypress1PackageReader >> readPackageDefinitionFromDirectory: fullPath [

	| packageDefinition |
	packageDefinition := RwPackageDefinition new.
	(disk directoryEntriesFrom: fullPath) do: 
			[:entry |
			(self isPropertiesFilePath: entry)
				ifTrue: 
					[packageDefinition properties: (self readPropertiesFileFromPath: entry)].
			(entry endsWith: '.class')
				ifTrue: 
					[packageDefinition
						addClassDefinition: (self readClassDefinitionFromPath: entry)].
			(entry endsWith: '.extension')
				ifTrue: 
					[packageDefinition
						addClassExtension: (self readClassExtensionDefinitionFromPath: entry)]].
	self setPackageNameIn: packageDefinition from: fullPath.
	^packageDefinition
]

{ #category : 'private' }
CypCypress1PackageReader >> readPropertiesFileFromPath: aPath [

	^disk readStreamFor: aPath
		do: [:stream | CypressJsonParser parseStream: stream]
]

{ #category : 'private' }
CypCypress1PackageReader >> setPackageNameIn: packageDefinition from: aPath [
	"The Cypress reference implementation incorrectly omits name from the package properties. 
	So if the name is not set, get it from the directory name."

	packageDefinition propertyAt: 'name'
		ifAbsent: 
			[| packageName dirName expectedExtension |
			dirName := disk localNameFrom: aPath.
			expectedExtension := '.package'.
			(dirName endsWith: expectedExtension)
				ifFalse: 
					[self
						error: 'Package directory does not have a valid name for Cypress1 format.'].
			packageName := dirName copyFrom: 1
						to: dirName size - expectedExtension size.
			packageDefinition propertyAt: 'name' put: packageName]
]

{ #category : 'private' }
CypCypress1PackageReader >> sortVariablesOf: properties [
	"Properties is a properties dictionary. Class variable names and pool variable names should be sorted, if present."

	#('classvars' 'pools') do: 
			[:propertyName |
			| values |
			values := properties at: propertyName ifAbsent: [nil].
			values
				ifNotNil: [properties at: propertyName put: values asSortedCollection]]
]
