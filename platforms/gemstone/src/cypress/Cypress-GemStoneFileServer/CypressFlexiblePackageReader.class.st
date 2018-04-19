"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressFlexiblePackageReader',
	#superclass : 'CypressAbstractPackageReader',
	#category : 'Cypress-GemStoneFileServer'
}

{ #category : 'reading' }
CypressFlexiblePackageReader >> isPropertiesFileDirectoryEntry: entry [
	"Expect .ston properties file, but tolerate .json if present."

	^(super isPropertiesFileDirectoryEntry: entry)
		or: [self string: entry endsWith: '/properties.json']

]

{ #category : 'reading' }
CypressFlexiblePackageReader >> readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods [
	"If the stream begins with a double quote, process it on the assumption it is a Cypress-format method.
	 Otherwise, treat it as a FileTree-format method."

	fileStream peek = $"
		ifTrue: 
			[self
				readCypressFormatMethodStructureFrom: fileStream
				intoClassStructure: classStructure
				meta: isMeta
				methods: methods]
		ifFalse: 
			[self
				readFileTreeFormatMethodStructureFrom: fileStream
				intoClassStructure: classStructure
				meta: isMeta
				methods: methods]

]
