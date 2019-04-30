"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressPackageReader',
	#superclass : 'CypressAbstractPackageReader',
	#category : 'Cypress-GemStoneFileServer'
}

{ #category : 'reading' }
CypressPackageReader >> readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods [
	"Strict!"

	self
		readCypressFormatMethodStructureFrom: fileStream
		intoClassStructure: classStructure
		meta: isMeta
		methods: methods

]
