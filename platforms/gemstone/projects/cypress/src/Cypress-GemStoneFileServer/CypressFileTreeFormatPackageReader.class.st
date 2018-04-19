"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressFileTreeFormatPackageReader',
	#superclass : 'CypressAbstractPackageReader',
	#category : 'Cypress-GemStoneFileServer'
}

{ #category : 'private' }
CypressFileTreeFormatPackageReader >> classStructureFrom: fileteeClassPropertiesDict comment: classComment [
  | classPropertiesDict subclassType filetreeSubclassType |
  classPropertiesDict := fileteeClassPropertiesDict copy.
  filetreeSubclassType := classPropertiesDict at: 'type'.
  filetreeSubclassType = 'normal'
    ifTrue: [ subclassType := '' ]
    ifFalse: [ 
      filetreeSubclassType = 'variable'
        ifTrue: [ subclassType := 'indexableSubclass' ]
        ifFalse: [ 
          filetreeSubclassType = 'bytes'
            ifTrue: [ subclassType := 'byteSubclass' ]
            ifFalse: [ self error: 'unknown subclass type: ' , filetreeSubclassType printString ] ] ].
  classPropertiesDict at: '_gs_subclassType' put: subclassType.
  ^ super classStructureFrom: classPropertiesDict comment: classComment

]

{ #category : 'accessing' }
CypressFileTreeFormatPackageReader >> propertiesFileNameExtension [

	^'.json'

]

{ #category : 'accessing' }
CypressFileTreeFormatPackageReader >> propertiesFileNameExtensions [

	^Array
		with: super propertiesFileNameExtension
		with: self propertiesFileNameExtension

]

{ #category : 'reading' }
CypressFileTreeFormatPackageReader >> readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods [
	"Strict!"

	self
		readFileTreeFormatMethodStructureFrom: fileStream
		intoClassStructure: classStructure
		meta: isMeta
		methods: methods

]
