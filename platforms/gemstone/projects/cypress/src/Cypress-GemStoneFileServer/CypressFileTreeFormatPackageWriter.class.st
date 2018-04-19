"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressFileTreeFormatPackageWriter',
	#superclass : 'CypressAbstractPackageWriter',
	#category : 'Cypress-GemStoneFileServer'
}

{ #category : 'private' }
CypressFileTreeFormatPackageWriter >> adjustClassPropertiesForFileTree: classPropertyDict [
  | props classType |
  props := classPropertyDict copy.
  classType := (props at: '_gs_subclassType' ifAbsent: [  ])
    ifNil: [ 'normal' ]
    ifNotNil: [ :type | 
      props removeKey: '_gs_subclassType'.
      type = 'indexableSubclass'
        ifTrue: [ 'variable' ]
        ifFalse: [ 
          type = 'byteSubclass'
            ifTrue: [ 'bytes' ]
            ifFalse: [ self error: 'unknown subclass type: ' , type ] ] ].
  props at: 'type' put: classType.
  ^ props

]

{ #category : 'accessing' }
CypressFileTreeFormatPackageWriter >> propertiesFileNameExtension [
  ^ '.json'

]

{ #category : 'writing' }
CypressFileTreeFormatPackageWriter >> removeOldReplacingWithNew [

	self fileUtils deleteAll: self packageDirectory
		rejecting: 
			[:filename |
			"do not delete the monticello.meta directory to preserve existing Monticello meta data.
       Equivalent behavior to MCFileTreeRepository with Metadata property set to false."
			(self string: filename endsWith: 'monticello.meta')
				or: [ (self string: filename endsWith: '.filetree')
					or: [self string: filename endsWith: 'methodProperties.json']]].
	self writePropertiesFile.
	self writePackageStructure

]

{ #category : 'writing' }
CypressFileTreeFormatPackageWriter >> writeClassStructure: classStructure to: classPath [

	self
		writeInDirectoryName: classPath
			fileName: 'README'
			extension: '.md'
			visit: [:fileStream | self writeClassComment: classStructure on: fileStream];
		writeInDirectoryName: classPath
			fileName: 'properties'
			extension: self propertiesFileNameExtension
			visit: 
				[:fileStream |
				(self adjustClassPropertiesForFileTree: classStructure properties)
					_writeCypressJsonOn: fileStream]

]

{ #category : 'writing' }
CypressFileTreeFormatPackageWriter >> writeExtensionClassStructure: classStructure to: classPath [

	self
		writeInDirectoryName: classPath
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: 
			[:fileStream |
			(Dictionary with: 'name' -> classStructure className)
				_writeCypressJsonOn: fileStream]

]

{ #category : 'writing' }
CypressFileTreeFormatPackageWriter >> writeMethodStructure: methodStructure onStream: fileStream [

	fileStream
		nextPutAll: methodStructure category;
		lf;
		nextPutAll: (CypressObject normalizeLineEndingsOf: methodStructure source)

]

{ #category : 'writing' }
CypressFileTreeFormatPackageWriter >> writePropertiesFile [

	self
		writeInDirectoryName: '.'
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: [:fileStream | Dictionary new _writeCypressJsonOn: fileStream]

]
