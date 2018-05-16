Extension { #name : 'RwPkgDiffTool' }

{ #category : '*rowan-tode' }
RwPkgDiffTool >> diffSpecification: aRwSpecification packageName: packageName [

	| repo stream diffy patch diskSnapshot imageSnapshot |
	super specification: aRwSpecification.
	stream := TextStream on: Text new.
	repo := CypressAbstractRepository
		onUrl: (CypressUrl absoluteFromText: specification repositoryUrl)
		alias: ''.

	diskSnapshot := repo readPackageStructureForPackageNamed: packageName.
	imageSnapshot := (CypressPackageDefinition named: packageName) snapshot.
	patch := CypressPatch fromBase: diskSnapshot toTarget: imageSnapshot.
	patch operations isEmpty
		ifTrue: [ 
			stream
				nextPutAll:
						('==================== ' , packageName , ' NO CHANGES  ====================')
								asText allBold;
				cr ]
		ifFalse: [ 
			stream
				nextPutAll:
						('==================== ' , packageName
								, ' (-) --> disk (+)  ====================') asText allBold;
				cr.
			diffy := MCDiffyTextWriter on: stream.
			diffy writePatch: patch.
			stream cr ].
	^ stream contents
]

{ #category : '*rowan-tode' }
RwPkgDiffTool >> diffSpecUrl: aString packageName: packageName [

	^ self
		diffSpecification: (RwSpecification fromUrl: aString)
		packageName: packageName
]

{ #category : '*rowan-tode' }
RwPkgDiffTool >> manPage [
  ^ TDManPage
    commandName: 'xxx'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  pkg xxx - xxx
SYNOPSIS
  pkg xxx
DESCRIPTION
EXAMPLES
  pkg xxx 
'
]
