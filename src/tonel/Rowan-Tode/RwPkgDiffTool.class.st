Class {
	#name : 'RwPkgDiffTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tode'
}

{ #category : 'smalltalk api' }
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

{ #category : 'smalltalk api' }
RwPkgDiffTool >> diffSpecUrl: aString packageName: packageName [

	^ self
		diffSpecification: (RwSpecification fromUrl: aString)
		packageName: packageName
]

{ #category : 'man page' }
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

{ #category : 'smalltalk api' }
RwPkgDiffTool >> patchSpecification: aRwSpecification packageName: packageName [

	| repo patch diskSnapshot imageSnapshot |
	super specification: aRwSpecification.
	repo := CypressAbstractRepository
		onUrl: (CypressUrl absoluteFromText: specification repositoryUrl)
		alias: ''.

	diskSnapshot := repo readPackageStructureForPackageNamed: packageName.
	imageSnapshot := (CypressPackageDefinition named: packageName) snapshot.
	^ CypressPatch fromBase: diskSnapshot toTarget: imageSnapshot
]

{ #category : 'smalltalk api' }
RwPkgDiffTool >> patchSpecUrl: aString packageName: packageName [

	^ self
		patchSpecification: (RwSpecification fromUrl: aString)
		packageName: packageName
]
