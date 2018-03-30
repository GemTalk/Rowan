Extension { #name : 'RwPrjDiffTool' }

{ #category : '*rowan-tode' }
RwPrjDiffTool >> diffForProjectName: projectName [

	| loadedProject stream diffy patch diffTool |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	super specification: loadedProject specification.
	stream := TextStream on: Text new.
	diffTool := Rowan packageTools diff.
	self packageNames
		do: [ :packageName | 
			patch := diffTool patchForPackageName: packageName.
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
					stream cr ] ].
	^ stream contents
]

{ #category : '*rowan-tode' }
RwPrjDiffTool >> diffSpecification: aRwSpecification [

	| repo stream diffy patch diffTool |
	super specification: aRwSpecification.
	stream := TextStream on: Text new.
	repo := CypressAbstractRepository
		onUrl: (CypressUrl absoluteFromText: specification repositoryUrl)
		alias: ''.
	diffTool := Rowan packageTools diff.
	self packageNames
		do: [ :packageName | 
			| diskSnapshot imageSnapshot |
			diskSnapshot := repo readPackageStructureForPackageNamed: packageName.
			imageSnapshot := (CypressPackageDefinition named: packageName) snapshot.
			patch := diffTool
				patchSpecification: aRwSpecification
				packageName: packageName.
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
					stream cr ] ].
	^ stream contents
]

{ #category : '*rowan-tode' }
RwPrjDiffTool >> manPage [
  ^ TDManPage
    commandName: 'diff'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  prj diff - Browse package level diffs for the given spec
SYNOPSIS
  prj diff <spec-url>
DESCRIPTION
EXAMPLES
  prj diff \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston
  prj diff \
    file:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston
  prj diff Rowan
'
]
