Class {
	#name : 'RwPrjDiffTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'smalltalk api' }
RwPrjDiffTool >> diffSpecification: aRwSpecification [

  | repo stream diffy patch |
  super specification: aRwSpecification.
  stream := TextStream on: Text new.
  repo := CypressAbstractRepository
    onUrl: (CypressUrl absoluteFromText: specification repositoryUrl)
    alias: ''.
  self packageNames
    do: [ :packageName | 
      | diskSnapshot imageSnapshot |
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
          stream cr ] ].
  ^ stream contents
]

{ #category : 'smalltalk api' }
RwPrjDiffTool >> diffSpecUrl: aString [

	^ self diffSpecification: (RwSpecification fromUrl: aString)
]

{ #category : 'man page' }
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

{ #category : 'validation' }
RwPrjDiffTool >> validate: aRwSpecification [
  super validate: aRwSpecification.
  self validateForRead: aRwSpecification
]
