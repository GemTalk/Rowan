Class {
	#name : 'RwDiffTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwDiffTool >> manPage [
  ^ TDManPage
    commandName: 'diff'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan diff - Browse package level diffs for the given spec
SYNOPSIS
  rowan diff [-h|--help] <specUrl>
DESCRIPTION
EXAMPLES
  rowan diff --help
  rowan diff -h

  rowan diff \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston
  rowan diff \
    file:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston
  rowan diff rowanLoadSpec
'
]

{ #category : 'smalltalk api' }
RwDiffTool >> specification: aRwSpecification [
  "Note that specUrl: takes a string argument that calls this method, if you don't <aRwSpecification> handy"

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
