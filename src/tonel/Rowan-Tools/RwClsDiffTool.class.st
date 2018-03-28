Class {
	#name : 'RwClsDiffTool',
	#superclass : 'RwClassTool',
	#category : 'Rowan-Tools'
}

{ #category : 'smalltalk api' }
RwClsDiffTool >> diffClass: aClass1 with: aClass2 [
  | stream diffy patch label classDef1 package1 classDef2 package2 |
  label := 'Compare ' , aClass1 name , ' to ' , aClass2 name.
  stream := TextStream on: Text new.
  classDef1 := (RwGsLoadedClass existingForClass: aClass1) asDefinition.
  package1 := RwPackageDefinition
    withProperties: (Dictionary with: 'name' -> aClass1 name)
    classDefinitions: (Dictionary new at: classDef1 key put: classDef1)
    classExtensions: Dictionary new.
  classDef2 := (RwGsLoadedClass existingForClass: aClass2) asDefinition.
  package2 := RwPackageDefinition
    withProperties: (Dictionary with: 'name' -> aClass2 name)
    classDefinitions: (Dictionary new at: classDef2 key put: classDef2)
    classExtensions: Dictionary new.
  patch := CypressPatch fromBase: package2 toTarget: package1.
  patch operations isEmpty
    ifTrue: [ 
      stream
        nextPutAll:
            ('==================== ' , label , ' NO CHANGES  ====================') asText
                allBold;
        cr ]
    ifFalse: [ 
      stream
        nextPutAll:
            ('==================== ' , label , aClass1 name , ' (-) --> ' , aClass2 name
                , ' (+)  ====================') asText allBold;
        cr.
      diffy := MCDiffyTextWriter on: stream.
      diffy writePatch: patch.
      stream cr ].
  ^ stream contents
]

{ #category : 'man page' }
RwClsDiffTool >> manPage [
  "TDRowanTool install"

  ^ TDManPage
    commandName: 'class'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  class diff - Compare two classes
SYNOPSIS
  clas diff class1 class2
DESCRIPTION
EXAMPLES
  clas diff RwGsPatchSet RwGsPatchSetBasic
'
]
