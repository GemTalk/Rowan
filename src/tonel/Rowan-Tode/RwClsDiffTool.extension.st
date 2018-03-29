Extension { #name : 'RwClsDiffTool' }

{ #category : '*rowan-tode' }
RwClsDiffTool >> diffClass: aClass1 with: aClass2 [

	| stream diffy patch label |
	label := 'Compare ' , aClass1 name , ' to ' , aClass2 name.
	stream := TextStream on: Text new.
	patch := self patchClass: aClass1 with: aClass2.
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

{ #category : '*rowan-tode' }
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
