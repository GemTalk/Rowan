Class {
	#name : 'RwPackageTool',
	#superclass : 'RwAbstractTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'commands' }
RwPackageTool class >> adopt [
  ^ RwPkgAdoptTool new
]

{ #category : 'commands' }
RwPackageTool class >> create [
  ^ RwPkgCreateTool new
]

{ #category : 'commands' }
RwPackageTool class >> diff [
  ^ RwPkgDiffTool new
]

{ #category : 'commands' }
RwPackageTool class >> disown [
  ^ RwPkgDisownTool new

]

{ #category : 'commands' }
RwPackageTool class >> load [
  ^ RwPkgLoadTool new
]
