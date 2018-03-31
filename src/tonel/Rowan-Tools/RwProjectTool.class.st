Class {
	#name : 'RwProjectTool',
	#superclass : 'RwAbstractTool',
	#category : 'Rowan-Tools'
}

{ #category : 'commands' }
RwProjectTool class >> browser [

	^ RwPrjBrowserTool new
]

{ #category : 'commands' }
RwProjectTool class >> checkout [

	^RwPrjCheckoutTool new
]

{ #category : 'commands' }
RwProjectTool class >> clone [

	^RwPrjCloneTool new
]

{ #category : 'commands' }
RwProjectTool class >> commit [

	^RwPrjCommitTool new
]

{ #category : 'commands' }
RwProjectTool class >> create [
  ^ RwPrjCreateTool new
]

{ #category : 'commands' }
RwProjectTool class >> diff [

	^RwPrjDiffTool new
]

{ #category : 'commands' }
RwProjectTool class >> edit [

	^ RwPrjEditTool new
]

{ #category : 'commands' }
RwProjectTool class >> install [
	^ RwPrjInstallTool new
]

{ #category : 'commands' }
RwProjectTool class >> list [

	^RwPrjListTool new
]

{ #category : 'commands' }
RwProjectTool class >> load [

	^RwPrjLoadTool new
]

{ #category : 'commands' }
RwProjectTool class >> log [

	^RwPrjLogTool new
]

{ #category : 'commands' }
RwProjectTool class >> pull [

	^RwPrjPullTool new
]

{ #category : 'commands' }
RwProjectTool class >> push [

	^RwPrjPushTool new
]

{ #category : 'commands' }
RwProjectTool class >> read [
  ^ RwPrjReadTool new
]

{ #category : 'commands' }
RwProjectTool class >> reflect [

	^ RwPrjReflectionTool new
]

{ #category : 'commands' }
RwProjectTool class >> register [
  ^ RwPrjRegisterTool new
]

{ #category : 'commands' }
RwProjectTool class >> registry [

	^RwPrjRegistryTool new
]

{ #category : 'commands' }
RwProjectTool class >> revert [

	^RwPrjRevertTool new
]

{ #category : 'commands' }
RwProjectTool class >> spec [
  ^ RwPrjSpecTool new
]

{ #category : 'commands' }
RwProjectTool class >> test [

	^RwPrjTestTool new
]

{ #category : 'commands' }
RwProjectTool class >> unregister [

	^RwPrjUnregisterTool new
]

{ #category : 'commands' }
RwProjectTool class >> write [

	^RwPrjWriteTool new
]

{ #category : 'smalltalk api' }
RwProjectTool >> packageNames [
  ^ specification packageNamesForTool: self
]

{ #category : 'operations for package specs' }
RwProjectTool >> packageNamesForPackageSpec: aPackageSpec [

	^ aPackageSpec packageNamesInLoadOrder
]

{ #category : 'operations for project specs' }
RwProjectTool >> packageNamesForProjectSpec: aProjectSpec [
  self halt: 'not yet implemented'
]

{ #category : 'smalltalk api' }
RwProjectTool >> repositoryUrl [
  ^ self specification repositoryUrl
]
