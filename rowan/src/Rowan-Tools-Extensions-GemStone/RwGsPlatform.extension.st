Extension { #name : 'RwGsPlatform' }

{ #category : '*rowan-tools-extensions-gemstone' }
RwGsPlatform >> classTools [

	"Answer the platform-specific class for class tools"

	^ RwClassTool
]

{ #category : '*rowan-tools-extensions-gemstone' }
RwGsPlatform >> gemstoneTools [
	"Answer the platform-specific class for project tools"

	^RwGemStoneTool
]

{ #category : '*rowan-tools-extensions-gemstone' }
RwGsPlatform >> gitTools [

	"Answer the platform-specific class for git tools"

	^ RwGitTool new
]

{ #category : '*rowan-tools-extensions-gemstone' }
RwGsPlatform >> packageTools [
	"Answer the platform-specific class for package tools"

	^RwPackageTool
]

{ #category : '*rowan-tools-extensions-gemstone' }
RwGsPlatform >> projectTools [
	"Answer the platform-specific class for project tools"

	^RwProjectTool
]
