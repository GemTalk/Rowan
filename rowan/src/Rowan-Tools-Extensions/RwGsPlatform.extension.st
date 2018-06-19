Extension { #name : 'RwGsPlatform' }

{ #category : '*rowan-tools-extensions' }
RwGsPlatform >> gitTools [

	"Answer the platform-specific class for git tools"

	^ RwGitTool new
]

{ #category : '*rowan-tools-extensions' }
RwGsPlatform >> packageTools [
	"Answer the platform-specific class for package tools"

	^RwPackageTool
]

{ #category : '*rowan-tools-extensions' }
RwGsPlatform >> projectTools [
	"Answer the platform-specific class for project tools"

	^RwProjectTool
]

{ #category : '*rowan-tools-extensions' }
RwGsPlatform >> repositoryTools [
	"Answer the platform-specific class for repository tools"

	^RwRepositoryTool
]
