Extension { #name : 'RwGsPlatform' }

{ #category : '*rowan-tools-core' }
RwGsPlatform >> gitTools [

	"Answer the platform-specific class for git tools"

	^ RwGitTool new
]

{ #category : '*rowan-tools-core' }
RwGsPlatform >> packageTools [
	"Answer the platform-specific class for package tools"

	^RwPackageTool
]

{ #category : '*rowan-tools-core' }
RwGsPlatform >> projectTools [
	"Answer the platform-specific class for project tools"

	^RwProjectTool
]

{ #category : '*rowan-tools-core' }
RwGsPlatform >> repositoryTools [
	"Answer the platform-specific class for repository tools"

	^RwRepositoryTool
]
