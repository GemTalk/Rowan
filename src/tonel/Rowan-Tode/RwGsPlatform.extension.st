Extension { #name : 'RwGsPlatform' }

{ #category : '*rowan-tode' }
RwGsPlatform >> packageTools [
	"Answer the platform-specific class for package tools"

	^RwPackageTool
]

{ #category : '*rowan-tode' }
RwGsPlatform >> projectTools [
	"Answer the platform-specific class for project tools"

	^RwProjectTool
]

{ #category : '*rowan-tode' }
RwGsPlatform >> repositoryTools [
	"Answer the platform-specific class for repository tools"

	^RwRepositoryTool
]
