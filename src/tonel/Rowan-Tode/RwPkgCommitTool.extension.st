Extension { #name : 'RwPkgCommitTool' }

{ #category : '*rowan-tode' }
RwPkgCommitTool >> manPage [

	"TDRowanTool install"

	^ TDManPage
		commandName: 'commit'
		sourceMethod: self class name asString , '>>manPage'
		fromString:
			'NAME
  pkg commit - Write package(s) to disk and commit repository (defs -> disk)
SYNOPSIS
  pkg commit --message=<message-ref> [--push[=remote]] <package-name> ... <spec-url>
DESCRIPTION
EXAMPLES
  pkg commit --message=`implement RwCommitTool` Rowan-Core Rowan
  pkg commit --message=`implement RwCommitTool` Rowan-Core Rowan-Tests Rowan
  pkg commit Rowan-Core Rowan

  pkg commit --push=origin Rowan-Core Rowan
  pkg commit --push Rowan-Core Rowan
'
]
