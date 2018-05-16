Extension { #name : 'RwPrjCommitTool' }

{ #category : '*rowan-tode' }
RwPrjCommitTool >> manPage [
	^ TDManPage
		commandName: 'commit'
		sourceMethod: self class name asString , '>>manPage'
		fromString:
			'NAME
  prj commit - Write project to disk and commit repository (defs -> disk)
SYNOPSIS
  prj commit --message=<message-ref> [--push[=remote]] <spec-url>
DESCRIPTION
EXAMPLES
  prj commit --message=`implement RwCommitTool` Rowan
  prj commit Rowan

  prj commit --push=origin Rowan
  prj commit --push Rowan
'
]
