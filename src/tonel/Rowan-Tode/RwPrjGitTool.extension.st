Extension { #name : 'RwPrjGitTool' }

{ #category : '*rowan-tode' }
RwPrjGitTool >> tdGitTool [
  ^ tdGitTool ifNil: [ tdGitTool := TDGitTool new ]
]
