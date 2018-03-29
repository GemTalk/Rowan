Class {
	#name : 'RwPkgCommitTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools'
}

{ #category : 'cypress api' }
RwPkgCommitTool >> commitSpecification: aRwSpecification packageNames: packageNames message: messageString [

	"CYPRESS API: this messge unconditionally uses #Category to interpret package membership --- useful for saving Rowan code during development ONLY"

	"write packages and commit repository"

	self specification: aRwSpecification.
	self class write writeSpecification: specification packageNames: packageNames.
	specification canCommit
		ifFalse: [ 
			| msg |
			msg := 'repository for ' , specification specName printString
				,
					' does not support commit operations. Source written to repository and skipping commit'.
			self inform: msg.
			^ msg ].
	specification commitForTool: self message: messageString.
	^ specification updateLoadedCommitIdForTool: self
]

{ #category : 'cypress api' }
RwPkgCommitTool >> commitSpecUrl: aSpecUrlString packageNames: packageNames message: messageString [

	"CYPRESS API: this messge unconditionally uses #Category to interpret package membership --- useful for saving Rowan code during development ONLY"

	"write packages and commit repository"

	^ self
		commitSpecification: (RwSpecification fromUrl: aSpecUrlString)
		packageNames: packageNames
		message: messageString
]

{ #category : 'man page' }
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
