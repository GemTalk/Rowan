Class {
	#name : 'RwRepAddTool',
	#superclass : 'RwRepositoryTool',
	#category : 'Rowan-Tools'
}

{ #category : 'man page' }
RwRepAddTool >> manPage [
  "TDRowanTool install"

  ^ TDManPage
    commandName: 'add'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rep add - Add a repository and create a new project spec for the repository
SYNOPSIS
  rep add --repos=<path-to-repos-directory> <project-name>
DESCRIPTION
EXAMPLES
  rep add --root=$GS_HOME/share/repos/Foo Foo
'
]
