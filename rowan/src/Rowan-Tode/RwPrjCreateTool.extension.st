Extension { #name : 'RwPrjCreateTool' }

{ #category : '*rowan-tode' }
RwPrjCreateTool >> manPage [
  "TDRowanTool install"

  ^ TDManPage
    commandName: 'create'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rep create - Create a new repository and project
SYNOPSIS
  rep create --format=[disk|cypress|filetree|tonel] --type=[metacello|package] \
             [--git] --root=<path-to-root-directory> <project-name>
DESCRIPTION
  New repository is created in <path-to-root-directory>. If --git is specified
  a git repository is created.

EXAMPLES
  rep create --format=tonel --type=package --root=$GS_HOME/share/repos Foo
  rep create --git --format=tonel --type=metacello --root=$GS_HOME/share/repos Bar
'
]
