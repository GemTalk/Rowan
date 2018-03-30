Extension { #name : 'RwPrjCloneTool' }

{ #category : '*rowan-tode' }
RwPrjCloneTool >> manPage [
  ^ TDManPage
    commandName: 'clone'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan clone - Clone project repository
SYNOPSIS
  rowan clone [--https] --gitRoot=<git-root-dir> <spec-url>

DESCRIPTION
  The project specifiedin the <spec-url> is cloned using an SSH url (git@github.com)
  into the directory specified by the <git-root-dir>. The spec url repository url
  is updated to point to the cloned project directory.

  If the --https option is used then the clone is performed using an HTTPS url
  (https://github.com).

EXAMPLES
  prj clone \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston
  prj clone \
    file:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston

  prj clone --gitRoot=$GS_HOME/shared/repos Rowan
'
]
