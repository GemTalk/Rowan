Class {
	#name : 'RwSpecificationTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'smalltalk api' }
RwSpecificationTool >> export [

	^specification export
]

{ #category : 'smalltalk api' }
RwSpecificationTool >> exportToUrl: fileUrl [

	^specification exportToUrl: fileUrl

]

{ #category : 'man page' }
RwSpecificationTool >> manPage [
  ^ TDManPage
    commandName: 'spec'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan spec - Manipulate spec object
SYNOPSIS
  rowan spec [--name=<spec-name>] [--project=<project-url>] [--repository=<repository-url>] \
             [--comment=<comment-string>] [--repoPath=<repo-path>] [--specsPath=<specs-path>] \
             [<spec-url>]

  rowan spec [--env=<meth-env>] [--symbolDict=<symbolDict-name>] [--userId=<user-id>] [<spec-url>]

  rowan spec [--type=package] [--packages=<project-package-names>] [--loads=<package-names-to-load>] \
             [<spec-url>]

  rowan spec [--type=project] [--baseline=<project-name>] [--loads=<metacello-load-list>] \
             [<spec-url>]

  rowan spec [--gitBranch=<branch-name>|--gitSHA=<SHA>|--gitTag=<tag>|--gitTagPattern=<pattern> \
             [<spec-url>]

  rowan spec [--export[=<export-url>]] [<spec-url>]

DESCRIPTION

  If <spec-url> is specified then the fields specified on the command will be applied to an 
  existing spec.

  If <spec-url> is not specified, then a new specification based on the --type option is created.
  The --type, and --baseline or --name and --packages options must be specified. The new spec is 
  registered.

  If the --name option is used on an existing specification, spec is copied an registered under the
  new name. All options are applied to the copied spec.

  If only the <spec-url> is specified, the existing spec that matches the <spec-url> will be returned.

EXAMPLES
  rowan spec Rowan

  rowan spec --name=OldRowan Rowan

  rowan spec --type=package --name=Rowan \
        --packages=`#(''Rowan-Core'' ''Rowan-GemStone'' ''Rowan-Tests'')`\
        --project=https://github.com/dalehenrich/CypressReferenceImplementation \
        --gitBranch=rowan --repoPath=tonel --specsPath=specs

  rowan spec --project=https://github.com/dalehenrich/CypressReferenceImplementation\
        --export=file:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/specs/Rowan.ston \
        Rowan
'
]

{ #category : 'smalltalk api' }
RwSpecificationTool >> newPackageSpecNamed: aString [
  ^ RwPackageLoadSpecification new
    specName: aString;
    yourself
]

{ #category : 'smalltalk api' }
RwSpecificationTool >> newProjectSpecNamed: aString [
  ^ RwProjectLoadSpecification new
    specName: aString;
    yourself
]
