Class {
	#name : 'RwPrjSpecTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tode'
}

{ #category : 'smalltalk api' }
RwPrjSpecTool >> exportSpecification: aRwSpecification [

	self specification: aRwSpecification.
	^specification export
]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> exportSpecification: aRwSpecification toUrl: fileUrl [

	self specification: aRwSpecification.
	^specification exportToUrl: fileUrl

]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> exportSpecUrl: aString [

	^ self exportSpecification: (RwSpecification fromUrl: aString)
]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> exportSpecUrl: aString toUrl: exportUrl [

	^ self exportSpecification: (RwSpecification fromUrl: aString) toUrl: exportUrl
]

{ #category : 'man page' }
RwPrjSpecTool >> manPage [
  ^ TDManPage
    commandName: 'spec'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  prj spec - Manipulate spec object
SYNOPSIS
  prj spec [--name=<spec-name>] [--project=<project-url>] [--repository=<repository-url>] \
             [--comment=<comment-string>] [--repoPath=<repo-path>] [--specsPath=<specs-path>] \
             [<spec-url>]

  prj spec [--env=<meth-env>] [--symbolDict=<symbolDict-name>] [--userId=<user-id>] [<spec-url>]

  prj spec [--type=package] [--packages=<project-package-names>] [--loads=<package-names-to-load>] \
             [<spec-url>]

  prj spec [--type=project] [--baseline=<project-name>] [--loads=<metacello-load-list>] \
             [<spec-url>]

  prj spec [--gitBranch=<branch-name>|--gitSHA=<SHA>|--gitTag=<tag>|--gitTagPattern=<pattern> \
             [--gitRemote=<remote-name>] [<spec-url>]

  prj spec [--export[=<export-url>]] [<spec-url>]

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
  prj spec Rowan

  prj spec --name=OldRowan Rowan

  prj spec --type=package --name=Rowan \
        --packages=`#(''Rowan-Core'' ''Rowan-GemStone'' ''Rowan-Tests'')`\
        --project=https://github.com/dalehenrich/CypressReferenceImplementation \
        --gitBranch=rowan --repoPath=tonel --specsPath=specs

  prj spec --project=https://github.com/dalehenrich/CypressReferenceImplementation\
        --export=file:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/specs/Rowan.ston \
        Rowan
'
]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> newPackageSpecNamed: aString useGit: useGit [
  | spec |
  spec := RwPackageProjectSpecification new
    specName: aString;
    yourself.
  useGit
    ifTrue: [ ^ spec ].
  spec repoSpec: RwDiskRepositorySpecification new.
  ^ spec
]

{ #category : 'smalltalk api' }
RwPrjSpecTool >> newProjectSpecNamed: aString useGit: useGit [
  | spec |
  spec := RwMetacelloProjectSpecification new
    specName: aString;
    yourself.
  useGit
    ifTrue: [ ^ spec ].
  spec repoSpec: RwDiskRepositorySpecification new.
  ^ spec
]
