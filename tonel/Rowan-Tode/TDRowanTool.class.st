Class {
	#name : 'TDRowanTool',
	#superclass : 'TDStandardTool',
	#category : 'Rowan-Tode'
}

{ #category : 'initialization' }
TDRowanTool class >> initialize [
  self install
]

{ #category : 'initialization' }
TDRowanTool class >> installExampleOn: aTDTopezServer [
  "nothing for now"
]

{ #category : 'initialization' }
TDRowanTool class >> priority [
    <topezCommandPriority>
    ^ 50
]

{ #category : 'rowan' }
TDRowanTool class >> rowan [
  "self initialize"

  <topezCommand: 'rowan' manPage: #'rowanManPage'>
  ^ [ :topez :objIn :tokens :command | 
  self
    performSubCommand: topez
    objIn: objIn
    commandOptions: {#('help' $h #'none')}
    todeCommand: command ] 
]

{ #category : 'rowan' }
TDRowanTool class >> rowanManPage [

	^RwTool new manPage
]

{ #category : 'initialization' }
TDRowanTool class >> toolName [
  ^ 'rowan'
]

{ #category : 'initialization' }
TDRowanTool class >> toolReadMe [
  ^ self commandPath , ' provides Rowan project/package management support.'
]

{ #category : 'commands' }
TDRowanTool >> rowanclone [
  "
  rowan clone [--https] --gitRoot=<git-root-dir> <spec-url>
"

  | useSsh gitRoot |
  useSsh := true.
  self
    getSubcommandOptsMixedLongShort:
      {#('https' nil #'none').
      #('gitRoot' nil #'required')}.
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <specUrl>. See `man rowan clone` for details.' ].
  subOptions at: 'gitRoot' ifPresent: [ :ignored | useSsh := false ].
  subOptions
    at: 'gitRoot'
    ifPresent: [ :arg | gitRoot := arg ]
    ifAbsent: [ 
      self
        error:
          'Missing required options <--gitRoot. See `man rowan clone` for details.' ].
  ^ RwTool clone
    specUrl: (subArguments at: 1)
    gitRootPath: gitRoot
    useSsh: useSsh
]

{ #category : 'commands' }
TDRowanTool >> rowancommit [
]

{ #category : 'commands' }
TDRowanTool >> rowandiff [
  "
  rowan diff <specUrl>
"

  | diffText |
  self getSubcommandOptsMixedLongShort: {}.
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <specUrl>. See `man rowan diff` for details.' ].
  diffText := RwTool diff specUrl: (subArguments at: 1).
  diffText
    editUsing:
      ((TDEditorSpec topez: topez editorAspect: #'edit')
        windowName: #'mcDiff';
        yourself)
]

{ #category : 'commands' }
TDRowanTool >> rowanlist [
]

{ #category : 'commands' }
TDRowanTool >> rowanload [
]

{ #category : 'commands' }
TDRowanTool >> rowanlog [
]

{ #category : 'commands' }
TDRowanTool >> rowanpull [
]

{ #category : 'commands' }
TDRowanTool >> rowanpush [
]

{ #category : 'commands' }
TDRowanTool >> rowanregister [
  "
  rowan register [--name=<spec-name>] <specUrl>
"

  self getSubcommandOptsMixedLongShort: {#('name' nil #'required')}.
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <specUrl>. See `man rowan register` for details.' ].
  ^ subOptions
    at: 'name'
    ifPresent: [ :specName | RwTool register specUrl: (subArguments at: 1) as: specName ]
    ifAbsent: [ RwTool register specUrl: (subArguments at: 1) ]
]

{ #category : 'commands' }
TDRowanTool >> rowanregistry [
  "
  rowan registry [--names|--list]
"

  self
    getSubcommandOptsMixedLongShort:
      {#('names' nil #'none').
      #('list' nil #'none')}.
  ^ subOptions
    at: 'names'
    ifPresent: [ :ignored | RwTool registry registeredSpecNames ]
    ifAbsent: [ 
      subOptions
        at: 'list'
        ifPresent: [ :ignored | RwTool registry registeredSpecs ]
        ifAbsent: [ RwTool registry registry ] ]
]

{ #category : 'commands' }
TDRowanTool >> rowanrevert [
]

{ #category : 'commands' }
TDRowanTool >> rowansave [
  "
  rowan save <specUrl>
"

  | diffText |
  self getSubcommandOptsMixedLongShort: {}.
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <specUrl>. See `man rowan save` for details.' ].
  ^ RwTool save specUrl: (subArguments at: 1)
]

{ #category : 'commands' }
TDRowanTool >> rowanspec [
  "
  rowan spec [--name=<spec-name>] [--project=<project-url>] [--repository=<repository-url>] \
             [--env=<meth-env>] [--symbolDict=<symbolDict-name>] [--userId=<user-id>] [<spec-url>]

  rowan spec [--type=package] [--packages=<project-package-names>] [--loads=<package-names-to-load>] \
             [<spec-url>]

  rowan spec [--type=project] [--baseline=<project-name>] [--loads=<metacello-load-list>] \
             [<spec-url>]

  rowan spec [--gitCheckout=<git-commitish>] [--gitRemote=<git-remote-name>] \
             [--comment=<comment-string>] [--repoPath=<repo-path>] [<spec-url>]

  rowan spec [--export=<export-url>] [<spec-url>]
"

  | shouldRegister specification specTool specName |
  specTool := RwTool spec.
  shouldRegister := false.
  self
    getSubcommandOptsMixedLongShort:
      {#('name' nil #'required').
      #('type' nil #'required').
      #('project' nil #'required').
      #('repository' nil #'required').
      #('symbolDict' nil #'required').
      #('userId' nil #'required').
      #('packages' nil #'required').
      #('loads' nil #'required').
      #('baseline' nil #'required').
      #('gitCheckout' nil #'required').
      #('gitRemote' nil #'required').
      #('repoPath' nil #'required').
      #('comment' nil #'required').
      #('export' nil #'required')}.
  subOptions at: 'name' ifPresent: [ :arg | specName := arg ].
  subArguments size = 0
    ifTrue: [ 
      "Creating a new spec object: --type required -- remaining required options a fuction of --type"
      subOptions
        at: 'type'
        ifPresent: [ :arg | 
          specName
            ifNil: [ 
              self
                error:
                  'Missing required option --name when no <spec-url> specified. See `man rowan spec` for details.' ].
          arg = 'project'
            ifTrue: [ specification := specTool newProjectSpecNamed: specName ]
            ifFalse: [ 
              arg = 'package'
                ifTrue: [ specification := specTool newPackageSpecNamed: specName ]
                ifFalse: [ 
                  self
                    error:
                      'Unrecognized --type ' , arg printString
                        ,
                          '. Should be ''project'' or ''package''. See `man rowan spec` for details.' ] ].
          shouldRegister := true ]
        ifAbsent: [ 
          self
            error:
              'Missing required option --type when no <spec-url> specified. See `man rowan spec` for details.' ] ]
    ifFalse: [ 
      "Updating existing spec. If --name is used, copy existing spec."
      specification := specTool specUrl: (subArguments at: 1).
      subOptions
        at: 'name'
        ifPresent: [ :arg | 
          specification specName = arg
            ifFalse: [ 
              specification := specification copy.
              shouldRegister := true.
              specification specName: specName ] ] ].
  shouldRegister
    ifTrue: [ specification register ].
  subOptions at: 'project' ifPresent: [ :arg | specification projectUrl: arg ].
  subOptions
    at: 'repository'
    ifPresent: [ :arg | specification repositoryUrl: arg ].
  subOptions
    at: 'symbolDict'
    ifPresent: [ :arg | specification symbolDictName: arg ].
  subOptions at: 'userId' ifPresent: [ :arg | specification userId: arg ].
  subOptions at: 'packages' ifPresent: [ :arg | specification ].
  subOptions at: 'loads' ifPresent: [ :arg | specification ].
  subOptions at: 'baseline' ifPresent: [ :arg | specification projectName: arg ].
  subOptions
    at: 'gitCheckout'
    ifPresent: [ :arg | specification gitCheckout: arg ].
  subOptions at: 'gitRemote' ifPresent: [ :arg | specification gitRemote: arg ].
  subOptions at: 'repoPath' ifPresent: [ :arg | specification repoPath: arg ].
  subOptions at: 'comment' ifPresent: [ :arg | specification comment: arg ].
  subOptions at: 'export' ifPresent: [ :arg | specification exportToUrl: arg ].
  ^ specification
]

{ #category : 'commands' }
TDRowanTool >> rowantest [
]

{ #category : 'commands' }
TDRowanTool >> rowanunregister [
  "
  rowan unregister <spec-url>
"

  self getSubcommandOptsMixedLongShort: {}.
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <specUrl>. See `man rowan unregister` for details.' ].
  ^ RwTool unregister specUrl: (subArguments at: 1)
]
