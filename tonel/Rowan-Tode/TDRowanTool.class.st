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
    cloneSpecUrl: (subArguments at: 1)
    gitRootPath: gitRoot
    useSsh: useSsh
]

{ #category : 'commands' }
TDRowanTool >> rowancommit [
  "
rowan commit --message=<message-ref> <spec-url>
"

  | commitMessage specUrlString |
  self getSubcommandOptsMixedLongShort: {#('message' nil #'required')}.
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <spec-url>. See `man rowan commit` for details.' ].
  specUrlString := subArguments at: 1.
  subOptions
    at: 'message'
    ifPresent: [ :arg | commitMessage := arg ]
    ifAbsent: [ 
      commitMessage := (GsMultiLineTextInteraction
        prompt: 'Please enter a commit message for project: ' , specUrlString
        template: '') signal.
      commitMessage
        ifNil: [ ^ self inform: 'Commit aborted ... no commit message' ] ].
  ^ RwTool commit commitSpecUrl: specUrlString message: commitMessage
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
  diffText := RwTool diff diffSpecUrl: (subArguments at: 1).
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
  "
  rowan log [--limit=<max-commit-log-entries>] <spec-url>
"

  | logLimit log |
  self getSubcommandOptsMixedLongShort: {#('limit' nil #'required')}.
  subOptions
    at: 'limit'
    ifPresent: [ :limit | logLimit := limit asNumber ]
    ifAbsent: [ logLimit := 100 ].
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <spec-url>. See `man rowan log` for details.' ].
  log := RwTool log commitLogSpecUrl: (subArguments at: 1) limit: logLimit.
  log
    editUsing:
      ((TDEditorSpec topez: topez editorAspect: #'edit')
        windowName: #'gitStatus';
        yourself).
  ^ log
]

{ #category : 'commands' }
TDRowanTool >> rowanpull [
  "
  rowan pull [--remote=<git-remote-name>] <spec-url>
"

  | specUrlString |
  self getSubcommandOptsMixedLongShort: {#('remote' nil #'required')}.
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <spec-url>. See `man rowan pull` for details.' ].
  specUrlString := subArguments at: 1.
  ^ subOptions
    at: 'remote'
    ifPresent: [ :remote | RwTool pull pullSpecUrl: specUrlString remote: remote ]
    ifAbsent: [ RwTool pull pullSpecUrl: specUrlString ]
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
    ifPresent: [ :specName | RwTool register registerSpecUrl: (subArguments at: 1) as: specName ]
    ifAbsent: [ RwTool register registerSpecUrl: (subArguments at: 1) ]
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
  ^ RwTool save saveSpecUrl: (subArguments at: 1)
]

{ #category : 'commands' }
TDRowanTool >> rowanspec [
  "
  rowan spec [--name=<spec-name>] [--project=<project-url>] [--repository=<repository-url>] \
             [--comment=<comment-string>] [--repoPath=<repo-path>] [--specsPath=<specs-path>] \
             [<spec-url>]

  rowan spec [--env=<meth-env>] [--symbolDict=<symbolDict-name>] [--userId=<user-id>] [<spec-url>]

  rowan spec [--type=package] [--packages=<project-package-names>] [--loads=<package-names-to-load>] \
             [<spec-url>]

  rowan spec [--type=project] [--baseline=<project-name>] [--loads=<metacello-load-list>] \
             [<spec-url>]

  rowan spec [--gitBranch=<branch-name>|--gitSHA=<SHA>|--gitTag=<tag>|--gitTagPattern=<pattern> \
             [--gitRemote=<remote-name>] [<spec-url>]

  rowan spec [--export[=<export-url>]] [<spec-url>]
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
      #('env' nil #'required').
      #('packages' nil #'required').
      #('loads' nil #'required').
      #('baseline' nil #'required').
      #('gitBranch' nil #'required').
      #('gitSHA' nil #'required').
      #('gitTag' nil #'required').
      #('gitTagPattern' nil #'required').
      #('gitRemote' nil #'required').
      #('repoPath' nil #'required').
      #('specsPath' nil #'required').
      #('comment' nil #'required').
      #('export' nil #'optional')}.
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
    ifPresent: [ :arg | (specification platformSpec at: 'gemstone') symbolDictName: arg ].
  subOptions
    at: 'env'
    ifPresent: [ :arg | (specification platformSpec at: 'gemstone') methodEnv: arg asNumber ].
  subOptions
    at: 'userId'
    ifPresent: [ :arg | (specification platformSpec at: 'gemstone') userId: arg ].
  subOptions
    at: 'packages'
    ifPresent: [ :arg | specification packageNames: arg evaluate ].
  subOptions
    at: 'loads'
    ifPresent: [ :arg | specification packageNamesToLoad: arg evaluate ].
  subOptions at: 'baseline' ifPresent: [ :arg | specification projectName: arg ].
  subOptions
    at: 'gitBranch'
    ifPresent: [ :arg | specification repoSpec branch: arg ].
  subOptions at: 'gitSHA' ifPresent: [ :arg | specification repoSpec SHA: arg ].
  subOptions
    at: 'gitTag'
    ifPresent: [ :arg | specification repoSpec gitTag: arg ].
  subOptions
    at: 'gitTagPattern'
    ifPresent: [ :arg | specification repoSpec gitTagPattern: arg ].
  subOptions
    at: 'gitRemote'
    ifPresent: [ :arg | specification repoSpec gitRemote: arg ].
  subOptions at: 'repoPath' ifPresent: [ :arg | specification repoPath: arg ].
  subOptions at: 'specsPath' ifPresent: [ :arg | specification specsPath: arg ].
  subOptions at: 'comment' ifPresent: [ :arg | specification comment: arg ].
  subOptions
    at: 'export'
    ifPresent: [ :argOrNil | 
      | exportUrl |
      exportUrl := argOrNil.
      exportUrl
        ifNil: [ 
          specification repositoryUrl
            ifNil: [ 
              self
                error:
                  '<exportUrl> must be specified if repository has not been cloned. See `man rowan spec` for details.' ].
          specTool exportSpecification: specification ]
        ifNotNil: [ specTool exportSpecification: specification toUrl: exportUrl ] ].
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
  ^ RwTool unregister unregisterSpecUrl: (subArguments at: 1)
]
