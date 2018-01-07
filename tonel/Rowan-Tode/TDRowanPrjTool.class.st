Class {
	#name : 'TDRowanPrjTool',
	#superclass : 'TDStandardTool',
	#category : 'Rowan-Tode'
}

{ #category : 'initialization' }
TDRowanPrjTool class >> initialize [
  self install
]

{ #category : 'initialization' }
TDRowanPrjTool class >> installExampleOn: aTDTopezServer [
  "nothing for now"
]

{ #category : 'initialization' }
TDRowanPrjTool class >> priority [
    <topezCommandPriority>
    ^ 50
]

{ #category : 'rowan project' }
TDRowanPrjTool class >> prj [
  "self initialize"

  <topezCommand: 'prj' manPage: #'prjManPage'>
  ^ [ :topez :objIn :tokens :command | 
  self
    performSubCommand: topez
    objIn: objIn
    commandOptions: {#('help' $h #'none')}
    todeCommand: command ] 
]

{ #category : 'rowan project' }
TDRowanPrjTool class >> prjManPage [

	^Rowan projectTools new manPage
]

{ #category : 'initialization' }
TDRowanPrjTool class >> toolName [
  ^ 'prj'
]

{ #category : 'initialization' }
TDRowanPrjTool class >> toolReadMe [
  ^ self commandPath , ' provides Rowan project/package management support.'
]

{ #category : 'commands' }
TDRowanPrjTool >> prjclone [
  "
  prj clone [--https] --gitRoot=<git-root-dir> <spec-url>
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
          'Missing required argument <spec-url>. See `man prj clone` for details.' ].
  subOptions at: 'gitRoot' ifPresent: [ :ignored | useSsh := false ].
  subOptions
    at: 'gitRoot'
    ifPresent: [ :arg | gitRoot := arg ]
    ifAbsent: [ 
      self
        error:
          'Missing required options <--gitRoot. See `man prj clone` for details.' ].
  ^ Rowan projectTools clone
    cloneSpecUrl: (subArguments at: 1)
    gitRootPath: gitRoot
    useSsh: useSsh
]

{ #category : 'commands' }
TDRowanPrjTool >> prjcommit [
  "
  prj commit --message=<message-ref> [--push[=remote]] <spec-url>
"

  | commitMessage specUrlString commitResult |
  self
    getSubcommandOptsMixedLongShort:
      {#('message' nil #'required').
      #('push' nil #'optional')}.
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <spec-url>. See `man prj commit` for details.' ].
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
  commitResult := Rowan projectTools commit
    commitSpecUrl: specUrlString
    message: commitMessage.
  subOptions
    at: 'push'
    ifPresent: [ :argOrNil | 
      ^ argOrNil
        ifNil: [ Rowan projectTools push pushSpecUrl: specUrlString ]
        ifNotNil: [ Rowan projectTools push pushSpecUrl: specUrlString remote: argOrNil ] ].
  ^ commitResult
]

{ #category : 'commands' }
TDRowanPrjTool >> prjdiff [
  "
  prj diff <spec-url>
"

  | diffText |
  self getSubcommandOptsMixedLongShort: {}.
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <spec-url>. See `man prj diff` for details.' ].
  diffText := Rowan projectTools diff diffSpecUrl: (subArguments at: 1).
  diffText
    editUsing:
      ((TDEditorSpec topez: topez editorAspect: #'edit')
        windowName: #'mcDiff';
        yourself)
]

{ #category : 'commands' }
TDRowanPrjTool >> prjlist [
]

{ #category : 'commands' }
TDRowanPrjTool >> prjload [
  "
  prj load [--loads=<load-expression>] <spec-url>
"

  | loads |
  self getSubcommandOptsMixedLongShort: {#('loads' nil #'required')}.
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <spec-url>. See `man prj load` for details.' ].
  subOptions at: 'loads' ifPresent: [ :arg | loads := arg evaluate ].
  ^ Rowan projectTools load loadSpecUrl: (subArguments at: 1)
]

{ #category : 'commands' }
TDRowanPrjTool >> prjlog [
  "
  prj log [--limit=<max-commit-log-entries>] <spec-url>
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
          'Missing required argument <spec-url>. See `man prj log` for details.' ].
  log := Rowan projectTools log commitLogSpecUrl: (subArguments at: 1) limit: logLimit.
  log
    editUsing:
      ((TDEditorSpec topez: topez editorAspect: #'edit')
        windowName: #'gitStatus';
        yourself).
  ^ log
]

{ #category : 'commands' }
TDRowanPrjTool >> prjpull [
  "
  prj pull [--remote=<git-remote-name>] <spec-url>
"

  | specUrlString |
  self getSubcommandOptsMixedLongShort: {#('remote' nil #'required')}.
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <spec-url>. See `man prj pull` for details.' ].
  specUrlString := subArguments at: 1.
  ^ subOptions
    at: 'remote'
    ifPresent: [ :remote | Rowan projectTools pull pullSpecUrl: specUrlString remote: remote ]
    ifAbsent: [ Rowan projectTools pull pullSpecUrl: specUrlString ]
]

{ #category : 'commands' }
TDRowanPrjTool >> prjpush [
  "
  prj push [--remote=<git-remote-name>] <spec-url>
"

  | specUrlString |
  self getSubcommandOptsMixedLongShort: {#('remote' nil #'required')}.
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <spec-url>. See `man prj push` for details.' ].
  specUrlString := subArguments at: 1.
  ^ subOptions
    at: 'remote'
    ifPresent: [ :remote | Rowan projectTools push pushSpecUrl: specUrlString remote: remote ]
    ifAbsent: [ Rowan projectTools push pushSpecUrl: specUrlString ]
]

{ #category : 'commands' }
TDRowanPrjTool >> prjread [
  "
  prj read <spec-url>
"

  | diffText |
  self getSubcommandOptsMixedLongShort: {}.
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <spec-url>. See `man prj read` for details.' ].
  ^ Rowan projectTools read readPackageSetForSpecUrl: (subArguments at: 1)
]

{ #category : 'commands' }
TDRowanPrjTool >> prjregister [
  "
  prj register [--name=<spec-name>] <spec-url>
"

  self getSubcommandOptsMixedLongShort: {#('name' nil #'required')}.
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <spec-url>. See `man prj register` for details.' ].
  ^ subOptions
    at: 'name'
    ifPresent: [ :specName | Rowan projectTools register registerSpecUrl: (subArguments at: 1) as: specName ]
    ifAbsent: [ Rowan projectTools register registerSpecUrl: (subArguments at: 1) ]
]

{ #category : 'commands' }
TDRowanPrjTool >> prjregistry [
  "
  prj registry [--names|--list]
"

  self
    getSubcommandOptsMixedLongShort:
      {#('names' nil #'none').
      #('list' nil #'none')}.
  ^ subOptions
    at: 'names'
    ifPresent: [ :ignored | Rowan projectTools registry registeredSpecNames ]
    ifAbsent: [ 
      subOptions
        at: 'list'
        ifPresent: [ :ignored | Rowan projectTools registry registeredSpecs ]
        ifAbsent: [ Rowan projectTools registry registry ] ]
]

{ #category : 'commands' }
TDRowanPrjTool >> prjrevert [
]

{ #category : 'commands' }
TDRowanPrjTool >> prjspec [
  "
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
"

  | shouldRegister specification specTool specName |
  specTool := Rowan projectTools spec.
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
                  'Missing required option --name when no <spec-url> specified. See `man prj spec` for details.' ].
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
                          '. Should be ''project'' or ''package''. See `man prj spec` for details.' ] ].
          shouldRegister := true ]
        ifAbsent: [ 
          self
            error:
              'Missing required option --type when no <spec-url> specified. See `man prj spec` for details.' ] ]
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
                  '<exportUrl> must be specified if repository has not been cloned. See `man prj spec` for details.' ].
          specTool exportSpecification: specification ]
        ifNotNil: [ specTool exportSpecification: specification toUrl: exportUrl ] ].
  ^ specification
]

{ #category : 'commands' }
TDRowanPrjTool >> prjtest [
  self error: 'not yet implemented'
]

{ #category : 'commands' }
TDRowanPrjTool >> prjunregister [
  "
  prj unregister <spec-url>
"

  self getSubcommandOptsMixedLongShort: {}.
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <spec-url>. See `man prj unregister` for details.' ].
  ^ Rowan projectTools unregister unregisterSpecUrl: (subArguments at: 1)
]

{ #category : 'commands' }
TDRowanPrjTool >> prjwrite [
  "
  prj write <spec-url>
"

  | diffText |
  self getSubcommandOptsMixedLongShort: {}.
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <spec-url>. See `man prj write` for details.' ].
  ^ Rowan projectTools write writeSpecUrl: (subArguments at: 1)
]
