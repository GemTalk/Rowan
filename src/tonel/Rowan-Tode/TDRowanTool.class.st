Class {
	#name : 'TDRowanTool',
	#superclass : 'TDStandardTool',
	#category : 'Rowan-Tode'
}

{ #category : 'rowan project' }
TDRowanTool class >> clas [
  "self initialize"

  <topezCommand: 'clas' manPage: #'clasManPage'>
  ^ [ :topez :objIn :tokens :command | 
  self
    performSubCommand: topez
    objIn: objIn
    commandOptions: {#('help' $h #'none')}
    todeCommand: command ]
]

{ #category : 'rowan project' }
TDRowanTool class >> clasManPage [

	^RwClassTool new manPage
]

{ #category : 'initialization' }
TDRowanTool class >> initialize [
  self install
]

{ #category : 'initialization' }
TDRowanTool class >> installExampleOn: aTDTopezServer [
  "nothing for now"
]

{ #category : 'rowan project' }
TDRowanTool class >> pkg [
  "self initialize"

  <topezCommand: 'pkg' manPage: #'pkgManPage'>
  ^ [ :topez :objIn :tokens :command | 
  self
    performSubCommand: topez
    objIn: objIn
    commandOptions: {#('help' $h #'none')}
    todeCommand: command ]
]

{ #category : 'rowan project' }
TDRowanTool class >> pkgManPage [
  ^ Rowan packageTools new manPage
]

{ #category : 'initialization' }
TDRowanTool class >> priority [
    <topezCommandPriority>
    ^ 50
]

{ #category : 'rowan project' }
TDRowanTool class >> prj [
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
TDRowanTool class >> prjManPage [

	^Rowan projectTools new manPage
]

{ #category : 'rowan project' }
TDRowanTool class >> rep [
  "self initialize"

  <topezCommand: 'rep' manPage: #'repManPage'>
  ^ [ :topez :objIn :tokens :command | 
  self
    performSubCommand: topez
    objIn: objIn
    commandOptions: {#('help' $h #'none')}
    todeCommand: command ]
]

{ #category : 'rowan project' }
TDRowanTool class >> repManPage [
  ^ Rowan repositoryTools new manPage
]

{ #category : 'initialization' }
TDRowanTool class >> toolName [
  ^ 'rowan'
]

{ #category : 'accessing' }
TDRowanTool class >> toolReadMe [
  ^ self commandPath , ' provides GemTools emulation.'
]

{ #category : 'clas commands' }
TDRowanTool >> clasdiff [
  "
  clas diff class1 class2
"

  | clas1 clas2 |
  self getSubcommandOptsMixedLongShort: {}.
  clas1 := Smalltalk at: (subArguments at: 1).
  clas2 := Smalltalk at: (subArguments at: 2).
  RwClassTool clas diffClass: clas1 with: clas2
]

{ #category : 'pkg commands' }
TDRowanTool >> pkgcommit [

	"
  pkg commit --message=<message-ref> [--push[=remote]] <package-name> <spec-url>
"

	| commitMessage specUrlString commitResult packageNames |
	self
		getSubcommandOptsMixedLongShort:
			{#('message' nil #'required').
			#('push' nil #'optional')}.
	subArguments size < 1
		ifTrue: [ 
			self
				error:
					'Missing required arguments <package-name>... <spec-url>. See `man pkg commit` for details.' ].
	subArguments size = 1
		ifTrue: [ 
			self
				error:
					'Missing required arguments <package-name>... . See `man pkg commit` for details.' ].
	specUrlString := subArguments at: subArguments size.
	packageNames := subArguments copyFrom: 1 to: subArguments size - 1.
	subOptions
		at: 'message'
		ifPresent: [ :arg | commitMessage := arg ]
		ifAbsent: [ 
			commitMessage := (GsMultiLineTextInteraction
				prompt: 'Please enter a commit message for project: ' , specUrlString
				template: '') signal.
			commitMessage
				ifNil: [ ^ self inform: 'Commit aborted ... no commit message' ] ].
	commitResult := Rowan packageTools commit
		commitSpecUrl: specUrlString
		packageNames: packageNames
		message: commitMessage.
	subOptions
		at: 'push'
		ifPresent: [ :argOrNil | 
			^ argOrNil
				ifNil: [ Rowan projectTools push pushSpecUrl: specUrlString ]
				ifNotNil: [ Rowan projectTools push pushSpecUrl: specUrlString remote: argOrNil ] ].
	^ commitResult
]

{ #category : 'pkg commands' }
TDRowanTool >> pkgcopy [
  
]

{ #category : 'pkg commands' }
TDRowanTool >> pkgcreate [
  "
  pkg create [--commit[=<commit-message>] [--toLoad] --project=<project-name>] <package-name>...
"

  | doCommit commitMsg projectName tool toLoad |
  toLoad := doCommit := false.
  self
    getSubcommandOptsMixedLongShort:
      {#('project' nil #'required').
      #('commit' nil #'optional').
      #('toLoad' nil #'none')}.
  subOptions
    at: 'project'
    ifPresent: [ :arg | 
      projectName := arg.
      subOptions
        at: 'commit'
        ifPresent: [ :arg | 
          doCommit := true.
          arg
            ifNil: [ 
              commitMsg := (GsMultiLineTextInteraction
                prompt:
                  'Please enter a commit message for project: ' , projectName
                template: '') signal.
              commitMsg
                ifNil: [ 
                  | msg |
                  msg := 'pkg create aborted ... no commit message'.
                  self inform: msg.
                  ^ msg ] ]
            ifNotNil: [ commitMsg := arg ] ].
      subOptions at: 'toLoad' ifPresent: [ :ignored | toLoad := true ] ].
  tool := Rowan packageTools create.
  ^ projectName
    ifNil: [ tool createPackagesNamed: subArguments ]
    ifNotNil: [ 
      toLoad
        ifTrue: [ tool tool createToLoadPackagesNamed: subArguments forProjectNamed: projectName ]
        ifFalse: [ 
          tool
            createPackagesNamed: subArguments
            forProjectNamed: projectName
            doCommit: doCommit
            commitMsg: commitMsg ] ]
]

{ #category : 'pkg commands' }
TDRowanTool >> pkgdiff [
  
]

{ #category : 'pkg commands' }
TDRowanTool >> pkgedit [

	
]

{ #category : 'pkg commands' }
TDRowanTool >> pkglist [
  
]

{ #category : 'pkg commands' }
TDRowanTool >> pkgload [
  
]

{ #category : 'pkg commands' }
TDRowanTool >> pkgmove [
  
]

{ #category : 'pkg commands' }
TDRowanTool >> pkgrecompile [
  
]

{ #category : 'pkg commands' }
TDRowanTool >> pkgrename [
  
]

{ #category : 'pkg commands' }
TDRowanTool >> pkgrevert [
  
]

{ #category : 'pkg commands' }
TDRowanTool >> pkgwrite [
  
]

{ #category : 'prj commands' }
TDRowanTool >> prjclone [
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

{ #category : 'prj commands' }
TDRowanTool >> prjcommit [
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

{ #category : 'rep commands' }
TDRowanTool >> prjcreate [
  "
  prj create --format=[disk|cypress|filetree|tonel] --type=[metacello|package] \
             [--git] [--repoPath=<repo-path] --root=<path-to-root-directory> <project-name>
"

  | formats types format type projectName tool rootPath useGit repoPath |
  self
    getSubcommandOptsMixedLongShort:
      {#('format' nil #'required').
      #('git' nil #'none').
      #('type' nil #'required').
      #('repoPath' nil #'required').
      #('root' nil #'required')}.
  formats := #('disk' 'cypress' 'filetree' 'tonel').
  types := #('metacello' 'package').
  subArguments size < 1
    ifTrue: [ 
      self
        error:
          'Missing required argument <project-name>. See `man rep create` for details.' ].
  projectName := subArguments at: 1.
  subOptions
    at: 'format'
    ifPresent: [ :arg | 
      (formats includes: arg)
        ifFalse: [ 
          self
            error:
              'unknown repository format: ' , arg printString
                , ' See `man rep create` for details.' ].
      format := arg ]
    ifAbsent: [ 
      self
        error:
          'Missing required option --format. See `man rep create` for details.' ].
  subOptions
    at: 'root'
    ifPresent: [ :arg | rootPath := arg ]
    ifAbsent: [ self error: 'Missing required option --root. See `man rep create` for details.' ].
  subOptions
    at: 'type'
    ifPresent: [ :arg | 
      (types includes: arg)
        ifFalse: [ 
          self
            error:
              'unknown repository type: ' , arg printString
                , ' See `man rep create` for details.' ].
      type := arg ]
    ifAbsent: [ self error: 'Missing required option --type. See `man rep create` for details.' ].
  useGit := subOptions includesKey: 'git'.
  repoPath := subOptions
    at: 'repoPath'
    ifPresent: [ :arg | arg ]
    ifAbsent: [ '' ].
  tool := Rowan projectTools create.
  ^ type = 'metacello'
    ifTrue: [ 
      tool
        createMetacelloProject: projectName
        format: format
        root: rootPath
        repoPath: repoPath
        useGit: useGit ]
    ifFalse: [ 
      "type = package"
      tool
        createPackageProject: projectName
        format: format
        root: rootPath
        repoPath: repoPath
        useGit: useGit ]
]

{ #category : 'prj commands' }
TDRowanTool >> prjdiff [
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

{ #category : 'prj commands' }
TDRowanTool >> prjlist [

	"
  prj list
"

	self getSubcommandOptsMixedLongShort: {}.

	^ Rowan projectTools list openProjectList: topez
]

{ #category : 'prj commands' }
TDRowanTool >> prjload [
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

{ #category : 'prj commands' }
TDRowanTool >> prjlog [
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

{ #category : 'prj commands' }
TDRowanTool >> prjpull [
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

{ #category : 'prj commands' }
TDRowanTool >> prjpush [
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

{ #category : 'prj commands' }
TDRowanTool >> prjread [

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
	^ Rowan projectTools read readProjectSetForSpecUrl: (subArguments at: 1)
]

{ #category : 'prj commands' }
TDRowanTool >> prjregister [
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

{ #category : 'prj commands' }
TDRowanTool >> prjregistry [
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

{ #category : 'prj commands' }
TDRowanTool >> prjrevert [
]

{ #category : 'prj commands' }
TDRowanTool >> prjspec [

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
						ifTrue: [ specification := specTool newProjectSpecNamed: specName useGit: true ]
						ifFalse: [ 
							arg = 'package'
								ifTrue: [ specification := specTool newPackageSpecNamed: specName useGit: true ]
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
		ifPresent: [ :arg | (specification platformSpec at: 'gemstone') defaultSymbolDictName: arg ].
	subOptions
		at: 'env'
		ifPresent: [ :arg | (specification platformSpec at: 'gemstone') methodEnv: arg asNumber ].
	subOptions
		at: 'userId'
		ifPresent: [ :arg | (specification platformSpec at: 'gemstone') userId: arg ].
	subOptions
		at: 'packages'
		ifPresent: [ :arg | specification packageNamesInLoadOrder: arg evaluate ].
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

{ #category : 'prj commands' }
TDRowanTool >> prjtest [
  self error: 'not yet implemented'
]

{ #category : 'prj commands' }
TDRowanTool >> prjunregister [
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

{ #category : 'prj commands' }
TDRowanTool >> prjwrite [

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

{ #category : 'rep commands' }
TDRowanTool >> repadd [
  "
  rep add --format=[disk|cypress|filetree|tonel] --type=[metacello|package] <project-name>
"

  
]

{ #category : 'rep commands' }
TDRowanTool >> repcommit [
  
]

{ #category : 'rep commands' }
TDRowanTool >> replist [
  
]

{ #category : 'rep commands' }
TDRowanTool >> reppackages [
  
]

{ #category : 'rep commands' }
TDRowanTool >> reppull [
  
]

{ #category : 'rep commands' }
TDRowanTool >> reppush [
  
]

{ #category : 'rep commands' }
TDRowanTool >> repremove [
  
]
