"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
initializeUrl: anUrl andAlias: aString
  super initializeUrl: anUrl andAlias: aString.
  anUrl hasHost
    ifTrue: [ | protocol path |
      self repositoryPath: anUrl repositoryPath.
      self branch: (self class extractBranch: anUrl).
      path := self class extractPath: anUrl.
      protocol := anUrl queryAt: 'protocol' ifAbsent: [ 'git' ].
      self readonly: (anUrl queryAt: 'readOnly' ifAbsent: [  ]) notNil.
      (anUrl hasUsername and: [ anUrl hasPassword ])
        ifTrue: [ protocol := 'https' ].
      protocol = 'git'
        ifTrue: [ self remoteUrl: 'git@' , anUrl host , ':' , path ].
      protocol = 'https'
        ifTrue: [ | targetUrl |
          targetUrl := 'https://' asZnUrl.
          targetUrl host: anUrl host.
          targetUrl addPathSegments: ($/ split: path).
          targetUrl username: anUrl username.
          targetUrl password: anUrl password.
          self remoteUrl: targetUrl printString ].
      self gitCloneRepositoryAndCheckoutIn: (path
            ifNil: [ anUrl host ]
            ifNotNil: [ :n | n asFileReference basenameWithoutExtension ]).
      (self isGitRepository: self directory)
        ifFalse: [ self error: 'This url is not a git repository' , anUrl printString ] ]
    ifFalse: [ self directory: (self urlAsFileReference: anUrl) ]