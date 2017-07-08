"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: git querying
"
gitCloneRepositoryAndCheckoutIn: aDirectoryName workingDirectory: workingDirectory
  "Do a clone on the remote repository and do a checkout on it to get the right branch. Complement the directory as well with the working directory."

  | gitDir branch |
  gitDir := workingDirectory , self fileUtils pathNameDelimiter
    , (aDirectoryName subStrings: '/') last.
  branch := self projectBranchOrTag.
  (self fileUtils directoryExists: gitDir)
    ifTrue: [ | targetDirBranch |
      self directoryPath: (self repositoryPath isEmpty
            ifTrue: [ gitDir ]
            ifFalse: [ gitDir , self fileUtils pathNameDelimiter , self repositoryPath ]).
      targetDirBranch := self currentBranch.
      targetDirBranch = branch
        ifFalse: [ self error: self class name asString
                ,
                  ' target directory already exists and is on another branch, cancelling clone and repository instanciation : '
                , gitDir ] ]
    ifFalse: [ self gitCommand: #('clone') , {'-b'.
              branch} , {remoteUrl.
              gitDir} in: workingDirectory.
      self directoryPath: (self repositoryPath isEmpty
            ifTrue: [ gitDir ]
            ifFalse: [ gitDir , self fileUtils pathNameDelimiter , self repositoryPath ]) ]
