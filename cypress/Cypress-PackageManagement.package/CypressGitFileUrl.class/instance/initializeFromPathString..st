"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-initialization
"
initializeFromPathString: aPathString
  | projectDelim repoDelimIndex branchOrTagDelimIndex |
  projectBranchOrTag := repositoryPath := nil.
  projectDelim := aPathString indexOf: $/ startingAt: 2.
  repoDelimIndex := aPathString indexOf: $/ startingAt: projectDelim + 1.
  (branchOrTagDelimIndex := aPathString indexOf: $:) == 0
    ifTrue: [ repoDelimIndex == 0
        ifTrue: [ projectPath := aPathString ]
        ifFalse: [ projectPath := aPathString copyFrom: 1 to: repoDelimIndex - 1.
          repositoryPath := aPathString
            copyFrom: repoDelimIndex + 1
            to: aPathString size ] ]
    ifFalse: [ projectPath := aPathString copyFrom: 1 to: branchOrTagDelimIndex - 1.
      repoDelimIndex == 0
        ifTrue: [ projectBranchOrTag := aPathString
            copyFrom: branchOrTagDelimIndex + 1
            to: aPathString size ]
        ifFalse: [ projectPath := aPathString copyFrom: 1 to: branchOrTagDelimIndex - 1.
          self parseBranchOrTagField: [ :pv :rp | projectBranchOrTag := pv.
              repositoryPath := rp ] pathString: aPathString
          branchOrTagDelimIndex: branchOrTagDelimIndex ] ]