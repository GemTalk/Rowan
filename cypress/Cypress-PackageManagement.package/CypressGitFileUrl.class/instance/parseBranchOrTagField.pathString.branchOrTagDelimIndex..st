"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-initialization
"
parseBranchOrTagField: parseBlock pathString: aPathString branchOrTagDelimIndex: branchOrTagDelimIndex
  | strm done escaped repoDelimIndex |
  strm := WriteStream on: String new.
  repoDelimIndex := branchOrTagDelimIndex + 1.
  escaped := done := false.
  [ done ] whileFalse: [ | char |
      repoDelimIndex > aPathString size
        ifTrue: [ done := true ]
        ifFalse: [ char := aPathString at: repoDelimIndex.
          char == $\
            ifTrue: [ escaped
                ifTrue: [ "$\ not legal in branch name ... literally ignored"
                  escaped := false ]
                ifFalse: [ escaped := true ] ]
            ifFalse: [ char == $/
                ifTrue: [ escaped
                    ifFalse: [ done := true ] ].
              done
                ifFalse: [ strm nextPut: char ].
              escaped := false ].
          repoDelimIndex := repoDelimIndex + 1 ] ].
  repoDelimIndex := repoDelimIndex - 1.
  parseBlock
    value: strm contents
    value: (aPathString copyFrom: repoDelimIndex + 1 to: aPathString size)