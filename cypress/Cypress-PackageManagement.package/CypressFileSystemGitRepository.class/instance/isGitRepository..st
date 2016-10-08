"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: git querying
"
isGitRepository: aDirectory
  "Check that we have a git repository"

  | gitPath |
  gitPath := self gitCommand: #('rev-parse' '--show-toplevel') in: aDirectory.
  (gitPath indexOfSubCollection: 'fatal:') = 1
    ifTrue: [ ^ false ].
  ^ true