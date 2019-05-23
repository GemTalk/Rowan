testing
canCommit
  ^ self repositoryUrl notNil and: [ self repoSpec canCommit ]