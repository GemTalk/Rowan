smalltalk api
pullSpecification: aRwSpecification remote: remoteOrNil
  | remote |
  self specification: aRwSpecification.
  remoteOrNil
    ifNil: [ remote := specification repoSpec remote ]
    ifNotNil: [ remote := remoteOrNil ].
  ^ specification pullForTool: self remote: remote