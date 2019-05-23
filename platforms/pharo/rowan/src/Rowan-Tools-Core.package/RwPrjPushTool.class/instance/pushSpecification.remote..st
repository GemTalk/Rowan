smalltalk api
pushSpecification: aRwSpecification remote: remoteOrNil
  | remote |
  self specification: aRwSpecification.
  remoteOrNil
    ifNil: [ remote := specification repoSpec remote ]
    ifNotNil: [ remote := remoteOrNil ].
  ^ specification pushForTool: self remote: remote