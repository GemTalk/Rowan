Extension { #name : 'RowanQueryServicesTest' }

{ #category : '*rowan-services-tests-32x' }
RowanQueryServicesTest >> test_dontUseFileStream [
  | queryService results |
  self
    jadeiteIssueTested: #'issue611'
    withTitle: 'Use FileStreamPortable not FileStream'.
  queryService := RowanQueryService new.
  queryService organizer: ClassOrganizer new.
  queryService browseClassReferences: 'FileStream'.
  results := queryService queryResults.
  self assert: results size equals: 1.
  self assert: results first className equals: 'Stream'.
  self
    assert: results first selector
    equals: #'installStreamImplementationFrom:'
]
