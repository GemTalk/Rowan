Class {
	#name : 'RwRowanSample9V2Test',
	#superclass : 'RwRowanSample9Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwRowanSample9V2Test class >> _adjustSpecForIssue805: loadSpecification [
	"https://github.com/GemTalk/Rowan/issues/805"

	"have to use https for project url if NOT dhenrich"

	(System gemEnvironmentVariable: 'USER') ~= 'dhenrich'
		ifTrue: [ 
			(loadSpecification projectUrl includesString: 'RowanSample9')
				ifTrue: [ loadSpecification gitUrl: 'https://github.com/dalehenrich/RowanSample9V2.git' ]
				ifFalse: [ 
					(loadSpecification projectUrl includesString: 'RowanSample10')
						ifTrue: [ loadSpecification gitUrl: 'https://github.com/dalehenrich/RowanSample10.git' ]
						ifFalse: [ self error: 'cannot convert load spec project url to use https://' ] ] ]
		ifFalse: [ 
			(loadSpecification projectUrl includesString: 'RowanSample9')
				ifTrue: [ loadSpecification gitUrl: 'git@github.com:dalehenrich/RowanSample9V2.git' ]
				ifFalse: [ 
					(loadSpecification projectUrl includesString: 'RowanSample10')
						ifTrue: [ loadSpecification gitUrl: 'git@github.com:dalehenrich/RowanSample10.git' ]
						ifFalse: [ 
							(loadSpecification projectUrl includesString: 'RowanSample11')
								ifTrue: [ loadSpecification gitUrl: 'git@github.com:dalehenrich/RowanSample11.git' ]
								ifFalse: [ 
									(loadSpecification projectUrl includesString: 'RowanSample12')
										ifTrue: [ loadSpecification gitUrl: 'git@github.com:dalehenrich/RowanSample12.git' ]
										ifFalse: [ 
											(loadSpecification projectUrl includesString: 'RowanSample11')
												ifTrue: [ loadSpecification gitUrl: 'git@github.com:dalehenrich/RowanSample11.git' ]
												ifFalse: [ 
													(loadSpecification projectUrl includesString: 'RowanSample12')
														ifTrue: [ loadSpecification gitUrl: 'git@github.com:dalehenrich/RowanSample12.git' ]
														ifFalse: [ self error: 'cannot convert load spec project url to use https://' ] ] ] ] ] ] ]
]

{ #category : 'private' }
RwRowanSample9V2Test class >> _gitPullSessionCacheKey [
	^ #'RowanSample9GitBranchDict'
]

{ #category : 'private' }
RwRowanSample9V2Test class >> _loadSpecSessionCacheKey [
	^ #'RowanSample9LoadSpecsDict'
]

{ #category : 'private' }
RwRowanSample9V2Test class >> _rowanSample_0000_load_spec [
	^ (RwSpecification
		fromUrl:
			'https://raw.githubusercontent.com/dalehenrich/RowanSample9V2/master/specs/spec_0000.ston')
		projectAlias: 'RowanSample9';
		projectsHome: self _testRowanProjectsSandbox;
		yourself
]
