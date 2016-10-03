! Package: BaselineOfCypress


! Remove existing behavior from package BaselineOfCypress
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'BaselineOfCypress'.
System myUserProfile symbolList do: [:symDict |
	symDict do: [:possibleClass |
			| toRemove |
		possibleClass isBehavior ifTrue: [
			{possibleClass. possibleClass class} do: [:aClass |
				aClass category = packageName
					ifTrue: [
							"*anythingbutpackagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])
										or: [each first ~= $*]]
					]
					ifFalse: [
							"*packagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]
					].
				toRemove do: [:each | aClass removeCategory: each].
			]
		]
	]
].
true.
%


! Class Declarations

doit
(BaselineOf
	subclass: 'BaselineOfCypress'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'BaselineOfCypress';
		comment: '';
		immediateInvariant.
true.
%

! Class Implementation for BaselineOfCypress

! ------------------- Instance methods for BaselineOfCypress

category: 'baseline'
method: BaselineOfCypress
baseline: spec
  <baseline>
  spec
    for: #'gemstoneBase'
    do: [ 
      spec
        package: 'Cypress-GemStoneCompatibilityLayer';
        package: 'Cypress-Definitions'
          with: [ spec requires: 'Cypress-GemStoneCompatibilityLayer' ];
        package: 'Cypress-Structure'
          with: [ spec requires: 'Cypress-Definitions' ];
        package: 'Cypress-GemStoneFileServer'
          with: [ spec requires: 'Cypress-Structure' ];
        package: 'Cypress-MesssageDigest'
          with: [ spec requires: 'Cypress-Structure' ];
        package: 'Cypress-Comparison'
          with: [ spec requires: 'Cypress-Definitions' ];
        package: 'Network-Url';
        package: 'Cypress-PackageManagement'
          with: [ 
              spec
                requires:
                  #('Cypress-Definitions' 'Network-Url' 'Cypress-GemStoneFileServer') ];
        package: 'Cypress-Mocks';
        package: 'Cypress-Tests'
          with: [ spec requires: #('Cypress-Mocks' 'Cypress-Definitions' 'Cypress-Structure') ];
        package: 'NetworkTests' with: [ spec requires: #('Network-Url') ];
        yourself.
      spec
        group: 'default' with: #('Core');
        group: 'Core'
          with:
            #('Cypress-Structure' 'Cypress-MesssageDigest' 'Cypress-PackageManagement' 'Cypress-Comparison');
        group: 'Tests' with: #('Cypress-Tests');
        group: 'Core Tests' with: #('Core' 'Tests' 'NetworkTests');
        yourself ]
%

category: 'accessing'
method: BaselineOfCypress
projectClass
  ^ MetacelloCypressBaselineProject
%

! Class Extensions

! Class initializers 

doit
true.
%



! End of Package: BaselineOfCypress


