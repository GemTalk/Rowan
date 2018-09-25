! Package: BaselineOfSton


! Remove existing behavior from package BaselineOfSton
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'BaselineOfSton'.
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
										each isEmpty not and: [
											(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])
											or: [each first ~= $*]]]
					]
					ifFalse: [
							"*packagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each isEmpty not and: [
											each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]]
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
	subclass: 'BaselineOfSton'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'BaselineOfSton';
		comment: '';
		immediateInvariant.
true.
%

! Class Implementation for BaselineOfSton

! ------------------- Instance methods for BaselineOfSton

category: 'baseline'
method: BaselineOfSton
baseline: spec
  <baseline>
  spec for: #'common' do: [ spec
        package: 'STON-Core';
        package: 'STON-Tests' with: [ spec requires: 'STON-Core' ];
        yourself.
      spec
        group: 'default' with: #('Core');
        group: 'Core' with: #('STON-Core');
        group: 'UTF8' with: #();
        group: 'Tests' with: #('STON-Tests');
        yourself ].
  spec for: #(#'pharo' #'gemstone') do: [ spec
        package: 'STON-Tests'
          with: [ spec includes: #('STON-PharoCompatible-Tests') ];
        package: 'STON-PharoCompatible-Tests'
          with: [ spec requires: #('STON-Tests') ];
        package: 'STON-UTF8-Core' with: [ spec requires: 'STON-Core' ];
        package: 'STON-UTF8-Tests' with: [ spec requires: 'STON-UTF8-Core' ];
        yourself.
      spec
        group: 'default' with: #('Core');
        group: 'UTF8' with: #('STON-UTF8-Core');
        group: 'Tests' with: #('STON-UTF8-Tests');
        yourself ].
  spec for: #'pharo' do: [ spec
        package: 'STON-Core' with: [ spec includes: #('STON-Pharo-Core') ];
        package: 'STON-Pharo-Core' with: [ spec requires: #('STON-Core') ];
        package: 'STON-Tests' with: [ spec includes: #('STON-Pharo-Tests') ];
        package: 'STON-Pharo-Tests' with: [ spec requires: #('STON-Tests') ];
        yourself ].
  spec for: #(#'pharo.3.x' #'Pharo.4.x.') do: [ spec
        package: 'STON-Pharo-Tests' with: [ spec includes: 'STON-Pharo3x-Tests']; 
        package: 'STON-Pharo3x-Tests' with: [ spec requires: 'STON-Pharo-Tests'];
        yourself ].
  spec for: #'gemstoneCommon' do: [ spec
        package: 'STON-Core'
          with: [ spec includes: #('STON-GemStoneCommon-Core') ];
        package: 'STON-GemStoneCommon-Core'
          with: [ spec requires: #('STON-Core') ];
        package: 'STON-Tests'
          with: [ spec includes: #('STON-GemStoneCommon-Tests') ];
        package: 'STON-GemStoneCommon-Tests'
          with: [ spec requires: #('STON-Tests') ];
        yourself ].
  spec for: #'gsc3.x' do: [ spec
        package: 'STON-GemStoneCommon-Tests' with: [ spec includes: 'STON-GemStoneCommon3x-Tests']; 
        package: 'STON-GemStoneCommon3x-Tests' with: [ spec requires: 'STON-GemStoneCommon-Tests'];
        yourself ].
  spec for: #'gemstone64' do: [ spec
        package: 'STON-Core'
          with: [ spec includes: #('STON-GemStoneBase-Core') ];
        package: 'STON-GemStoneBase-Core'
          with: [ spec requires: #('STON-Core') ];
        package: 'STON-Tests'
          with: [ spec includes: #('STON-GemStoneBase-Tests') ];
        package: 'STON-GemStoneBase-Tests'
          with: [ spec requires: #('STON-Tests') ];
        yourself ].
  spec for: #'gemstone' do: [ spec
        package: 'STON-Core' with: [ spec includes: #('STON-GemStone-Core') ];
        package: 'STON-GemStone-Core' with: [ spec requires: #('STON-Core') ];
        package: 'STON-Tests' with: [ spec includes: #('STON-GemStone-Tests') ];
        package: 'STON-GemStone-Tests' with: [ spec requires: #('STON-Tests') ];
        yourself ]
%

! Class Extensions

! Class initializers 

doit
true.
%



! End of Package: BaselineOfSton


