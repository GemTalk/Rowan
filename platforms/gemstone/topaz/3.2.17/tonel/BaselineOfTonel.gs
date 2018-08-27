! Package: BaselineOfTonel


! Remove existing behavior from package BaselineOfTonel
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'BaselineOfTonel'.
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
	subclass: 'BaselineOfTonel'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'BaselineOfTonel';
		comment: '';
		immediateInvariant.
true.
%

! Class Implementation for BaselineOfTonel

! ------------------- Instance methods for BaselineOfTonel

category: 'baselines'
method: BaselineOfTonel
baseline: spec
  <baseline>
  spec
    for: #'common'
    do: [ 
      spec
        package: 'Tonel-Core';
        package: 'Tonel-Tests' with: [ spec requires: #('Tonel-Core') ];
        package: 'Tonel-Monticello-Tests'
          with: [ spec requires: #('Tonel-Tests') ];
        package: 'MonticelloTonel-Core'
          with: [ spec requires: #('Tonel-Core') ];
        package: 'MonticelloTonel-Tests'
          with: [ spec requires: #('MonticelloTonel-Core') ];
        yourself.
      spec
        group: 'monticello' with: #('MonticelloTonel-Core');
        group: 'monticello tests'
          with: #('MonticelloTonel-Tests' 'Tonel-Monticello-Tests');
        yourself ].
  spec
    for: #'pharo'
    do: [ 
      spec
        package: 'Tonel-Core' with: [ spec includes: #('Tonel-Pharo-Core') ];
        package: 'Tonel-Pharo-Core' with: [ spec requires: #('Tonel-Core') ];
        package: 'MonticelloTonel-Core'
          with: [ spec includes: #('MonticelloTonel-FileSystem' 'MonticelloTonel-Pharo-Core') ];
        package: 'MonticelloTonel-FileSystem'
          with: [ spec requires: #('MonticelloTonel-Core') ];
        package: 'MonticelloTonel-Pharo-Core'
          with: [ spec requires: #('MonticelloTonel-Core') ];
        package: 'MonticelloTonel-Tests'
          with: [ spec includes: #('MonticelloTonel-Pharo-Tests') ];
        package: 'MonticelloTonel-Pharo-Tests'
          with: [ spec requires: #('MonticelloTonel-Tests') ];
        yourself.
      spec
        group: 'default' with: #('monticello' 'monticello tests');
        yourself ].
  spec
    for: #'gemstoneCommon'
    do: [ 
      spec
        package: 'Tonel-Core'
          with: [ spec includes: #('Tonel-GemStoneCommon-Core') ];
        package: 'Tonel-GemStoneCommon-Core'
          with: [ spec requires: #('Tonel-Core') ];
        package: 'CypressTonel-Core' with: [ spec requires: #('Tonel-Core') ];
        package: 'Tonel-Cypress-Tests'
          with: [ spec requires: #('Tonel-Tests') ];
        package: 'Tonel-GemStone-Tests'
          with: [ spec requires: #('Tonel-Tests') ];
        package: 'CypressTonel-Tests'
          with: [ spec requires: #('CypressTonel-Core') ];
        package: 'CypressTonel-Tests'
          with: [ spec includes: #('CypressTonel-GemStone-Tests') ];
        package: 'CypressTonel-GemStone-Tests'
          with: [ spec requires: #('CypressTonel-Tests') ];
        yourself.
      spec
        group: 'cypress' with: #('Tonel-Core');
        group: 'cypress tests'
          with:
            #('Tonel-Tests' 'Tonel-Cypress-Tests' 'CypressTonel-Tests' 'Tonel-GemStone-Tests');
        yourself ].
  spec
    for: #'gemstone64'
    do: [ 
      spec
        package: 'Tonel-Core'
          with: [ spec includes: #('Tonel-GemStoneBase-Core') ];
        package: 'Tonel-GemStoneBase-Core'
          with: [ spec requires: #('Tonel-Core') ];
        yourself.
      spec
        group: 'default' with: #('cypress' 'cypress tests');
        yourself ].
  spec
    for: #'gemstone'
    do: [ 
      spec
        package: 'Tonel-Core' with: [ spec includes: #('Tonel-GemStone-Core') ];
        package: 'Tonel-GemStone-Core' with: [ spec requires: #('Tonel-Core') ];
        package: 'MonticelloTonel-Core'
          with: [ 
              spec
                includes:
                  #('MonticelloTonel-GemStone-Directory' 'MonticelloTonel-GemStone-Core') ];
        package: 'MonticelloTonel-GemStone-Directory'
          with: [ spec requires: #('MonticelloTonel-Core') ];
        package: 'MonticelloTonel-GemStone-Core'
          with: [ spec requires: #('MonticelloTonel-Core') ];
        package: 'MonticelloTonel-Tests'
          with: [ spec includes: #('MonticelloTonel-GemStone-Tests') ];
        package: 'MonticelloTonel-GemStone-Tests'
          with: [ spec requires: #('MonticelloTonel-Tests') ];
        yourself.
      spec
        group: 'default' with: #('monticello' 'monticello tests');
        yourself ]
%

category: 'accessing'
method: BaselineOfTonel
projectClass
  Smalltalk at: #'MetacelloCypressBaselineProject' ifPresent: [ :cl | ^ cl ].
  ^ super projectClass
%

! Class Extensions

! Class initializers 

doit
true.
%



! End of Package: BaselineOfTonel


