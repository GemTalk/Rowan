"
notice: This work is protected by copyright. All rights reserved.
category: instance creation
"
on: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock
	"Create an instance of the receiver capable for sorting the dependencies of someCandidates.
	 aOneArgBlock is used to evaluate the key of the object depended on for a candidate.
	 anotherOneArgBlock is used to evaluate the key of the candidate itself."

	^self new
		initializeOn: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock;
		yourself.
