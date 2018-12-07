Extension { #name : 'GsFile' }

{ #category : '*filesystem-gemstone-kernel' }
GsFile >> truncateTo: anInteger [

	self setPositionTo: anInteger
	
	"work needs to be done here to call out to the bash shell script 'truncate -s anInteger' "

]
