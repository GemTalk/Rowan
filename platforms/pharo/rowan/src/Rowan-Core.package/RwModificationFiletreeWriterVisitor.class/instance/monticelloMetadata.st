package writing
monticelloMetadata
	"monticello metadata  --- for writing metadataless is the only supported format"

	^ monticelloMetadata ifNil: [ monticelloMetadata := false ]