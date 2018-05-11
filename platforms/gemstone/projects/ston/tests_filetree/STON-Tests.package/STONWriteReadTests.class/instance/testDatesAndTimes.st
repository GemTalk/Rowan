tests
testDatesAndTimes
  | datesAndTimes |
  datesAndTimes := STON listClass
    withAll:
      {(Time fromSeconds: Time now asSeconds).
      (Date today).
      (DateAndTime fromString: DateAndTime now asString).	"normalize for comparison purposes"
      (TimeStamp fromString: TimeStamp now asString)}.			"normalize for comparison purposes"
  self serializeAndMaterialize: datesAndTimes