%%% snmp_simulator.hrl

-define(Unsigned32, 0..4294967295).
-define(Unsigned64, 0..18446744073709551615).
-define(Integer32, -2147483648..2147483647).

-record(alarmModelTable,
		{key :: {AlarmListName :: string(),
				AlarmModelIndex :: ?Unsigned32,
				AlarmModelState :: ?Unsigned32} | undefined | '$1',
		alarmModelNotificationId = [0, 0] :: snmpa:oid() | '_',
		alarmModelVarbindIndex = 0 :: ?Unsigned32 | '_',
		alarmModelVarbindValue = 0 :: ?Integer32 | '_',
		alarmModelDescription = "" :: string() | '_',
		alarmModelSpecificPointer = [0, 0] :: snmpa:oid() | '_',
		alarmModelVarbindSubtree = [0, 0] :: snmpa:oid() | '_',
		alarmModelResourcePrefix = [0, 0] :: snmpa:oid() | '_',
		alarmModelRowStatus = 3 :: 1..6 | '_'}).

-record(alarmActiveTable,
		{key :: {AlarmListName :: string(),
				AlarmActiveDateAndTime :: string(),
				AlarmActiveIndex :: ?Unsigned32} | undefined,
		alarmActiveEngineID :: [byte()],
		alarmActiveEngineAddressType :: 0..4 | 16,
		alarmActiveEngineAddress :: [byte()],
		alarmActiveContextName :: string(),
		alarmActiveVariables :: ?Unsigned32,
		alarmActiveNotificationID :: snmpa:oid(),
		alarmActiveResourceId :: snmpa:oid(),
		alarmActiveDescription :: string(),
		alarmActiveLogPointers :: snmpa:oid(),
		alarmActiveModelPointer :: snmpa:oid(),
		alarmActiveSpecificPointer :: snmpa:oid()}).

-record(alarmActiveVariableTable,
		{key :: {AlarmListName :: string(),
				AlarmActiveIndex :: ?Unsigned32,
				AlarmActiveVariableIndex :: ?Unsigned32} | undefined,
		alarmActiveVariableID :: snmpa:oid(),
		alarmActiveVariableValueType :: 1..9,
		alarmActiveVariableCounter32Val :: ?Unsigned32,
		alarmActiveVariableUnsigned32Val :: ?Unsigned32,
		alarmActiveVariableTimeTicksVal :: ?Unsigned32,
		alarmActiveVariableInteger32Val :: ?Integer32,
		alarmActiveVariableOctetStringVal :: [byte()],
		alarmActiveVariableIpAddressVal :: string(),
		alarmActiveVariableOidVal :: snmpa:oid(),
		alarmActiveVariableCounter64Val :: ?Unsigned64,
		alarmActiveVariableOpaqueVal :: [byte()]}).

-record(alarmActiveStatsTable,
		{key :: (AlarmListName :: string() | undefined),
		alarmActiveStatsActiveCurrent :: ?Unsigned32,
		alarmActiveStatsActives :: ?Unsigned32,
		alarmActiveStatsLastRaise :: ?Unsigned32,
		alarmActiveStatsLastClear :: ?Unsigned32}).

-record(alarmClearTable,
		{key :: {AlarmListName :: string(),
				AlarmClearDateAndTime :: string(),
				AlarmClearIndex :: ?Unsigned32} | undefined,
		alarmClearDateAndTime :: string(),
		alarmClearEngineID :: [byte()],
		alarmClearEngineAddressType :: 0..4 | 16,
		alarmClearEngineAddress :: [byte()],
		alarmClearContextName :: string(),
		alarmClearNotificationID :: snmpa:oid(),
		alarmClearResourceId :: snmpa:oid(),
		alarmClearLogIndex :: ?Unsigned32,
		alarmClearModelPointer :: snmpa:oid()}).

-record(ituAlarmTable,
		{key :: {AlarmListName :: string(),
				AlarmModelIndex :: ?Unsigned32,
				ItuAlarmPerceivedSeverity :: 1..6} | undefined,
		 ituAlarmEventType :: 1..11 | undefined,
		 ituAlarmProbableCause :: 1..1024| undefined,
		 ituAlarmAdditionalText :: string() | undefined,
		 ituAlarmGenericModel = [0, 0] :: snmpa:oid()}).

-record(ituAlarmActiveTable,
		{key :: {AlarmListName :: string(),
				AlarmActiveDateAndTime :: string(),
				AlarmActiveIndex :: ?Unsigned32} | undefined,
		ituAlarmActiveTrendIndication :: 1..3,
		ituAlarmActiveDetector :: snmpa:oid(),
		tuAlarmActiveServiceProvider :: snmpa:oid(),
		ituAlarmActiveServiceUser :: snmpa:oid()}).

-record(ituAlarmActiveStatsTable,
		{key :: (AlarmListName :: string() | undefined),
		ituAlarmActiveStatsIndeterminateCurrent :: ?Unsigned32,
		ituAlarmActiveStatsCriticalCurrent :: ?Unsigned32,
		ituAlarmActiveStatsMajorCurrent :: ?Unsigned32,
		ituAlarmActiveStatsMinorCurrent :: ?Unsigned32,
		ituAlarmActiveStatsWarningCurrent :: ?Unsigned32,
		ituAlarmActiveStatsIndeterminates :: ?Unsigned32,
		ituAlarmActiveStatsCriticals :: ?Unsigned32,
		ituAlarmActiveStatsMajors :: ?Unsigned32,
		ituAlarmActiveStatsMinors :: ?Unsigned32,
		ituAlarmActiveStatsWarnings ::?Unsigned32}).

