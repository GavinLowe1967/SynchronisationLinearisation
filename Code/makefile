CP = .:/home/gavin/Scala/CSO:/home/gavin/Scala/CADS:/home/gavin/Scala/Util

# Directory for the testing architecture
DIR = synchronisationTesting

# Directory for specific testers
EXDIR = synchronisationTester

# Directory for synchronisation objects
OBDIR = synchronisationObject

# The different types of testers.

BINARY_HETERO_TESTERS = $(EXDIR)/ChanTester.class		\
$(EXDIR)/FilterChanTester.class $(EXDIR)/MenWomenTester.class 

BINARY_STATEFUL_TESTERS = $(EXDIR)/ChanCounterTester.class	\
$(EXDIR)/TwoFamiliesTester.class $(EXDIR)/OneFamilyTester.class

BINARY_HOMO_TESTERS = $(EXDIR)/ExchangerTester.class

NON_BINARY_TESTERS = $(EXDIR)/ABCTester.class $(EXDIR)/BarrierTester.class	\
$(EXDIR)/TimeoutChannelTester.class

NON_BINARY_STATEFUL_TESTERS = $(EXDIR)/ABCCounterTester.class			\
$(EXDIR)/BarrierCounterTester.class $(EXDIR)/TerminatingQueueTester.class

TESTERS = $(BINARY_HETERO_TESTERS) $(BINARY_STATEFUL_TESTERS)	\
$(BINARY_HOMO_TESTERS) $(NON_BINARY_TESTERS) $(NON_BINARY_STATEFUL_TESTERS)

all:	$(TESTERS)

clean:
	rm $(DIR)/*.class $(EXDIR)/*.class $(OBDIR)/*.class; fsc -shutdown

# The testing machinery.

$(DIR)/HistoryLog.class: $(DIR)/Log.class

$(DIR)/Tester.class: $(DIR)/HistoryLog.class $(DIR)/ThreadUtil.class

$(DIR)/BinaryTester.class: $(DIR)/Tester.class

$(DIR)/BinaryStatelessTester.class: $(DIR)/BinaryTester.class $(DIR)/FordFulkerson.class

$(DIR)/HomogeneousBinaryStatelessTester.class: $(DIR)/BinaryTester.class $(DIR)/DFS.class

$(DIR)/BinaryStatefulTester.class: $(DIR)/Tester.class

# Each tester depends upon the testing machinery, and the synchronisation
# objects it is testing.

# Binary stateless heterogeneous

$(BINARY_HETERO_TESTERS):	$(DIR)/BinaryStatelessTester.class

$(EXDIR)/ChanTester.class:	$(OBDIR)/FaultyChan.class

$(EXDIR)/FilterChanTester.class:	$(OBDIR)/FilterChan.class

$(EXDIR)/MenWomenTester.class: $(OBDIR)/MenWomen.class

# Binary stateless homogeneous

$(EXDIR)/ExchangerTester.class: $(OBDIR)/Exchanger.class $(DIR)/HomogeneousBinaryStatelessTester.class

# Binary stateful

$(BINARY_STATEFUL_TESTERS):  $(DIR)/BinaryStatefulTester.class

$(EXDIR)/ChanCounterTester.class: $(OBDIR)/ChanCounter.class

$(EXDIR)/TwoFamiliesTester.class: $(OBDIR)/TwoFamilies.class

$(EXDIR)/OneFamilyTester.class: $(OBDIR)/OneFamily.class

# Non-binary stateless

$(NON_BINARY_TESTERS): $(DIR)/StatelessTester.class

$(EXDIR)/ABCTester.class: $(OBDIR)/ABC.class 

$(EXDIR)/BarrierTester.class:  $(OBDIR)/Barrier.class

$(EXDIR)/TimeoutChannelTester.class: $(OBDIR)/TimeoutChannel.class

# Non-binary stateful

$(NON_BINARY_STATEFUL_TESTERS): $(DIR)/StatefulTester.class

$(EXDIR)/ABCCounterTester.class: $(OBDIR)/ABCCounter.class 

$(EXDIR)/BarrierCounterTester.class: $(OBDIR)/BarrierCounter.class

$(EXDIR)/TerminatingQueueTester.class: $(OBDIR)/TerminatingQueue.class

# Recipes for compilation

$(DIR)/%.class:     SynchronisationTesting/%.scala
	fsc -cp $(CP) $<

$(OBDIR)/%.class:     SynchronisationObjects/%.scala
	fsc -cp $(CP) $<

$(EXDIR)/%.class:     SynchronisationTesters/%.scala
	fsc -cp $(CP) $<

%.class:     %.scala
	fsc -cp $(CP) $<

