# Basic class path
CP = .:/home/gavin/Scala/Util

# Class path including SCL
CPSCL = .:/home/gavin/Scala/Util:/home/gavin/Scala/SCL

# CP = .:/home/gavin/Scala/CSO:/home/gavin/Scala/CADS:/home/gavin/Scala/Util

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
$(EXDIR)/TimeoutChannelTester.class $(EXDIR)/TimeoutExchangerTester.class

NON_BINARY_STATEFUL_TESTERS = $(EXDIR)/ABCCounterTester.class			\
$(EXDIR)/BarrierCounterTester.class $(EXDIR)/TerminatingQueueTester.class	\
$(EXDIR)/CloseableChanTester.class

TESTERS = $(BINARY_HETERO_TESTERS) $(BINARY_STATEFUL_TESTERS)			\
$(BINARY_HOMO_TESTERS) $(NON_BINARY_TESTERS) $(NON_BINARY_STATEFUL_TESTERS)	\
$(EXDIR)/ChanTwoStepTester.class $(EXDIR)/ChanCounterTwoStepTester.class

all:	$(TESTERS)

clean:
	rm $(DIR)/*.class $(EXDIR)/*.class $(OBDIR)/*.class; fsc -shutdown

# Synchronisation objects based on semaphores

$(OBDIR)/TwoFamilies.class $(OBDIR)/OneFamily.class $(OBDIR)/ABC.class	\
  $(OBDIR)/ABCCounter.class $(OBDIR)/BarrierCounter.class		\
  $(OBDIR)/FilterChan.class: $(OBDIR)/Semaphore.class

# The testing machinery.

$(DIR)/HistoryLog.class: $(DIR)/Log.class

$(DIR)/Tester.class: $(DIR)/HistoryLog.class $(DIR)/ThreadUtil.class

$(DIR)/BinaryTester.class $(DIR)/BinaryStatefulTester.class $(DIR)/StatelessTester.class $(DIR)/StatefulTester.class: $(DIR)/Tester.class

$(DIR)/BinaryStatelessTester.class: $(DIR)/BinaryTester.class $(DIR)/FordFulkerson.class

$(DIR)/HomogeneousBinaryStatelessTester.class: $(DIR)/BinaryTester.class $(DIR)/DFS.class


# Each tester depends upon the testing machinery, and the synchronisation
# objects it is testing.

# Binary stateless heterogeneous

$(BINARY_HETERO_TESTERS):	$(DIR)/BinaryStatelessTester.class

$(EXDIR)/ChanTester.class:	$(OBDIR)/Chan.class

$(EXDIR)/FilterChanTester.class:	$(OBDIR)/FilterChan.class

$(EXDIR)/MenWomenTester.class: $(OBDIR)/MenWomen.class

# Binary stateless homogeneous

$(EXDIR)/ExchangerTester.class: $(OBDIR)/Exchanger.class $(DIR)/HomogeneousBinaryStatelessTester.class

# Binary stateful

$(BINARY_STATEFUL_TESTERS):  $(DIR)/BinaryStatefulTester.class

$(EXDIR)/ChanCounterTester.class: $(OBDIR)/ChanCounter.class

$(EXDIR)/TwoFamiliesTester.class: $(OBDIR)/TwoFamilies.class

$(EXDIR)/OneFamilyTester.class: $(OBDIR)/OneFamily.class

$(EXDIR)/ChanTwoStepTester.class: $(DIR)/TwoStepLinSpec.class

$(EXDIR)/ChanCounterTwoStepTester.class: $(DIR)/TwoStepLinSpec.class

# Non-binary stateless

$(NON_BINARY_TESTERS): $(DIR)/StatelessTester.class

$(EXDIR)/ABCTester.class: $(OBDIR)/ABC.class 

$(EXDIR)/BarrierTester.class:  $(OBDIR)/Barrier.class

$(EXDIR)/TimeoutChannelTester.class: $(OBDIR)/TimeoutChannel.class

$(EXDIR)/TimeoutExchangerTester.class: $(OBDIR)/TimeoutExchanger.class

# Non-binary stateful

$(NON_BINARY_STATEFUL_TESTERS): $(DIR)/StatefulTester.class

$(EXDIR)/ABCCounterTester.class: $(OBDIR)/ABCCounter.class 

$(EXDIR)/BarrierCounterTester.class: $(OBDIR)/BarrierCounter.class

$(EXDIR)/TerminatingQueueTester.class: $(OBDIR)/TerminatingQueue.class

$(EXDIR)/CloseableChanTester.class: $(OBDIR)/CloseableChan.class

# Recipes for compilation

FSC = fsc -deprecation -cp $(CP)

$(DIR)/StatefulTwoStepLinSpec.class: SynchronisationTesting/StatefulTwoStepLinSpec.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesting/StatefulTwoStepLinSpec.scala

$(DIR)/TwoStepLinSpec.class: SynchronisationTesting/TwoStepLinSpec.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesting/TwoStepLinSpec.scala

$(DIR)/%.class:     SynchronisationTesting/%.scala
	$(FSC) $<

$(OBDIR)/Chan.class: SynchronisationObjects/Chan.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationObjects/Chan.scala

$(OBDIR)/CloseableChan.class: SynchronisationObjects/CloseableChan.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationObjects/CloseableChan.scala

$(OBDIR)/Barrier.class: SynchronisationObjects/Barrier.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationObjects/Barrier.scala

$(EXDIR)/ChanTwoStepTester.class: SynchronisationTesters/ChanTwoStepTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/ChanTwoStepTester.scala

$(EXDIR)/ChanCounterTwoStepTester.class: SynchronisationTesters/ChanCounterTwoStepTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/ChanCounterTwoStepTester.scala

$(EXDIR)/CloseableChanTester.class: SynchronisationTesters/CloseableChanTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/CloseableChanTester.scala

$(OBDIR)/%.class:     SynchronisationObjects/%.scala
	$(FSC) $<

# $(OBDIR)/ChanTester.class: SynchronisationTesters/ChanTester.scala
# 	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/ChanTester.scala

$(EXDIR)/ChanTester.class:  SynchronisationTesters/ChanTester.scala
	fsc -cp $(CPSCL) SynchronisationTesters/ChanTester.scala

$(EXDIR)/%.class:     SynchronisationTesters/%.scala
	$(FSC) $<

%.class:     %.scala
	$FSC(CP) $<

