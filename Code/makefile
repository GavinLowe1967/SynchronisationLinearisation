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

# Directory for experiments
EXPDIR = experiments

##### The different types of testers.

BINARY_HETERO_TESTERS = $(EXDIR)/ChanTester.class		\
$(EXDIR)/FilterChanTester.class $(EXDIR)/MenWomenTester.class 

BINARY_STATEFUL_TESTERS = $(EXDIR)/ChanCounterTester.class	\
$(EXDIR)/TwoFamiliesTester.class $(EXDIR)/OneFamilyTester.class

BINARY_HOMO_TESTERS = $(EXDIR)/ExchangerTester.class

NON_BINARY_TESTERS = $(EXDIR)/ABCTester.class $(EXDIR)/BarrierTester.class	\
$(EXDIR)/TimeoutChannelTester.class $(EXDIR)/TimeoutExchangerTester.class

NON_BINARY_STATEFUL_TESTERS = $(EXDIR)/ABCCounterTester.class			\
$(EXDIR)/BarrierCounterTester.class $(EXDIR)/TerminatingQueueTester.class	\
$(EXDIR)/CloseableChanTester.class $(EXDIR)/ResignableBarrierTester.class

TWO_STEP_TESTERS = $(EXDIR)/ChanTwoStepTester.class				\
$(EXDIR)/ChanCounterTwoStepTester.class $(EXDIR)/MenWomenTwoStepTester.class	\
$(EXDIR)/ExchangerTwoStepTester.class $(EXDIR)/ABCTwoStepTester.class		\
$(EXDIR)/BarrierTwoStepTester.class						\
$(EXDIR)/TimeoutExchangerTwoStepTester.class					\
$(EXDIR)/TwoFamiliesTwoStepTester.class						\
$(EXDIR)/ResignableBarrierTwoStepTester.class					\
$(EXDIR)/TerminatingQueueTwoStepTester.class					\
$(EXDIR)/TimeoutChannelTwoStepTester.class					\
$(EXDIR)/CloseableChannelTwoStepTester.class					\
$(EXDIR)/FilterChanTwoStepTester.class $(EXDIR)/OneFamilyTwoStepTester.class

TESTERSX = $(BINARY_HETERO_TESTERS) $(BINARY_STATEFUL_TESTERS)			\
$(BINARY_HOMO_TESTERS) $(NON_BINARY_TESTERS) $(NON_BINARY_STATEFUL_TESTERS)	\
$(TWO_STEP_TESTERS)

TESTERS = $(EXDIR)/Tester.class $(TESTERSX)

EXPERIMENTS = $(EXPDIR)/Experiment.class $(EXPDIR)/ParameterExperiment.class	\
$(EXPDIR)/BugFinderExperiment.class $(EXPDIR)/BugParametersExperiment.class	\
$(EXPDIR)/ProgressExperiment.class $(EXPDIR)/TwoStepExperiment.class

all:	$(TESTERS) $(EXPERIMENTS)

clean:
	rm $(DIR)/*.class $(EXDIR)/*.class $(OBDIR)/*.class; fsc -shutdown

# Synchronisation objects based on semaphores

$(OBDIR)/TwoFamilies.class $(OBDIR)/OneFamily.class $(OBDIR)/ABC.class	\
  $(OBDIR)/ABCCounter.class $(OBDIR)/BarrierCounter.class		\
  $(OBDIR)/FilterChan.class: $(OBDIR)/Semaphore.class

##### The testing machinery.

$(DIR)/HistoryLog.class: $(DIR)/Log.class

$(DIR)/Tester.class: $(DIR)/HistoryLog.class $(DIR)/ThreadUtil.class

$(DIR)/BinaryTester.class $(DIR)/BinaryStatefulTester.class $(DIR)/StatelessTester.class $(DIR)/StatefulTester.class: $(DIR)/Tester.class

$(DIR)/BinaryStatelessTester.class: $(DIR)/BinaryTester.class $(DIR)/FordFulkerson.class

$(DIR)/HomogeneousBinaryStatelessTester.class: $(DIR)/BinaryTester.class $(DIR)/DFS.class

##### Testers

# Each tester depends upon the testing machinery, and the synchronisation
# objects it is testing.

$(TESTERSX):	$(EXDIR)/Tester.class

# Binary stateless heterogeneous

$(BINARY_HETERO_TESTERS):	$(DIR)/BinaryStatelessTester.class

$(EXDIR)/ChanTester.class:	$(OBDIR)/Chan.class

$(EXDIR)/FilterChanTester.class:	$(OBDIR)/FilterChan.class

$(EXDIR)/MenWomenTester.class: $(OBDIR)/MenWomen.class

$(EXDIR)/MenWomenTwoStepTester.class: $(DIR)/TwoStepLinSpec.class 

# Binary stateless homogeneous

$(EXDIR)/ExchangerTester.class: $(OBDIR)/Exchanger.class $(DIR)/HomogeneousBinaryStatelessTester.class

$(EXDIR)/ExchangerTwoStepTester.class: $(OBDIR)/Exchanger.class \
  $(DIR)/TwoStepHomoLinSpec.class

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

$(EXDIR)/ResignableBarrierTester.class: $(OBDIR)/ResignableBarrier.class

$(EXDIR)/TimeoutChannelTester.class: $(OBDIR)/TimeoutChannel.class

$(EXDIR)/TimeoutExchangerTester.class: $(OBDIR)/TimeoutExchanger.class

$(EXDIR)/ABCTwoStepTester.class: $(OBDIR)/ABC.class $(DIR)/TwoStepLinSpec3.class 

# Non-binary stateful

$(NON_BINARY_STATEFUL_TESTERS): $(DIR)/StatefulTester.class

$(EXDIR)/ABCCounterTester.class: $(OBDIR)/ABCCounter.class 

$(EXDIR)/BarrierCounterTester.class: $(OBDIR)/BarrierCounter.class

$(EXDIR)/TerminatingQueueTester.class: $(OBDIR)/TerminatingQueue.class

$(EXDIR)/CloseableChanTester.class: $(OBDIR)/CloseableChan.class


##### Recipes for compilation

FSC = fsc -deprecation -cp $(CP)

# Testing framework.  The two-step approach needs SCL

$(DIR)/StatefulTwoStepLinSpec.class: SynchronisationTesting/StatefulTwoStepLinSpec.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesting/StatefulTwoStepLinSpec.scala

$(DIR)/TwoStepLinSpec.class: SynchronisationTesting/TwoStepLinSpec.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesting/TwoStepLinSpec.scala


$(DIR)/TwoStepHomoLinSpec.class: SynchronisationTesting/TwoStepHomoLinSpec.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesting/TwoStepHomoLinSpec.scala

$(DIR)/TwoStepLinSpec3.class: SynchronisationTesting/TwoStepLinSpec3.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesting/TwoStepLinSpec3.scala


$(DIR)/%.class:     SynchronisationTesting/%.scala
	$(FSC) $<

##### Objects.  Some need SCL

$(OBDIR)/Chan.class: SynchronisationObjects/Chan.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationObjects/Chan.scala

$(OBDIR)/CloseableChan.class: SynchronisationObjects/CloseableChan.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationObjects/CloseableChan.scala

$(OBDIR)/Barrier.class: SynchronisationObjects/Barrier.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationObjects/Barrier.scala

$(OBDIR)/%.class:     SynchronisationObjects/%.scala
	$(FSC) $<

##### Two-step testers

$(EXDIR)/ChanTwoStepTester.class: SynchronisationTesters/ChanTwoStepTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/ChanTwoStepTester.scala

$(EXDIR)/ChanCounterTwoStepTester.class: SynchronisationTesters/ChanCounterTwoStepTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/ChanCounterTwoStepTester.scala

$(EXDIR)/ExchangerTwoStepTester.class: SynchronisationTesters/ExchangerTwoStepTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/ExchangerTwoStepTester.scala

$(EXDIR)/MenWomenTwoStepTester.class: SynchronisationTesters/MenWomenTwoStepTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/MenWomenTwoStepTester.scala

$(EXDIR)/ABCTwoStepTester.class: SynchronisationTesters/ABCTwoStepTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/ABCTwoStepTester.scala

$(EXDIR)/BarrierTwoStepTester.class: SynchronisationTesters/BarrierTwoStepTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/BarrierTwoStepTester.scala

$(EXDIR)/TimeoutExchangerTwoStepTester.class: SynchronisationTesters/TimeoutExchangerTwoStepTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/TimeoutExchangerTwoStepTester.scala

$(EXDIR)/TwoFamiliesTwoStepTester.class: SynchronisationTesters/TwoFamiliesTwoStepTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/TwoFamiliesTwoStepTester.scala

$(EXDIR)/ResignableBarrierTwoStepTester.class: SynchronisationTesters/ResignableBarrierTwoStepTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/ResignableBarrierTwoStepTester.scala

$(EXDIR)/TerminatingQueueTwoStepTester.class: SynchronisationTesters/TerminatingQueueTwoStepTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/TerminatingQueueTwoStepTester.scala

$(EXDIR)/TimeoutChannelTwoStepTester.class: SynchronisationTesters/TimeoutChannelTwoStepTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/TimeoutChannelTwoStepTester.scala

$(EXDIR)/CloseableChannelTwoStepTester.class: SynchronisationTesters/CloseableChannelTwoStepTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/CloseableChannelTwoStepTester.scala

$(EXDIR)/FilterChanTwoStepTester.class: SynchronisationTesters/FilterChanTwoStepTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/FilterChanTwoStepTester.scala

$(EXDIR)/OneFamilyTwoStepTester.class: SynchronisationTesters/OneFamilyTwoStepTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/OneFamilyTwoStepTester.scala

#### Other testers

$(EXDIR)/CloseableChanTester.class: SynchronisationTesters/CloseableChanTester.scala
	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/CloseableChanTester.scala


# $(OBDIR)/ChanTester.class: SynchronisationTesters/ChanTester.scala
# 	fsc -deprecation -cp $(CPSCL) SynchronisationTesters/ChanTester.scala

$(EXDIR)/ChanTester.class:  SynchronisationTesters/ChanTester.scala
	fsc -cp $(CPSCL) SynchronisationTesters/ChanTester.scala

$(EXDIR)/%.class:     SynchronisationTesters/%.scala
	$(FSC) $<

$(EXPDIR)/%.class:	Experiments/%.scala
	$(FSC) $<

%.class:     %.scala
	$FSC(CP) $<

