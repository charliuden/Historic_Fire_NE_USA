# November 30 2021
#Step 1 of analysis: Finding best of 5 neighbors to get SSR

###### ****** THIS SCRIPT IS TO BE RUN ON BABBAGE ***** ########

#Read in FIA data:
fia <- read.csv("../data/FIA/FIA_PFTs.csv")[,-1]

#Read in lpj-guess output:

#---
#fire = 0 
#------------------
f_0_grc_0_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-0-fsm-0/lai.out"),fireprob=0.0, distinterval=1, gcr=0, fsm=1)
f_0_grc_1_fsm_0 <- cleanup(lai=read.table("/mnt/netfiles02_nfs/delphi/raccfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-01-fsm-0/lai.out"),fireprob=0.0, distinterval=1, gcr=-0.1, fsm=0)
f_0_grc_5_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-01-fsm-0/lai.out"),fireprob=0.0, distinterval=1, gcr=-0.5, fsm=0)
f_0_grc_9_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-09-fsm-0/lai.out"),fireprob=0.0, distinterval=1, gcr=-0.9, fsm=0)
f_0_grc_0_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-0-fsm-01/lai.out"),fireprob=0.0, distinterval=1, gcr=0, fsm=1.1)
f_0_grc_0_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-0-fsm-05/lai.out"),fireprob=0.0, distinterval=1, gcr=0, fsm=1.5)
f_0_grc_0_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-0-fsm-09/lai.out"),fireprob=0.0, distinterval=1, gcr=0, fsm=1.9)
f_0_grc_1_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-gcrfsm-10/lai.out"),fireprob=0.0, distinterval=1, gcr=-0.1, fsm=1.1)
f_0_grc_1_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-01-fsm-05/lai.out"),fireprob=0.0, distinterval=1, gcr=-0.1, fsm=1.5)
f_0_grc_1_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-01-fsm-09/lai.out"),fireprob=0.0, distinterval=1, gcr=-0.1, fsm=1.9)
f_0_grc_5_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-05-fsm-01/lai.out"),fireprob=0.0, distinterval=1, gcr=-0.5, fsm=1.1)
f_0_grc_5_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-gcrfsm-50/lai.out"),fireprob=0.0, distinterval=1, gcr=-0.5, fsm=1.5)
f_0_grc_5_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-05-fsm-09/lai.out"),fireprob=0.0, distinterval=1, gcr=-0.5, fsm=1.9)
f_0_grc_9_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-09-fsm-01/lai.out"),fireprob=0.0, distinterval=1, gcr=-0.9, fsm=1.1)
f_0_grc_9_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-09-fsm-05/lai.out"),fireprob=0.0, distinterval=1, gcr=-0.9, fsm=1.5)
f_0_grc_9_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-gcrfsm-90/lai.out"),fireprob=0.0, distinterval=1, gcr=-0.9, fsm=1.9)
#------------------

#fire = 0.1
#------------------
f_0.1_grc_0_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-0-fsm-0/lai.out"),fireprob=0.1, distinterval=1, gcr=-0, fsm=1)
f_0.1_grc_1_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-01-fsm-0/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.1, fsm=0)
f_0.1_grc_5_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-05-fsm-0/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.5, fsm=0)
f_0.1_grc_9_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-09-fsm-0/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.9, fsm=0)
f_0.1_grc_0_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-0-fsm-01/lai.out"),fireprob=0.1, distinterval=1, gcr=0, fsm=1.1)
f_0.1_grc_0_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-0-fsm-05/lai.out"),fireprob=0.1, distinterval=1, gcr=0, fsm=1.5)
f_0.1_grc_0_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-0-fsm-09/lai.out"),fireprob=0.1, distinterval=1, gcr=0, fsm=1.9)
f_0.1_grc_1_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-01-fsm-01/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.1, fsm=1.1)
f_0.1_grc_1_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-01-fsm-05/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.1, fsm=1.5)
f_0.1_grc_1_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-01-fsm-09/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.1, fsm=1.9)
f_0.1_grc_5_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-05-fsm-01/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.5, fsm=1.1)
f_0.1_grc_5_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-05-fsm-05/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.5, fsm=1.5)
f_0.1_grc_5_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-05-fsm-09/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.5, fsm=1.9)
f_0.1_grc_9_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-09-fsm-01/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.9, fsm=1.1)
f_0.1_grc_9_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-09-fsm-05/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.9, fsm=1.5)
f_0.1_grc_9_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-09-fsm-09/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.9, fsm=1.9)
#------------------

#fire = 0.02
#------------------
f_0.02_grc_0_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-0-fsm-0/lai.out"),fireprob=0.02, distinterval=1, gcr=-0, fsm=1)
f_0.02_grc_1_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-01-fsm-0/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.1, fsm=0)
f_0.02_grc_5_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-05-fsm-0/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.5, fsm=0)
f_0.02_grc_9_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-09-fsm-0/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.9, fsm=0)
f_0.02_grc_0_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-0-fsm-01/lai.out"),fireprob=0.02, distinterval=1, gcr=0, fsm=1.1)
f_0.02_grc_0_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-0-fsm-05/lai.out"),fireprob=0.02, distinterval=1, gcr=0, fsm=1.5)
f_0.02_grc_0_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-0-fsm-09/lai.out"),fireprob=0.02, distinterval=1, gcr=0, fsm=1.9)
f_0.02_grc_1_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-01-fsm-01/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.1, fsm=1.1)
f_0.02_grc_1_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-01-fsm-05/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.1, fsm=1.5)
f_0.02_grc_1_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-01-fsm-09/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.1, fsm=1.9)
f_0.02_grc_5_fsm_1 <- cleanup(lai=read.table("//epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-05-fsm-01/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.5, fsm=1.1)
f_0.02_grc_5_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-05-fsm-05/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.5, fsm=1.5)
f_0.02_grc_5_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-05-fsm-09/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.5, fsm=1.9)
f_0.02_grc_9_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-09-fsm-01/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.9, fsm=1.1)
f_0.02_grc_9_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-09-fsm-05/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.9, fsm=1.5)
f_0.02_grc_9_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-09-fsm-09/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.9, fsm=1.9)
#------------------

#fire = 0.01
#------------------
f_0.01_grc_0_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-0-fsm-0/lai.out"),fireprob=0.01, distinterval=1, gcr=-0, fsm=1)
f_0.01_grc_1_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-01-fsm-0/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.1, fsm=0)
f_0.01_grc_5_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-05-fsm-0/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.5, fsm=0)
f_0.01_grc_9_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-09-fsm-0/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.9, fsm=0)
f_0.01_grc_0_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-0-fsm-01/lai.out"),fireprob=0.01, distinterval=1, gcr=0, fsm=1.1)
f_0.01_grc_0_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-0-fsm-05/lai.out"),fireprob=0.01, distinterval=1, gcr=0, fsm=1.5)
f_0.01_grc_0_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-0-fsm-09/lai.out"),fireprob=0.01, distinterval=1, gcr=0, fsm=1.9)
f_0.01_grc_1_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-gcrfsm-10/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.1, fsm=1.1)
f_0.01_grc_1_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-01-fsm-05/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.1, fsm=1.5)
f_0.01_grc_1_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-01-fsm-09/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.1, fsm=1.9)
f_0.01_grc_5_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-05-fsm-01/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.5, fsm=1.1)
f_0.01_grc_5_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-05-fsm-05/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.5, fsm=1.5)
f_0.01_grc_5_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-05-fsm-09/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.5, fsm=1.9)
f_0.01_grc_9_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-09-fsm-01/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.9, fsm=1.1)
f_0.01_grc_9_fsm_5 <- cleanup(lai=read.table("//epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-09-fsm-05/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.9, fsm=1.5)
f_0.01_grc_9_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-09-fsm-09/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.9, fsm=1.9)
#------------------

#fire = 0.005
#------------------
f_0.005_grc_0_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-0-fsm-0/lai.out"),fireprob=0.005, distinterval=1, gcr=-0, fsm=1)
f_0.005_grc_1_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-01-fsm-0/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.1, fsm=011)
f_0.005_grc_5_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-05-fsm-0/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.5, fsm=0)
f_0.005_grc_9_fsm_0 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-09-fsm-0/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.9, fsm=0)
f_0.005_grc_0_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-0-fsm-01/lai.out"),fireprob=0.005, distinterval=1, gcr=0, fsm=1.1)
f_0.005_grc_0_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-0-fsm-05/lai.out"),fireprob=0.005, distinterval=1, gcr=0, fsm=1.5)
f_0.005_grc_0_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-0-fsm-09/lai.out"),fireprob=0.005, distinterval=1, gcr=0, fsm=1.9)
f_0.005_grc_1_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-gcrfsm-10/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.1, fsm=1.1)
f_0.005_grc_1_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-01-fsm-05/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.1, fsm=1.5)
f_0.005_grc_1_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-01-fsm-09/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.1, fsm=1.9)
f_0.005_grc_5_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-05-fsm-01/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.5, fsm=1.1)
f_0.005_grc_5_fsm_5 <- cleanup(lai=read.table("//epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-05-fsm-05/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.5, fsm=1.5)
f_0.005_grc_5_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-05-fsm-09/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.5, fsm=1.9)
f_0.005_grc_9_fsm_1 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-09-fsm-01/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.9, fsm=1.1)
f_0.005_grc_9_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-09-fsm-05/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.9, fsm=1.5)
f_0.005_grc_9_fsm_9 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-09-fsm-09/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.9, fsm=1.9)
#------------------

#------------------
#oct 18 tried values flanking grc -0.5 adn fsm 1.5
#------------------
f_0_grc_6_fsm_6 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-06-fsm-06/lai.out"),fireprob=0, distinterval=1, gcr=-0.6, fsm=1.6)
f_0.1_grc_6_fsm_6 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-06-fsm-06/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.6, fsm=1.6)
f_0.01_grc_6_fsm_6 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-06-fsm-06/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.6, fsm=1.6)
f_0.02_grc_6_fsm_6 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-06-fsm-06/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.6, fsm=1.6)
f_0.005_grc_6_fsm_6 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-06-fsm-06/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.6, fsm=1.6)

f_0_grc_4_fsm_4 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-04-fsm-04/lai.out"),fireprob=0, distinterval=1, gcr=-0.4, fsm=1.4)
f_0.1_grc_4_fsm_4 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-04-fsm-04/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.4, fsm=1.4)
f_0.01_grc_4_fsm_4 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-04-fsm-04/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.4, fsm=1.4)
f_0.02_grc_4_fsm_4 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-04-fsm-04/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.4, fsm=1.4)
f_0.005_grc_4_fsm_4 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-04-fsm-04/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.4, fsm=1.4)

f_0_grc_7_fsm_7 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-07-fsm-07/lai.out"),fireprob=0, distinterval=1, gcr=-0.7, fsm=1.7)
f_0.1_grc_7_fsm_7 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-07-fsm-07/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.7, fsm=1.7)
f_0.01_grc_7_fsm_7 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-07-fsm-07/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.7, fsm=1.7)
f_0.02_grc_7_fsm_7 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-07-fsm-07/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.7, fsm=1.7)
f_0.005_grc_7_fsm_7 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-07-fsm-07/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.7, fsm=1.7)

f_0_grc_8_fsm_8 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-08-fsm-08/lai.out"),fireprob=0, distinterval=1, gcr=-0.8, fsm=1.8)
f_0.1_grc_8_fsm_8 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-08-fsm-08/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.8, fsm=1.8)
f_0.01_grc_8_fsm_8 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-08-fsm-08/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.8, fsm=1.8)
f_0.02_grc_8_fsm_8 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-08-fsm-08/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.8, fsm=1.8)
f_0.005_grc_8_fsm_8 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-08-fsm-08/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.8, fsm=1.8)

f_0_grc_6_fsm_7 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-06-fsm-07/lai.out"),fireprob=0, distinterval=1, gcr=-0.6, fsm=1.7)
f_0.1_grc_6_fsm_7 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-06-fsm-07/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.6, fsm=1.7)
f_0.01_grc_6_fsm_7 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-06-fsm-07/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.6, fsm=1.7)
f_0.02_grc_6_fsm_7 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-06-fsm-07/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.6, fsm=1.7)
f_0.005_grc_6_fsm_7 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-06-fsm-07/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.6, fsm=1.7)

f_0_grc_6_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-06-fsm-05/lai.out"),fireprob=0, distinterval=1, gcr=-0.6, fsm=1.5)
f_0.1_grc_6_fsm_5<- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-06-fsm-05/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.6, fsm=1.5)
f_0.01_grc_6_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-06-fsm-05/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.6, fsm=1.5)
f_0.02_grc_6_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-06-fsm-05/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.6, fsm=1.5)
f_0.005_grc_6_fsm_5 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-06-fsm-05/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.6, fsm=1.5)

f_0_grc_7_fsm_6 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-07-fsm-06/lai.out"),fireprob=0, distinterval=1, gcr=-0.7, fsm=1.6)
f_0.1_grc_7_fsm_6 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-07-fsm-06/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.7, fsm=1.6)
f_0.01_grc_7_fsm_6 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-07-fsm-06/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.7, fsm=1.6)
f_0.02_grc_7_fsm_6 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-07-fsm-06/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.7, fsm=1.6)
f_0.005_grc_7_fsm_6 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-07-fsm-06/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.7, fsm=1.6)

f_0_grc_5_fsm_6 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-singledist-grc-05-fsm-06/lai.out"),fireprob=0, distinterval=1, gcr=-0.5, fsm=1.6)
f_0.1_grc_5_fsm_6 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-1-singledist-grc-05-fsm-06/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.5, fsm=1.6)
f_0.01_grc_5_fsm_6 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-01-singledist-grc-05-fsm-06/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.5, fsm=1.6)
f_0.02_grc_5_fsm_6 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-02-singledist-grc-05-fsm-06/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.5, fsm=1.6)
f_0.005_grc_5_fsm_6 <- cleanup(lai=read.table("/epscorfs/cuden/GRC_FSM_sensitivity/fire0-005-singledist-grc-05-fsm-06/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.5, fsm=1.6)



x1.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_0_fsm_0, neighbors=5))
x2.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_0_fsm_0, neighbors=5))
x3.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_0_fsm_0, neighbors=5))
x4.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_0_fsm_0, neighbors=5))
x5.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_0_fsm_0, neighbors=5))

x6.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_1_fsm_0, neighbors=5))
x7.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_1_fsm_0, neighbors=5))
x8.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_1_fsm_0, neighbors=5))
x9.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_1_fsm_0, neighbors=5))
x10.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_1_fsm_0, neighbors=5))

x11.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_5_fsm_0, neighbors=5))
x12.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_5_fsm_0, neighbors=5))
x13.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_5_fsm_0, neighbors=5))
x14.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_5_fsm_0, neighbors=5))
x15.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_5_fsm_0, neighbors=5))

x16.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_9_fsm_0, neighbors=5))
x17.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_9_fsm_0, neighbors=5))
x18.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_9_fsm_0, neighbors=5))
x19.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_9_fsm_0, neighbors=5))
x20.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_9_fsm_0, neighbors=5))

x21.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_0_fsm_1, neighbors=5))
x22.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_0_fsm_1, neighbors=5))
x23.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_0_fsm_1, neighbors=5))
x24.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_0_fsm_1, neighbors=5))
x25.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_0_fsm_1, neighbors=5))

x26.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_1_fsm_1, neighbors=5))
x27.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_1_fsm_1, neighbors=5))
x28.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_1_fsm_1, neighbors=5))
x29.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_1_fsm_1, neighbors=5))
x30.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_1_fsm_1, neighbors=5))

x31.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_5_fsm_1, neighbors=5))
x32.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_5_fsm_1, neighbors=5))
x33.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_5_fsm_1, neighbors=5))
x34.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_5_fsm_1, neighbors=5))
x35.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_5_fsm_1, neighbors=5))

x36.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_9_fsm_1, neighbors=5))
x37.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_9_fsm_1, neighbors=5))
x38.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_9_fsm_1, neighbors=5))
x39.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_9_fsm_1, neighbors=5))
x40.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_9_fsm_1, neighbors=5))

x41.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_0_fsm_5, neighbors=5))
x42.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_0_fsm_5, neighbors=5))
x43.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_0_fsm_5, neighbors=5))
x44.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_0_fsm_5, neighbors=5))
x45.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_0_fsm_5, neighbors=5))

x46.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_1_fsm_5, neighbors=5))
x47.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_1_fsm_5, neighbors=5))
x48.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_1_fsm_5, neighbors=5))
x49.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_1_fsm_5, neighbors=5))
x50.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_1_fsm_5, neighbors=5))

x51.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_5_fsm_5, neighbors=5))
x52.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_5_fsm_5, neighbors=5))
x53.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_5_fsm_5, neighbors=5))
x54.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_5_fsm_5, neighbors=5))
x55.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_5_fsm_5, neighbors=5))

x56.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_9_fsm_5, neighbors=5))
x57.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_9_fsm_5, neighbors=5))
x58.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_9_fsm_5, neighbors=5))
x59.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_9_fsm_5, neighbors=5))
x60.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_9_fsm_5, neighbors=5))

x61.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_0_fsm_9, neighbors=5))
x62.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_0_fsm_9, neighbors=5))
x63.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_0_fsm_9, neighbors=5))
x64.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_0_fsm_9, neighbors=5))
x65.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_0_fsm_9, neighbors=5))

x66.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_1_fsm_9, neighbors=5))
x67.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_1_fsm_9, neighbors=5))
x68.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_1_fsm_9, neighbors=5))
x69.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_1_fsm_9, neighbors=5))
x70.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_1_fsm_9, neighbors=5))

x71.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_5_fsm_9, neighbors=5))
x72.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_5_fsm_9, neighbors=5))
x73.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_5_fsm_9, neighbors=5))
x74.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_5_fsm_9, neighbors=5))
x75.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_5_fsm_9, neighbors=5))

x76.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_9_fsm_9, neighbors=5))
x77.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_9_fsm_9, neighbors=5))
x78.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_9_fsm_9, neighbors=5))
x79.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_9_fsm_9, neighbors=5))
x80.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_9_fsm_9, neighbors=5))

x81.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_6_fsm_6, neighbors=5))
x82.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_6_fsm_6, neighbors=5))
x83.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_6_fsm_6, neighbors=5))
x84.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_6_fsm_6, neighbors=5))
x85.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_6_fsm_6, neighbors=5))

x86.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_4_fsm_4, neighbors=5))
x87.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_4_fsm_4, neighbors=5))
x88.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_4_fsm_4, neighbors=5))
x89.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_4_fsm_4, neighbors=5))
x90.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_4_fsm_4, neighbors=5))

x91.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_7_fsm_7, neighbors=5))
x92.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_7_fsm_7, neighbors=5))
x93.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_7_fsm_7, neighbors=5))
x94.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_7_fsm_7, neighbors=5))
x95.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_7_fsm_7, neighbors=5))

x96.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_8_fsm_8, neighbors=5))
x97.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_8_fsm_8, neighbors=5))
x98.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_8_fsm_8, neighbors=5))
x99.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_8_fsm_8, neighbors=5))
x100.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_8_fsm_8, neighbors=5))

x101.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_6_fsm_7, neighbors=5))
x102.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_6_fsm_7, neighbors=5))
x103.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_6_fsm_7, neighbors=5))
x104.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_6_fsm_7, neighbors=5))
x105.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_6_fsm_7, neighbors=5))

x106.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_6_fsm_5, neighbors=5))
x107.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_6_fsm_5, neighbors=5))
x108.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_6_fsm_5, neighbors=5))
x109.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_6_fsm_5, neighbors=5))
x110.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_6_fsm_5, neighbors=5))

x111.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_7_fsm_6, neighbors=5))
x112.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_7_fsm_6, neighbors=5))
x113.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_7_fsm_6, neighbors=5))
x114.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_7_fsm_6, neighbors=5))
x115.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_7_fsm_6, neighbors=5))

x116.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0_grc_5_fsm_6, neighbors=5))
x117.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.1_grc_5_fsm_6, neighbors=5))
x118.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.01_grc_5_fsm_6, neighbors=5))
x119.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.02_grc_5_fsm_6, neighbors=5))
x120.1 = get_SSR_PFT(residuals=residuals_best_neighbor_in_neighborhood(obs=fia_pft, pred=f_0.005_grc_5_fsm_6, neighbors=5))



ssr <- rbind(x1.1, x2.1, x3.1, x4.1, x5.1, x6.1, x7.1, x8.1, x9.1, x10.1, x11.1, x12.1, x13.1, x14.1, x15.1, x16.1, x17.1, x18.1, x19.1, x20.1, x21.1, x22.1, x23.1, x24.1, x25.1, x26.1, x27.1, x28.1, x29.1, x30.1, x31.1, x32.1, x33.1, x34.1, x35.1, x36.1, x37.1, x38.1, x39.1, x40.1, x41.1, x42.1, x43.1, x44.1, x45.1, x46.1, x47.1, x48.1, x49.1, x50.1, x51.1, x52.1, x53.1, x54.1, x55.1, x56.1, x57.1, x58.1, x59.1, x60.1, x61.1, x62.1, x63.1, x64.1, x65.1, x66.1, x67.1, x68.1, x69.1, x70.1, x71.1, x72.1, x73.1, x74.1, x75.1, x76.1, x77.1, x78.1, x79.1, x80.1, x81.1, x82.1, x83.1, x84.1, x85.1, x86.1, x87.1, x88.1, x89.1, x90.1, x91.1, x92.1, x93.1, x94.1, x95.1, x96.1, x97.1, x98.1, x99.1, x100.1, x101.1, x102.1, x103.1, x104.1, x105.1, x106.1, x107.1, x108.1, x109.1, x110.1, x111.1, x112.1, x113.1, x114.1, x115.1, x116.1, x117.1, x118.1, x119.1, x120.1)


str(SSR_df_best_neighbor)

p3 <- ggplot() + geom_point(data=SSR_df_best_neighbor, aes(x=fire_survival_mod, y=all_PFT, col=fire_prob)) + ylab("squared sum of residuals") + ggtitle("best neighbor in neighborhood")+ scale_x_continuous(breaks=seq(from=1, to=2, by=0.1)) + guides(size="none")

p4 <- ggplot() + geom_point(data=SSR_df_best_neighbor, aes(x=growth_resp_cost, y=all_PFT, col=fire_prob)) + ylab("squared sum of residuals") + ggtitle("best neighbor in neighborhood")+ scale_x_continuous(breaks=seq(from=0, to=1, by=0.1)) + guides(size="none")



