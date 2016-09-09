MODULE MOD_ANGLE
  DOUBLE PRECISION,SAVE :: pi,rd
END MODULE

!*********************

MODULE MOD_staticvar
  DOUBLE PRECISION,SAVE :: cts,cto,ctscto
  DOUBLE PRECISION,SAVE :: ddb,ddf,dob,dof,dso
  DOUBLE PRECISION,SAVE :: ko,ks,sdb,sdf
  DOUBLE PRECISION,SAVE :: sob,sof,sumint
  DOUBLE PRECISION,ALLOCATABLE,SAVE :: sb(:),sf(:),vb(:),vf(:),w(:)
  DOUBLE PRECISION,ALLOCATABLE,SAVE :: m(:),m2(:),att(:),sigb(:),rinf(:),lidf(:)
END MODULE

!*********************

MODULE MOD_output_PROSPECT
  DOUBLE PRECISION,ALLOCATABLE,SAVE :: LRT(:,:),rho(:),tau(:)
END  MODULE

!*********************

MODULE MOD_flag_util
  LOGICAL flag(7),init_completed,init_completed0
  LOGICAL delta_geom,delta_lai,delta_hot,delta_leaf,delta_skyl,delta_soil,delta_lidf
  DOUBLE PRECISION,ALLOCATABLE,SAVE :: lidf_old(:),rsoil_old(:)
  DOUBLE PRECISION,SAVE :: N_old,Cab_old,Car_old,Cbrown_old,Cw_old,Cm_old
  DOUBLE PRECISION,SAVE :: LIDFa_old,LIDFb_old,lai_old,q_old,skyl_old
  DOUBLE PRECISION,SAVE :: tts_old,tto_old,psi_old
END  MODULE

!*********************

MODULE MOD_SAIL
  DOUBLE PRECISION,SAVE :: tss,too,tsstoo
  DOUBLE PRECISION,ALLOCATABLE,SAVE :: tso(:),tsd(:),tdd(:),tdo(:),rsd(:),rdd(:),rso(:),rdo(:)
  DOUBLE PRECISION,ALLOCATABLE,SAVE :: rsos(:),rsod(:),rddt(:),rsdt(:),rdot(:),rsodt(:),rsost(:),rsot(:)
END MODULE

!*********************