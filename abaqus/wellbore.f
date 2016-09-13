c 
C
C
      SUBROUTINE SIGINI(SIGMA,COORDS,NTENS,NCRDS,NOEL,NPT,LAYER,
     1 KSPT,LREBAR,NAMES)
C
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION SIGMA(NTENS),COORDS(NCRDS)
      CHARACTER NAMES(2)*80
C
      D=coords(2)
      h=-D
c 
      If (NAMES(2) =='CAX8P') then
        ratio=0.440116284
        porep = ((1000*h*9.81)/1000)+10100
        Total=((1750*h*10)/1000)+10100
            sigma(2) = -(Total - porep)
            sigma(1) = sigma(2) * ratio
            sigma(3) = sigma(2) * ratio
      end if  
      If (NAMES(2) =='CAX8') then
      ratio = 1.0
      IF(COORDS(1)>0.1122.and.COORDS(1)<0.122238.and.
     1 COORDS(2)>-97)THEN
                 Total=((8000*h*9.81)/1000)+10100
            sigma(2) =-(Total)
            sigma(1) = sigma(2)*ratio
            sigma(3) = sigma(2)*ratio
       else
            Total=((1240*h*9.81)/1000)+10100
            sigma(2) = -(Total)
            sigma(1) = sigma(2)*ratio
            sigma(3) = sigma(2)*ratio
      End if                         
      End if
C
      return
      end
C
C
       SUBROUTINE UFIELD(FIELD,KFIELD,NSECPT,KSTEP,KINC,TIME,NODE,      
     1  COORDS,TEMP,DTEMP)                  
        INCLUDE 'ABA_PARAM.INC'                                         
        DIMENSION FIELD(NSECPT),TIME(2),COORDS(3),TEMP(NSECPT),         
     1  DTEMP(NSECPT)                                          
        KFIELD=1                                                        
        FIELD(1)=-COORDS(2)                                           
        RETURN                                   
        END   
C
      subroutine uporep(uw0,coords,node)
C
      include 'aba_param.inc'
C
      dimension coords(3)
      COMMON /READ/ IRDR7
      COMMON /GRID/ XR7(897),YR7(897),SATR7(897)
C
      D=coords(2)
      h=-D
      uw0 =((1000* h*9.81)/1000)+10100
C
      return
      end
C
C
      subroutine disp(u,kstep,kinc,time,node,noel,jdof,coords)
c
      include 'aba_param.inc'
c
C      common/kycommonT/HWT
      common/Kycommon/HW8,HWP
      dimension u(3),time(2),coords(3)
           IF (kstep<7)then
      D=-coords(2)
      U(1) =((1000* D*9.81)/1000)+10100
      END IF
      h=-coords(2)
      IF (kstep==7)then
      IF(COORDS(2)>-150.and.COORDS(2)<-99.9)THEN
         if (COORDS(1)<3) then
          U(1) =((1000* h*9.81)/1000)+10000-1000*time(1)
                         else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
               else
         U(1) =((1000* h*9.81)/1000)+10100
      end if
      end if
      IF (kstep==8)then
      IF(COORDS(2)>-150.and.COORDS(2)<-99.9)THEN
         if (COORDS(1)<3) then
          U(1) =((1000* h*9.81)/1000)+9000-1000*time(1)
         else if (COORDS(1)>3.and.COORDS(1)<6) then
          U(1) =((1000* h*9.81)/1000)+10000-1000*time(1)
          else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
                  else
         U(1) =((1000* h*9.81)/1000)+10100
      end if
      end if
      IF (kstep==9)then
      IF(COORDS(2)>-150.and.COORDS(2)<-99.9)THEN
         if (COORDS(1)<3) then
          U(1) =((1000* h*9.81)/1000)+8000-1000*time(1)
          else if(COORDS(1)>3.and.COORDS(1)<6) then
          U(1) =((1000* h*9.81)/1000)+9000-1000*time(1)
          else if (COORDS(1)>6.and.COORDS(1)<9) then
          U(1) =((1000* h*9.81)/1000)+10000-1000*time(1)
          else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
                  else
         U(1) =((1000* h*9.81)/1000)+10100
      end if
      end if
      IF (kstep==10)then
      IF(COORDS(2)>-150.and.COORDS(2)<-99.9)THEN
         if (COORDS(1)<3) then
          U(1) =((1000* h*9.81)/1000)+7000-1000*time(1)
          else if (COORDS(1)>3.and.COORDS(1)<6) then
          U(1) =((1000* h*9.81)/1000)+8000-1000*time(1)
          else if (COORDS(1)>6.and.COORDS(1)<9) then
          U(1) =((1000* h*9.81)/1000)+9000-1000*time(1)
          else if (COORDS(1)>9.and.COORDS(1)<12) then
          U(1) =((1000* h*9.81)/1000)+10000-1000*time(1)
          else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
                  else
         U(1) =((1000* h*9.81)/1000)+10100
      end if
      end if
      IF (kstep==11)then
      IF(COORDS(2)>-150.and.COORDS(2)<-99.9)THEN
         if (COORDS(1)<3) then
          U(1) =((1000* h*9.81)/1000)+6000-500*time(1)
         else if (COORDS(1)>3.and.COORDS(1)<6) then
          U(1) =((1000* h*9.81)/1000)+7000-500*time(1)
         else if(COORDS(1)>6.and.COORDS(1)<9) then
          U(1) =((1000* h*9.81)/1000)+8000-500*time(1)
         else if(COORDS(1)>9.and.COORDS(1)<12) then
          U(1) =((1000* h*9.81)/1000)+9000-500*time(1)
         else if(COORDS(1)>12.and.COORDS(1)<15) then
          U(1) =((1000* h*9.81)/1000)+10000-500*time(1)
         else
         U(1) =((1000* h*9.81)/1000)+10100
      end if
                        else
         U(1) =((1000* h*9.81)/1000)+10100
      end if
      end if
      IF (kstep==12)then
      IF(COORDS(2)>-150.and.COORDS(2)<-99.9)THEN
         if (COORDS(1)<3) then
          U(1) =((1000* h*9.81)/1000)+5500-500*time(1)
         else if (COORDS(1)>3.and.COORDS(1)<6) then
          U(1) =((1000* h*9.81)/1000)+6500-500*time(1)
         else if(COORDS(1)>6.and.COORDS(1)<9) then
          U(1) =((1000* h*9.81)/1000)+7500-500*time(1)
         else if(COORDS(1)>9.and.COORDS(1)<12) then
          U(1) =((1000* h*9.81)/1000)+8500-500*time(1)
         else if(COORDS(1)>12.and.COORDS(1)<15) then
          U(1) =((1000* h*9.81)/1000)+9500-500*time(1)
         else
         U(1) =((1000* h*9.81)/1000)+10100
      end if
                        else
         U(1) =((1000* h*9.81)/1000)+10100
      end if
      end if
       IF (kstep==13)then
       IF(COORDS(2)>-150.and.COORDS(2)<-99.9)THEN
         if (COORDS(1)<3) then
          U(1) =((1000* h*9.81)/1000)+5000-500*time(1)
         else if (COORDS(1)>3.and.COORDS(1)<6) then
          U(1) =((1000* h*9.81)/1000)+6000-500*time(1)
         else if (COORDS(1)>6.and.COORDS(1)<9) then
          U(1) =((1000* h*9.81)/1000)+7000-500*time(1)
         else if (COORDS(1)>9.and.COORDS(1)<12) then
          U(1) =((1000* h*9.81)/1000)+8000-500*time(1)
         else if (COORDS(1)>12.and.COORDS(1)<15) then
          U(1) =((1000* h*9.81)/1000)+9000-500*time(1)
         else if (COORDS(1)>15.and.COORDS(1)<18) then
          U(1) =((1000* h*9.81)/1000)+10000-500*time(1)
         else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
                  else
         U(1) =((1000* h*9.81)/1000)+10100
      end if
      end if
      IF (kstep==14)then
      IF(COORDS(2)>-150.and.COORDS(2)<-99.9)THEN
         if (COORDS(1)<3) then
          U(1) =((1000* h*9.81)/1000)+4500-500*time(1)
         else if (COORDS(1)>3.and.COORDS(1)<6) then
          U(1) =((1000* h*9.81)/1000)+5500-500*time(1)
         else if (COORDS(1)>6.and.COORDS(1)<9) then
          U(1) =((1000* h*9.81)/1000)+6500-500*time(1)
         else if (COORDS(1)>9.and.COORDS(1)<12) then
          U(1) =((1000* h*9.81)/1000)+7500-500*time(1)
         else if (COORDS(1)>12.and.COORDS(1)<15) then
          U(1) =((1000* h*9.81)/1000)+8500-500*time(1)
         else if (COORDS(1)>15.and.COORDS(1)<18) then
          U(1) =((1000* h*9.81)/1000)+9500-500*time(1)
         else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
                  else
         U(1) =((1000* h*9.81)/1000)+10100
      end if
      end if
      IF (kstep==15)then
      IF(COORDS(2)>-150.and.COORDS(2)<-99.9)THEN
         if (COORDS(1)<3) then
          U(1) =((1000* h*9.81)/1000)+4000-500*time(1)
         else if (COORDS(1)>3.and.COORDS(1)<6) then
          U(1) =((1000* h*9.81)/1000)+5000-500*time(1)
         else if (COORDS(1)>6.and.COORDS(1)<9) then
          U(1) =((1000* h*9.81)/1000)+6000-500*time(1)
          else if (COORDS(1)>9.and.COORDS(1)<12) then
          U(1) =((1000* h*9.81)/1000)+7000-500*time(1)
         else if (COORDS(1)>12.and.COORDS(1)<15) then
          U(1) =((1000* h*9.81)/1000)+8000-500*time(1)
        else if (COORDS(1)>15.and.COORDS(1)<18) then
          U(1) =((1000* h*9.81)/1000)+9000-500*time(1)
         else if (COORDS(1)>18.and.COORDS(1)<21) then
          U(1) =((1000* h*9.81)/1000)+10000-500*time(1)
         else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
                  else
         U(1) =((1000* h*9.81)/1000)+10100
      end if
      end if
      IF (kstep==16)then
      IF(COORDS(2)>-150.and.COORDS(2)<-99.9)THEN
         if (COORDS(1)<3) then
          U(1) =((1000* h*9.81)/1000)+3500-500*time(1)
         else if (COORDS(1)>3.and.COORDS(1)<6) then
          U(1) =((1000* h*9.81)/1000)+4500-500*time(1)
         else if (COORDS(1)>6.and.COORDS(1)<9) then
          U(1) =((1000* h*9.81)/1000)+5500-500*time(1)
          else if (COORDS(1)>9.and.COORDS(1)<12) then
          U(1) =((1000* h*9.81)/1000)+6500-500*time(1)
         else if (COORDS(1)>12.and.COORDS(1)<15) then
          U(1) =((1000* h*9.81)/1000)+7500-500*time(1)
        else if (COORDS(1)>15.and.COORDS(1)<18) then
          U(1) =((1000* h*9.81)/1000)+8500-500*time(1)
         else if (COORDS(1)>18.and.COORDS(1)<21) then
          U(1) =((1000* h*9.81)/1000)+9500-500*time(1)
          else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
                  else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
      end if
            IF (kstep==17)then
      IF(COORDS(2)>-150.and.COORDS(2)<-99.9)THEN
         if (COORDS(1)<3) then
          U(1) =((1000* h*9.81)/1000)+3000-500*time(1)
         else if (COORDS(1)>3.and.COORDS(1)<6) then
          U(1) =((1000* h*9.81)/1000)+4000-500*time(1)
        else if (COORDS(1)>6.and.COORDS(1)<9) then
          U(1) =((1000* h*9.81)/1000)+5000-500*time(1)
         else if (COORDS(1)>9.and.COORDS(1)<12) then
          U(1) =((1000* h*9.81)/1000)+6000-500*time(1)
         else if (COORDS(1)>12.and.COORDS(1)<15) then
          U(1) =((1000* h*9.81)/1000)+7000-500*time(1)
         else if (COORDS(1)>15.and.COORDS(1)<18) then
          U(1) =((1000* h*9.81)/1000)+8000-500*time(1)
         else if (COORDS(1)>18.and.COORDS(1)<21) then
          U(1) =((1000* h*9.81)/1000)+9000-500*time(1)
         else if (COORDS(1)>21.and.COORDS(1)<24) then
          U(1) =((1000* h*9.81)/1000)+10000-500*time(1)
          else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
                  else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
      end if
                  IF (kstep==18)then
      IF(COORDS(2)>-150.and.COORDS(2)<-99.9)THEN
         if (COORDS(1)<3) then
          U(1) =((1000* h*9.81)/1000)+2500-500*time(1)
         else if (COORDS(1)>3.and.COORDS(1)<6) then
          U(1) =((1000* h*9.81)/1000)+3500-500*time(1)
        else if (COORDS(1)>6.and.COORDS(1)<9) then
          U(1) =((1000* h*9.81)/1000)+4500-500*time(1)
         else if (COORDS(1)>9.and.COORDS(1)<12) then
          U(1) =((1000* h*9.81)/1000)+5500-500*time(1)
         else if (COORDS(1)>12.and.COORDS(1)<15) then
          U(1) =((1000* h*9.81)/1000)+6500-500*time(1)
         else if (COORDS(1)>15.and.COORDS(1)<18) then
          U(1) =((1000* h*9.81)/1000)+7500-500*time(1)
         else if (COORDS(1)>18.and.COORDS(1)<21) then
          U(1) =((1000* h*9.81)/1000)+8500-500*time(1)
         else if (COORDS(1)>21.and.COORDS(1)<24) then
          U(1) =((1000* h*9.81)/1000)+9500-500*time(1)
          else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
                  else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
      end if
      IF (kstep==19)then
      IF(COORDS(2)>-150.and.COORDS(2)<-99.9)THEN
         if (COORDS(1)<3) then
          U(1) =((1000* h*9.81)/1000)+2000-500*time(1)
         else if (COORDS(1)>3.and.COORDS(1)<6) then
          U(1) =((1000* h*9.81)/1000)+3000-500*time(1)
        else if (COORDS(1)>6.and.COORDS(1)<9) then
          U(1) =((1000* h*9.81)/1000)+4000-500*time(1)
         else if (COORDS(1)>9.and.COORDS(1)<12) then
          U(1) =((1000* h*9.81)/1000)+5000-500*time(1)
         else if (COORDS(1)>12.and.COORDS(1)<15) then
          U(1) =((1000* h*9.81)/1000)+6000-500*time(1)
         else if (COORDS(1)>15.and.COORDS(1)<18) then
          U(1) =((1000* h*9.81)/1000)+7000-500*time(1)
         else if (COORDS(1)>18.and.COORDS(1)<21) then
          U(1) =((1000* h*9.81)/1000)+8000-500*time(1)
         else if (COORDS(1)>21.and.COORDS(1)<24) then
          U(1) =((1000* h*9.81)/1000)+9000-500*time(1)
          else if (COORDS(1)>24.and.COORDS(1)<27) then
          U(1) =((1000* h*9.81)/1000)+10000-500*time(1)
          else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
                  else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
      end if
            IF (kstep==20)then
      IF(COORDS(2)>-150.and.COORDS(2)<-99.9)THEN
         if (COORDS(1)<3) then
          U(1) =((1000* h*9.81)/1000)+1500-500*time(1)
         else if (COORDS(1)>3.and.COORDS(1)<6) then
          U(1) =((1000* h*9.81)/1000)+2500-500*time(1)
        else if (COORDS(1)>6.and.COORDS(1)<9) then
          U(1) =((1000* h*9.81)/1000)+3500-500*time(1)
         else if (COORDS(1)>9.and.COORDS(1)<12) then
          U(1) =((1000* h*9.81)/1000)+4500-500*time(1)
         else if (COORDS(1)>12.and.COORDS(1)<15) then
          U(1) =((1000* h*9.81)/1000)+5500-500*time(1)
         else if (COORDS(1)>15.and.COORDS(1)<18) then
          U(1) =((1000* h*9.81)/1000)+6500-500*time(1)
         else if (COORDS(1)>18.and.COORDS(1)<21) then
          U(1) =((1000* h*9.81)/1000)+7500-500*time(1)
         else if (COORDS(1)>21.and.COORDS(1)<24) then
          U(1) =((1000* h*9.81)/1000)+8500-500*time(1)
          else if (COORDS(1)>24.and.COORDS(1)<27) then
          U(1) =((1000* h*9.81)/1000)+9500-500*time(1)
          else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
                  else
         U(1) =((1000* h*9.81)/1000)+10100
         end if
      end if
      	RETURN
	END 
c
c
	SUBROUTINE UVARM(UVAR,DIRECT,T,TIME,DTIME,
     +CMNAME,ORNAME,NUVARM,NOEL,NPT,LAYER,KSPT,
     +KSTEP,KINC,NDI,NSHR,COORD,JMAC,JMATYP,
     +MATLAYO,LACCFLA)
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME,ORNAME
      CHARACTER*3 FLGRAY(15)
      DIMENSION UVAR(NUVARM),DIRECT(3,3),
     +T(3,3),TIME(2)
      DIMENSION ARRAY(15),JARRAY(15),
     +JMAC(*),JMATYP(*),COORD(*)
C
	CALL GETVRM('E',ARRAY,JARRAY,FLGRAY,JRCD,
     +JMAC,JMATYP,MATLAYO,LACCFLA)
C
C
C	
      E11 = ARRAY(1)
      E22 = ARRAY(2)
      E33 = ARRAY(3)
C
      UVAR(1)=E11
      UVAR(2)=E22
      UVAR(3)=E33
      UVAR(4)=E11+E22+E33
C
	RETURN
	END  
C 
c     SDVINI code for Modified Cam Clay
      subroutine sdvini(statev,coords,nstatv,ncrds,noel,npt,layer,kspt)
      include 'aba_param.inc'
      dimension statev(nstatv),coords(ncrds),YR(336), SAT(336)
c
            YO=-COORDS(2)
      statev(1)=0.0       ! initial plastic volumetric strain
      statev(2)=0.54   ! initial void ratio
      If (YO<-10) then
      statev(9)=750
      else 
      statev(9)=75*YO
      end if
      statev(3)=1         ! degragation factor
      statev(4)=0         ! initial plastic deviator strain
      statev(89)=0
      statev(90)=0
      statev(91)=0
      statev(92)=0
      statev(93)=0
      statev(94)=0
      statev(112)=0
c     
      return
      end
c   
cc     UMAT code for Cambridge Methane Hydrate Critical Model
      subroutine umat(stress,statev,DDSDDE,sse,spd,scd,
     1 rpl,ddsddt,drplde,drpldt,
     2 stran,dstran,time,dtime,temp,dtemp,predef,dpred,cmname,
     3 ndi,nshr,ntens,nstatev,props,nprops,coords,drot,pnewdt,
     4 celent,dfgrd0,dfgrd1,noel,npt,layer,kspt,kstep,kinc)
      include 'aba_param.inc'
      character*80 cmname
      dimension stress(ntens),statev(nstatev),
     1 DDSDDE(ntens,ntens),ddsddt(ntens),drplde(ntens),
     2 stran(ntens),dstran(ntens),time(2),predef(1),dpred(1),
     3 props(nprops),coords(3),drot(3,3),dfgrd0(3,3),dfgrd1(3,3)
c      
c     define internal variables as double precision
      double precision stress1(4,1),strain1(4,1),dstress(4),DeINV(4,4),
     1 dstress1(4,1),dstrain1(4,1),Dsp1(4,1),DShe(4,1),Dsp2(4,1),
     2 dFds(4,1),dFdst(1,4),dlamda0(1,4),dlamda1(1,4),Dp11(4,1),
     3 DHSDDE(4,1),Dp12(4,4),Dp1(4,4),Dp(4,4),ee(4,1),Dhe(4,4),
     4 De(4,4),Dsp(4,1),dstress2(4,1),Dsp22(4,1),DShe1(4,1),Dsp3(4,1),
     5 stress2(4,1)
c    
c    
c     Initialise local variables
      dlamda5=0.0
      dlamda2=0.0
      dlamda3=0.0
      dlamda4=0.0
      do i=1,4
          stress1(i,1)=0.0
          strain1(i,1)=0.0
          dstress(i)=0.0
          dstress1(i,1)=0.0
          dstress2(i,1)=0.0
          dstrain1(i,1)=0.0
          dFds(i,1)=0.0
          dFdst(1,i)=0.0
          dlamda0(1,i)=0.0
          dlamda1(1,i)=0.0
          Dp11(i,1)=0.0
          ee(i,1)=0.0
          DShe(i,1)=0.0
          DShe1(i,1)=0.0
          DHSDDE(i,1)=0.0
          Dsp(i,1)=0.0
          Dsp1(i,1)=0.0
          Dsp2(i,1)=0.0
          Dsp3(i,1)=0.0
          Dsp22(i,1)=0.0
          stress2(i,1)=0.0
      end do      
      do i=1,4
          do j=1,4
              De(i,j)=0.0
              Dp12(i,j)=0.0
              Dp1(i,j)=0.0
              Dp(i,j)=0.0
              Dhe(i,j)=0.0
              DeINV(i,j)=0.0
          end do
      end do
c     
      ndim=3
      if (ntens==4) then
          ndim=2
      end if
c  
c     Get stress tensor from abaqus (change sign)
      D=coords(2)
      h=-D
       ratio=0.440116284
      porep = ((1000*h*9.81)/1000)+10100
      Total=((1750*h*10)/1000)+10100
      VstressO = (Total - porep)
      HstressO = VstressO * ratio
      do i=1,ntens
          stress1(i,1)=-stress(i)
      end do
          stress2(1,1)=stress1(1,1)-VstressO
          stress2(2,1)=stress1(2,1)-HstressO
          stress2(3,1)=stress1(3,1)-HstressO
          stress2(4,1)=stress1(4,1)
c
c     Get strain tensor from abaqus (change sign)
      do i=1,ntens
          strain1(i,1)=-stran(i)
      end do
c
c     Get strain increment tensor from abaqus (change sign)
      do i=1,ntens
          dstrain1(i,1)=-dstran(i)
      end do  
c      
c     Calculate volmetric strain and deviatoric strain
      ev=strain1(1,1)+strain1(2,1)+strain1(3,1)
      ed=sqrt(2.0/3.0)*sqrt((strain1(1,1)-ev/3.0)**2+
     1 (strain1(2,1)-ev/3.0)**2+(strain1(3,1)-ev/3.0)**2+
     2 strain1(4,1)**2)
c 
      IDBG=1
      KST=1
      IF(IDBG.EQ.1)WRITE(6,900)KST
900   FORMAT(1X,'***KST',i5)
c         
c     Define material constants
      cil     =   props(1)    ! compression index
      sik     =   props(2)    ! swelling index
      rM      =   props(3)    ! stress ratio eta at the critical state
       u      =   props(4)    ! parameter represents the development of plastic strain
      pnyu    =   props(5)    ! Poisson's ratio of soil
      rM2     =   props(6)    ! parametre give the degree of increase in shear modulus
      eM      =   props(7)    ! mechanical degradation factor upon shearing
       a      =   props(8)    ! parameter for dilation
       b      =   props(9)    ! parameter for dilation
       c      =   props(10)   ! parameter for cohesion
       d      =   props(11)   ! parameter for cohesion
c       Sh     =   props(12)
c   
c     Get plastic volumetric strain
      epv=statev(1)
c
c     Get plastic deviator strain
      epd=statev(4)
c
c     Get void ratio
      void=statev(2)
c
c     Get preconsolidation pressure
      p1=statev(9)
c     
c     Get degradation factor 
      X=statev(3)
c
c     Get plastic strain 
      ep1=statev(89)
      ep2=statev(90)
      ep3=statev(91)
      ep4=statev(92)
      ep5=statev(93)
      ep6=statev(94)      
c
c     
c
c     Get hydrate saturation
      Sh=statev(112)
c     Get the enhancement of strength due to hydrate 
      Pcc=c*(X*Sh)**d
c
c     Get the enhancement of dilatancy  due to hydrate 
      Pcd=a*(X*Sh)**b
c     
c     Calculate mean and deviator stresses 
      pc=(stress1(1,1)+stress1(2,1)+stress1(3,1))/3.0
      if (pc<0.and.h<269.905) pc=0.0001
c
      qc=sqrt(3.0/2.0)*sqrt((stress1(1,1)-pc)**2+
     1 (stress1(2,1)-pc)**2+(stress1(3,1)-pc)**2+
     2 stress1(4,1)**2)
c
c
c
c     Calculate elastic stifness
 	  BKS=(1.0+void)/sik*pc
 	  GS0=3.0*BKS/2.0*(1.0-2.0*pnyu)/(1.0+pnyu) 
 	  GS=GS0+rM2*X*Sh
c
       do i=1,3
           do j=1,3
               if (i==j) then
                   De(i,j)=BKS+4./3.*GS
               else
                   De(i,j)=BKS-2./3.*GS
               end if
           end do
       end do
                   De(4,4)=GS 
c   
      call MATINV(1,4,De,DeINV)
      do i=1,4
           do j=1,4
               ee(i,1)=ee(i,1)+DeINV(i,j)*stress2(j,1)
           end do
      end do 
c     Calculate elastic stifness changes due to hydrate dissociation
       do i=1,3
           do j=1,3
               if (i==j) then
                   Dhe(i,j)=4./3.*rM2
               else
                   Dhe(i,j)=-2./3.*rM2
               end if
           end do
       end do
            Dhe(4,4)=rM2 
       do i=1,4
          do j=1,4
                  DShe1(i,1)=DShe1(i,1)+Dhe(i,j)*ee(j,1)
          end do
       end do       
c
         do i=1,4
                  DShe(i,1)=X*DShe1(i,1)
         end do                      
C     Calulate the dFdq
       dFdq=2*qc
c
c     Calculate the subloading ratio of R
       R=(qc**2+rM**2*pc*(pc+pcc))/(rM**2*(pc+pcc)*(p1+pcd+pcc))
       if (R>1) R=1
c   
      KST=2
      IF(IDBG.EQ.1)WRITE(6,901)KST
901   FORMAT(1X,'***KST',i5)   
c
c     Calculate the depddep
c      IF (ABS(epd)<1.0E-10)then
c          depddep1=1
c          depddep2=1
c          depddep3=1
c          depddep4=1
c          depddep5=1
c          depddep6=1
c      Else
c          depddep1=(2/3)/epd*(ep1-epv/3)
c          depddep2=(2/3)/epd*(ep2-epv/3)
c          depddep3=(2/3)/epd*(ep3-epv/3)
c          depddep4=(2/3)/(2*epd)*ep4
c          depddep5=(2/3)/(2*epd)*ep5
c          depddep6=(2/3)/(2*epd)*ep6
c      END IF
c
       IF (ABS(epd)<1.0E-10)then
           depddep1=0
           depddep2=0
           depddep3=0
           depddep4=0
           depddep5=0
           depddep6=0
       Else
           depddep1=(2.0/3.0)/epd*(ep1-epv/3.0)
           depddep2=(2.0/3.0)/epd*(ep2-epv/3.0)
           depddep3=(2.0/3.0)/epd*(ep3-epv/3.0)
           depddep4=(2.0/3.0)/(2*epd)*ep4
           depddep5=(2.0/3.0)/(2*epd)*ep5
           depddep6=(2.0/3.0)/(2*epd)*ep6
       END IF
C
c     Calculate the dFdS
      do i=1,3
          dFds(i,1)=(rM**2)*(2*pc-R*(pcd+pcc+p1)+pcc)/3
     1    +3*(stress1(i,1)-pc)
      end do
          dFds(4,1)=6.0*stress1(4,1)
c      
c      Calculate the dFdPcc and dFdPcd
       dFdPcc=(rM**2)*(pc-R*(pc+pcc+pcd+p1+pcc))
       dFdPcd=-R*(rM**2)*(pc+pcc)
c
c      Calculate the dlamdah
       dlamdah=dFdPcd*a*b*(X*Sh)**b+dFdPcc*c*d*(X*Sh)**d   
c 
c     
c     Calculate plastic stiffness matrix
      dFdsii=dFds(1,1)+dFds(2,1)+dFds(3,1)
c
      do i=1,4
          dFdst(1,i)=dFds(i,1)
      end do
      do i=1,4
          do j=1,4
              dlamda1(1,i)=dlamda1(1,i)+dFdst(1,j)*De(j,i)
          end do
      end do
c
C     abs(df/dsigma)
      rMODU=sqrt(dFds(1,1)**2+dFds(2,1)**2+dFds(3,1)**2
     1      +dFds(4,1)**2)
!      write(6,500) rMODU,dFds(1,1)
! 500  FORMAT(1X, 'rMODU=',E15.5, 'dFds(1,1)=',E15.5)
c     depd/dep*df/dsig
      rMODI=depddep1*dFds(1,1)+depddep2*dFds(2,1)
     1     +depddep3*dFds(3,1)+depddep4*dFds(4,1)
      dlamda2=-R*(rM**2)*(pc+pcc)*(1+void)*p1/(cil-sik)*dFdsii
     1     +rM**2*(pc+pcc)*(p1+pcd+pcc)*u*(1+(pcc+pcd)/p1)
     2     *log(R)*rMODU-eM*dlamdah*rMODI
c
      KST=3
      IF(IDBG.EQ.1)WRITE(6,902)KST
902   FORMAT(1X,'***KST',i5)
      do i=1,4
          dlamda3=dlamda3+dlamda1(1,i)*dFds(i,1)
      end do  
c
      do i=1,4
          dlamda0(1,i)=dlamda1(1,i)/(-dlamda2+dlamda3)
      end do
c
c
      do i=1,4
          dlamda4=dlamda4+dlamda0(1,i)*dstrain1(i,1)
      end do
c
c     Calculate the dfdsh
      dpcddsh=X*a*b*(X*Sh)**(b-1)
      dpccdsh=X*c*d*(X*Sh)**(d-1)
      dfdsh=dfdpcd*dpcddsh+dfdpcc*dpccdsh
      do i=1,4
          do j=1,4
               Dsp22(i,1)=Dsp22(i,1)+De(i,j)*dFds(j,1)
          end do
      end do
c
      do i=1,4
              Dsp2(i,1)=dfdsh*Dsp22(i,1)
      end do           
c
      dSh=0
            KST=4
      IF(IDBG.EQ.1)WRITE(6,903)KST
903   FORMAT(1X,'***KST',i5)
      do i=1,4
          dlamda5=dlamda5+dlamda0(1,i)*dstrain1(i,1)
      end do
          dlamda6=dSh*dfdsh/(-dlamda2+dlamda3)  
          dlamda7=dSh*dlamda4/(-dlamda2+dlamda3)
          dlamda=dlamda5+dlamda6+dlamda7
c
      do i=1,4
          do j=1,4
              Dp11(i,1)=Dp11(i,1)+De(i,j)*dFds(j,1)
          end do
      end do
c 
      do i=1,4
          do j=1,4
              Dp12(i,j)=Dp12(i,j)+Dp11(i,1)*dFdst(1,j)
          end do
      end do
c
      do i=1,4
          do j=1,4
              do k=1,4
                  Dp1(i,j)=Dp1(i,j)+Dp12(i,k)*De(k,j)
              end do
          end do
      end do          
c
      do i=1,4
              do k=1,4
                  Dsp1(i,1)=Dsp1(i,1)+Dp12(i,k)*Dshe(k,1)
              end do
      end do          
c 
c
      do i=1,4
          do j=1,4
              Dp(i,j)=Dp1(i,j)/(-dlamda2+dlamda3)
          end do
      end do   
c
c
      do i=1,4
              Dsp3(i,1)=Dsp1(i,1)+Dsp2(i,1)
      end do   
c
c
      do i=1,4
              Dsp(i,1)=Dsp3(i,1)/(-dlamda2+dlamda3)        
      end do   
c
c     Define yield function
      fs=qc**2+(rM**2)*(pc+pcc)*(pc-R*(p1+pcc+pcd))
c
c     Judging yielding    
      do i=1,4
          do j=1,4
              if (dlamda>=0.0) then
                  DDSDDE(i,j)=De(i,j)-Dp(i,j)
                  DHSDDE(i,1)=DShe(i,1)-Dsp(i,1)
                  depv=dlamda*dFdsii
                  depd=dlamda*dFDq
                  yjudge=1.0
              else
                  DDSDDE(i,j)=De(i,j)
                  DHSDDE(i,1)=DShe(i,1)
                  depv=0.0
                  yjudge=0.0
              end if
          end do
      end do
c
c     Update new stresses
      do i=1,4
          do j=1,4
              dstress2(i,1)=dstress2(i,1)+DDSDDE(i,j)*dstrain1(j,1)
          end do
      end do 
c
      do i=1,4
              dstress1(i,1)=dstress2(i,1)+DHSDDE(i,1)*dSh    
      end do 
c
      do i=1,4
          stress1(i,1)=stress1(i,1)+dstress1(i,1)
          stress(i)=-stress1(i,1)
      end do
c
c     Update plastic volumetric strain
      epv=epv+depv
c
c     Update plastic deviatoric strain
      epd=epd+depd
c
c     Update plastic strain
      dep1=dlamda*dFds(1,1)
      dep2=dlamda*dFds(2,1)
      dep3=dlamda*dFds(3,1)
      dep4=dlamda*dFds(4,1)
      dep5=dlamda*dFds(5,1)
      dep6=dlamda*dFds(6,1)                        
      ep1=ep1+dep1
      ep2=ep2+dep2
      ep3=ep3+dep3
      ep4=ep4+dep4
      ep5=ep5+dep5
      ep6=ep6+dep6
c
c     Update the void ratio
      dvols=dstran(1)+dstran(2)+dstran(3)
      dv=dvols*(1.0+void)
      void=void+dv
c
c     Update preconsolidation pressure
      p1=p1+(1+void)*(pcc+pcd+p1)/(cil-sik)*depv
      if (p1<0) then
      p1=0
      end if
C
C
c     Update degradation factor
      dX=-eM*X*depd
      X=X+dX
c
c     Update pcc and pcd
      dpcc=c*d*(X*Sh)**(d-1)*(dX*Sh+X*dSh)
      dpcd=a*b*(X*Sh)**(b-1)*(dX*Sh+X*dSh)
c      pcc=pcc+dpcc
c      pcd=pcd+dpcd
c
c     Update degradation factor
      pc1=p1+pcc+pcd
c
c
c     Update Hydrate Saturation
      Sh=Sh+dSh
c
c     Update solution dependent state variable (SDV)
      statev(1)=epv
      statev(2)=void
      statev(3)=X
      statev(4)=epd
      statev(5)=dX
      statev(6)=pc
      statev(7)=qc
      statev(8)=GS
      statev(9)=p1
      statev(10)=yjudge
      statev(11)=dlamda
      statev(12)=fs
      statev(13)=dFdsii
      statev(14)=dFDq
      statev(15)=De(1,1)
      statev(16)=De(2,2)
      statev(17)=De(3,3)
      statev(18)=De(1,2)
      statev(19)=De(1,3)
      statev(20)=De(2,3)
      statev(21)=De(4,4)
      statev(22)=De(5,5)
      statev(23)=De(6,6)
      statev(24)=Dp(1,1)
      statev(25)=Dp(2,2)
      statev(26)=Dp(3,3)
      statev(27)=Dp(1,2)
      statev(28)=Dp(1,3)
      statev(29)=Dp(2,3)
      statev(30)=Dp(4,4)
      statev(31)=Dp(5,5)
      statev(32)=Dp(6,6)
      statev(33)=DDSDDE(1,1)
      statev(34)=DDSDDE(2,2)
      statev(35)=DDSDDE(3,3)
      statev(36)=DDSDDE(1,2)
      statev(37)=DDSDDE(1,3)
      statev(38)=DDSDDE(2,3)
      statev(39)=DDSDDE(4,4)
      statev(42)=Dp1(1,1)
      statev(43)=Dp1(2,2)
      statev(44)=Dp1(3,3)
      statev(45)=Dp1(1,2)
      statev(46)=Dp1(1,3)
      statev(47)=Dp1(2,3)
      statev(48)=Dp1(4,4)
      statev(49)=Dp1(5,5)
      statev(50)=Dp1(6,6)
      statev(51)=Dp11(1,1)
      statev(52)=Dp11(2,2)
      statev(53)=Dp11(3,3)
      statev(54)=Dp11(1,2)
      statev(55)=Dp11(1,3)
      statev(56)=Dp11(2,3)
      statev(57)=Dp11(4,4)
      statev(58)=Dp11(5,5)
      statev(59)=Dp11(6,6)
      statev(60)=Dp12(1,1)
      statev(61)=Dp12(2,2)
      statev(62)=Dp12(3,3)
      statev(63)=Dp12(1,2)
      statev(64)=Dp12(1,3)
      statev(65)=Dp12(2,3)
      statev(66)=Dp12(4,4)
      statev(67)=Dp12(5,5)
      statev(68)=Dp12(6,6)
      statev(69)=dstran(1)
      statev(70)=dstran(2)
      statev(71)=dstran(3)
      statev(72)=dstran(4)
      statev(75)=R
      statev(76)=depv
      statev(77)=depd
      statev(78)=dFds(1,1)
      statev(79)=dFds(2,1)
      statev(80)=dFds(3,1)
      statev(81)=pcc
      statev(82)=pcd
      statev(83)=dlamda2
      statev(84)=dlamda3
      statev(85)=dlamda4
      statev(86)=dlamdah
      statev(87)=dFdPcc
      statev(88)=dFdPcd
      statev(89)=ep1
      statev(90)=ep2
      statev(91)=ep3
      statev(92)=ep4
      statev(93)=ep5
      statev(94)=ep6
      statev(95)=pc1
      statev(96)=MODI
      statev(97)=dFds(4,1)
      statev(98)=dFds(5,1)
      statev(99)=dFds(6,1)
      statev(100)=depddep1
      statev(101)=depddep2
      statev(102)=depddep3
      statev(103)=depddep4
      statev(104)=depddep5
      statev(105)=depddep6
      statev(106)=ee(1,1)
      statev(107)=ee(2,1)
      statev(108)=ee(3,1)
      statev(109)=ee(4,1)
      statev(110)=ee(5,1)
      statev(111)=ee(6,1)
      statev(112)=Sh
      statev(113)=dSh
c      
      return
      end
c                 
      SUBROUTINE MATINV(N1,N2,A,AINV)
C A general purpose matrix inverter by augmenting-pivoting technique:

C     A B C | 1 0 0        1 0 0 | J K L
C     D E F | 0 1 0   =>   0 1 0 | M N O
C     G H I | 0 0 1        0 0 1 | P Q R

C Explanation of passed parameters:
C        N1: lower dimension of square matrix
C        N2: upper dimension of square matrix
C         A: square matrix of dimension N1:N2,N1:N2 to be inverted
C      AINV: the inverted matrix

      IMPLICIT NONE              !N2-N1+1
      INTEGER I,J,K,N1,N2
      REAL*8 A(N1:N2,N1:N2),AINV(N1:N2,N1:N2),B(N1:N2,N1:2*N2-N1+1)
     &,PIVOT,XNUM

C     INITIALIZATION
      DO I=N1,N2
      DO J=N1,N2
      AINV(I,J)=0.0D0
      END DO
      END DO

C MAKE AUGMENTED MATRIX
      DO I=N1,N2
      DO J=N1,N2
      B(I,J)=0.0D0
      B(I,J+N2-N1+1)=0.0D0

      B(I,J)=A(I,J)
       IF(I.EQ.J) THEN
       B(I,J+N2-N1+1)=1.0D0
       END IF
      END DO
      END DO

      DO I=N1,N2

C CHOOSE THE LEFTMOST NON-ZERO ELEMENT AS PIVOT
      DO J=N1,N2
      IF(DABS(B(I,J)).GT.0)THEN
      PIVOT=B(I,J)
      EXIT
      END IF
      END DO

C STEP 1: Change the chosen pivot into "1" by dividing
C the pivot's row by the pivot number
      DO J=N1,2*N2-N1+1
      B(I,J)=B(I,J)/PIVOT
      END DO
      PIVOT=B(I,I) !UPDATE PIVOT VALUE

C STEP 2: Change the remainder of the pivot's COLUMN into 0's
C by adding to each row a suitable multiple of the PIVOT ROW
      DO K=N1,N2 !ROW
       IF(K.NE.I) THEN
       XNUM=B(K,I)/PIVOT !SAME COLUMN WITH THE CURRENT PIVOT
        DO J=N1,2*N2-N1+1 !COL
        B(K,J)=B(K,J)-XNUM*B(I,J)
        END DO
       END IF
      END DO

      END DO

C PREPARE THE FINAL INVERTED MATRIX
      DO I=N1,N2
      DO J=N1,N2
      AINV(I,J)=B(I,J+N2-N1+1)
      END DO
      END DO

      RETURN
      END
C    
       
      
       
      