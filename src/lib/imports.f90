module imports
!*Brief Description:* This module contains all the subroutines required to
!import fields, previous model results, etc.
!*LICENSE:*
!
!
!
!*Full Description:*
!
  !
  use arrays
  use diagnostics
  use geometry
  use indices
  use other_consts
  use ventilation

  implicit none

  !Module parameters

  !Module types

  !Module variables

  !Interfaces
  private
  public import_ventilation
  public import_perfusion
  public import_exelemfield
  public import_terminalfield

contains
!
!##############################################################################
!
!>*import_ventilation:* This subroutine reads in the results of a ventilation model that
! has been saved in an exelem format as a single flow field (elements listed with
! ventilation as field values).
 subroutine import_ventilation(FLOWFILE)
 !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"SO_IMPORT_VENTILATION" :: IMPORT_VENTILATION

   character(len=MAX_FILENAME_LEN),intent(in) :: FLOWFILE
   !local variables
   integer :: ierror,ne,nunit
   character(LEN=132) :: ctemp1,exfile
   real(dp) :: flow,flow_unit,maxflow

   character(len=60) :: sub_name

   sub_name = 'import_ventilation'
   call enter_exit(sub_name,1)

   print *, 'Reading in ventilation results'
   call import_exelemfield(FLOWFILE,ne_Vdot)
   do nunit = 1,num_units
     ne = units(nunit)
     if(elem_field(ne_Vdot,ne).lt.0.0_dp) elem_field(ne_Vdot,ne) = zero_tol
     unit_field(nu_Vdot0,nunit) = elem_field(ne_Vdot,ne)
   enddo

!!! sum the fields up the tree
   call sum_elem_field_from_periphery(ne_Vdot) !sum the air flows recursively UP the tree
   maxflow = elem_field(ne_Vdot,1)


   call enter_exit(sub_name,2)
 end subroutine import_ventilation

!
!###########################################################################################
!
!>*import_perfusion:* This subroutine reads in the results of a ventilation model that
! has been saved in an exelem format as a single flow field (elements listed with
! ventilation as field values).
 subroutine import_perfusion(FLOWFILE)
 !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"SO_IMPORT_PERFUSION" :: IMPORT_PERFUSION

   character(len=MAX_FILENAME_LEN),intent(in) :: FLOWFILE
   !local variables
   integer :: ierror,ne,nunit
   character(LEN=132) :: ctemp1,exfile
   real(dp) :: flow,flow_unit,maxflow

   character(len=60) :: sub_name

   sub_name = 'import_perfusion'
   call enter_exit(sub_name,1)

   print *, 'Reading in perfusion results'
   call import_exelemfield(FLOWFILE,ne_Qdot)
   do nunit = 1,num_units
     ne = units(nunit)
     if(elem_field(ne_Qdot,ne).lt.0.0_dp) elem_field(ne_Qdot,ne) = zero_tol
     unit_field(nu_perf,nunit) = elem_field(ne_Qdot,ne)
   enddo

!!! sum the fields up the tree
   call sum_elem_field_from_periphery(ne_Qdot) !sum the air flows recursively UP the tree
   maxflow = elem_field(ne_Qdot,1)

   call enter_exit(sub_name,2)
 end subroutine import_perfusion

!
!##############################################################################
!
!>*import_exelemfield:* This subroutine reads in the content of an exelem field file (1 field)
 subroutine import_exelemfield(FLOWFILE,field_no)
 !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"SO_IMPORT_EXELEMFIELD" :: IMPORT_EXELEMFIELD

   character(len=MAX_FILENAME_LEN),intent(in) :: FLOWFILE
   integer, intent(in) :: field_no
   !local variables
   integer :: ierror,ne,nunit
   character(LEN=132) :: ctemp1,exfile
   real(dp) :: flow,flow_unit,maxflow

   character(len=60) :: sub_name

   sub_name = 'import_exelemfield'
   call enter_exit(sub_name,1)

   open(10, file=FLOWFILE, status='old')
   ne = 0
   read_elem_flow : do !define a do loop name
     !.......read element flow
     read(unit=10, fmt="(a)", iostat=ierror) ctemp1
     if(index(ctemp1, "Values:")> 0) then
       ne = ne+1
       read(unit=10, fmt="(a)", iostat=ierror) ctemp1
       flow = get_final_real(ctemp1)
       ! if(flow.lt.0.0_dp) flow = zero_tol  ! Behdad commented out to read neg. intensity
         elem_field(field_no,ne) = flow! read it in
       end if
       if(ne.ge.num_elems) exit read_elem_flow
     end do read_elem_flow

   close(10)

    call enter_exit(sub_name,2)
 end subroutine import_exelemfield

 !
 !##############################################################################
 !

!>*import_exnodefield:* This subroutine reads in the content of an exnode field file (up to 2 fields)
 subroutine import_terminalfield(FILENAME,field_no,field1name,field2name)

   character(len=MAX_FILENAME_LEN),intent(in) :: FILENAME,field1name,field2name
   integer, intent(in) :: field_no ! can only be 1 or 2
   !local variables
   character(len=MAX_FILENAME_LEN) :: FILE
   integer :: ierror,nn,nunit,ne
   character(LEN=132) :: ctemp1
   real(dp) :: field1,field2,cluster,intensity_ratio

   character(len=60) :: sub_name

   sub_name = 'import_exnodefield'
   call enter_exit(sub_name,1)

   if(field_no.gt.2)then
     write(*,*) 'Invalid number of fields to import. Implemented only for to import maximum of two fields.'
     call exit(0)
   endif

   open(20, file=FILENAME, status='old')
   nn = 0 ! initialise node_number
   read_node_field : do !define a do loop name
     !.......read node field1
     read(unit=20, fmt="(a)", iostat=ierror) ctemp1
     if(index(ctemp1, "Node:")> 0) then
       nn = nn + 1
       ! nn = get_final_integer(ctemp1) ! getting terminal node number
       ! ne = elems_at_node(nn,1) ! finding the elem connected to terminal node nn
       ! since the node is terminal ne should be the terminal element number
       read(unit=20, fmt="(a)", iostat=ierror) ctemp1
       read(unit=20, fmt="(a)", iostat=ierror) ctemp1
       read(unit=20, fmt="(a)", iostat=ierror) ctemp1
       read(unit=20, fmt="(a)", iostat=ierror) ctemp1
       field1 = get_final_real(ctemp1)
       if (field_no.gt.1) then
         read(unit=20, fmt="(a)", iostat=ierror) ctemp1 ! read 5 files down to get to label
         field2 = get_final_real(ctemp1) ! read field one after the coordinates
       endif
       if (field1name.eq.'flow') then
         if(field1.lt.0.0_dp) then
           field1 = zero_tol
         else
           unit_field(nu_perf,nn) = field1! read it in
         end if
       elseif (field1name.eq.'pressure')then
         unit_field(nu_blood_press,nn) = field1
       elseif (field1name.eq.'intensity')then !for intensity map
         unit_field(nu_flow_map,nn) = field1
       elseif (field1name.eq.'cluster')then !for cluster labels
         unit_field(nu_label,nn) = field1
       else
         print *, 'Field 1 is invalid.'
         print *, 'Only valid fields to import are flow, pressure, intensity and cluster.'
         call exit(0)
       endif
       if (field2name.eq.'flow') then
         if(field2.lt.0.0_dp) then
           field2 = zero_tol
           unit_field(nu_perf,nn) = field2! read it in
         end if
       elseif (field2name.eq.'pressure')then
         unit_field(nu_blood_press,nn) = field2
       elseif (field2name.eq.'intensity')then !for intensity map
         unit_field(nu_flow_map,nn) = field2
       elseif (field2name.eq.'cluster')then !for cluster labels
         unit_field(nu_label,nn) = field2
       else
         print *, 'Field 2 is invalid.'
         print *, 'Only valid fields to import are flow, pressure, intensity and cluster.'
         call exit(0)
       endif
       if(nn.ge.num_units) exit read_node_field ! "Node:" index
     endif ! find "Node:" index
   end do read_node_field

   close(20)

    call enter_exit(sub_name,2)
    ! cluster = 11
    ! intensity_ratio = 0.127
    ! FILE = '/hpc/bsha219/lung/Data/CTEPH/CTEPH4/FRC/Intensity_mapping/CTEPH4_flow_diff_fractions_avg_RM1.exelem'
    ! call import_exelemfield(FILE,ne_intensity)
    ! write(*,*) 'underperfused cluster:', cluster
    ! call find_occlusion(cluster, intensity_ratio)
 end subroutine import_terminalfield

end module imports
