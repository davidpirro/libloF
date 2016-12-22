
module loF_common

  use iso_c_binding
  implicit none
  
  type, bind(c) :: lo_timetag
     integer(c_int32_t) sec
     integer(c_int32_t) frac
  end type lo_timetag

  type loarg
     character(c_char), dimension(16) :: p
  end type loarg

contains

  function cfstring(incs)
    use, intrinsic :: iso_c_binding
    character(len=128) :: cfstring
    type(c_ptr) :: incs
    call c_f_string(incs,cfstring)
    cfstring = trim(cfstring)
  end function cfstring

end module loF_common

