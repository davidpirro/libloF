
module loF_address

  use loF_common
  implicit none
  
   
  interface
     function lo_address_new(host, port) bind(c)
       use, intrinsic :: iso_c_binding
       type(c_ptr) :: lo_address_new
       character(kind=c_char), dimension(*) :: host, port
       ! type(*) :: lo_address_new
     end function lo_address_new
     !int lo_address_errno(lo_address a);
     function lo_address_errno(addr) bind(c)
       use, intrinsic :: iso_c_binding
       ! use iso_c_binding, only: c_ptr, c_int
       integer(c_int) :: lo_address_errno
       type(c_ptr), value :: addr
       ! integer :: lo_address_errno
     end function lo_address_errno
     !void lo_address_free(lo_address t);
     subroutine lo_address_free (addr) bind(c)
       use, intrinsic :: iso_c_binding
       type(c_ptr), value :: addr       
     end subroutine lo_address_free

     !const char *lo_address_get_hostname(lo_address a);
     function lo_address_get_hostname(addr) bind(c)
       use, intrinsic :: iso_c_binding
       type(c_ptr) :: lo_address_get_hostname
       type(c_ptr), value :: addr       
     end function lo_address_get_hostname
     !const char *lo_address_get_port(lo_address a);
     function lo_address_get_port(addr) bind(c)
       use, intrinsic :: iso_c_binding
       type(c_ptr) :: lo_address_get_port
       type(c_ptr), value :: addr       
     end function lo_address_get_port

  end interface

contains

end module loF_address

