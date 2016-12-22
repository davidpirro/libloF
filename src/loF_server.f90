
module loF_server

  use loF_common
  implicit none
  
   
  interface

     !lo_server lo_server_new(const char *port, lo_err_handler err_h);
     function lo_server_new(port, errhandle) bind(c)
       use, intrinsic :: iso_c_binding
       type(c_ptr) :: lo_server_new
       character(kind=c_char), dimension(*) :: port
       type(c_ptr), value :: errhandle
     end function lo_server_new
     ! lo_server_thread lo_server_thread_new(const char *port, lo_err_handler err_h);
     function lo_server_thread_new(port, errhandle) bind(c)
       use, intrinsic :: iso_c_binding
       type(c_ptr) :: lo_server_thread_new
       character(kind=c_char), dimension(*) :: port
       type(c_ptr), value :: errhandle
     end function lo_server_thread_new
     ! int lo_server_thread_start(lo_server_thread st);
     function lo_server_thread_start(sert) bind(c)
       use, intrinsic :: iso_c_binding
       integer(c_int) :: lo_server_thread_start
       type(c_ptr), value :: sert
     end function lo_server_thread_start
     !void lo_server_thread_free(lo_server_thread st);
     subroutine lo_server_thread_free(sert) bind(c)
       use, intrinsic :: iso_c_binding
       type(c_ptr), value :: sert
     end subroutine lo_server_thread_free
     
     !void lo_server_free(lo_server s);
     subroutine lo_server_free (ser) bind(c)
       use, intrinsic :: iso_c_binding
       type(c_ptr), value :: ser
     end subroutine lo_server_free
     ! lo_method lo_server_thread_add_method(lo_server_thread st, const char *path,
     !                           const char *typespec, lo_method_handler h,
     !                           void *user_data);
     subroutine lo_server_thread_add_method(st, path, types, h, data) bind(c)
       use, intrinsic :: iso_c_binding
       type(c_ptr), value :: st
       character(c_char), dimension(*) :: path, types
       type(c_funptr), value :: h
       type(c_ptr), value :: data
     end subroutine lo_server_thread_add_method

     ! typedef int (*lo_method_handler)(const char *path, const char *types,
     ! 				 lo_arg **argv, int argc, lo_message msg,
     ! 				 void *user_data);
     
  end interface

contains

end module loF_server
