
module loF_message

  use loF_common
  implicit none  
   
  interface
     function lo_message_new( ) bind(c)
       use, intrinsic :: iso_c_binding
       implicit none
       type(c_ptr) :: lo_message_new
     end function lo_message_new
     !void lo_message_free(lo_message m);
     subroutine lo_message_free (msg) bind(c)
       use, intrinsic :: iso_c_binding
       type(c_ptr), value :: msg
     end subroutine lo_message_free

     !int lo_message_get_argc(lo_message m);
     function lo_message_get_argc(msg) bind(c)
       use, intrinsic :: iso_c_binding
       integer(c_int) :: lo_message_get_argc
       type(c_ptr), value :: msg
     end function lo_message_get_argc
     !/** \brief Pretty-print a lo_message object. */
     !void lo_message_pp(lo_message m);
     subroutine lo_message_pp(msg) bind(c)
       use, intrinsic :: iso_c_binding
       type(c_ptr), value :: msg
     end subroutine lo_message_pp
     
     !int lo_message_add_float(lo_message m, float a);
     function lo_message_add_float(msg, f) bind(c)
       use, intrinsic :: iso_c_binding
       integer(c_int) :: lo_message_add_float
       type(c_ptr), value :: msg
       real(c_float), value :: f
       ! real :: f
     end function lo_message_add_float
     !int lo_message_add_double(lo_message m, double a);
     function lo_message_add_double(msg, f) bind(c)
       use, intrinsic :: iso_c_binding
       integer(c_int) :: lo_message_add_double
       type(c_ptr), value :: msg
       real(c_double), value :: f
       ! real :: f
     end function lo_message_add_double
     
     !int lo_message_add_int32(lo_message m, int32_t a);
     function lo_message_add_int32(msg, i) bind(c)
       use, intrinsic :: iso_c_binding
       integer(c_int) :: lo_message_add_int32
       type(c_ptr), value :: msg
       ! integer(c_int32_t) :: i
       integer(c_int), value :: i
     end function lo_message_add_int32
     function lo_message_add_int64(msg, i) bind(c)
       use, intrinsic :: iso_c_binding
       integer(c_int) :: lo_message_add_int64
       type(c_ptr), value :: msg
       ! integer(c_int32_t) :: i
       integer(c_long), value :: i
     end function lo_message_add_int64

     !int lo_message_add_string(lo_message m, const char *a);
     function lo_message_add_string(msg, s) bind(c)
       use, intrinsic :: iso_c_binding
       integer(c_int) :: lo_message_add_string
       type(c_ptr), value :: msg
       character(kind=c_char), dimension(*) :: s
     end function lo_message_add_string
     !int lo_message_add_symbol(lo_message m, const char *a);
     function lo_message_add_symbol(msg, s) bind(c)
       use, intrinsic :: iso_c_binding
       integer(c_int) :: lo_message_add_symbol
       type(c_ptr), value :: msg
       character(kind=c_char), dimension(*) :: s
     end function lo_message_add_symbol
     !int lo_message_add_char(lo_message m, char a);
     function lo_message_add_char(msg, c) bind(c)
       use, intrinsic :: iso_c_binding
       integer(c_int) :: lo_message_add_char
       type(c_ptr), value :: msg
       character(kind=c_char), value :: c
     end function lo_message_add_char


     !size_t lo_arg_size(lo_type type, void *data)
     function lo_arg_size(type, data) bind(c)
       use, intrinsic :: iso_c_binding
       integer(c_size_t) :: lo_arg_size
       character(c_char), value :: type
       ! type(c_ptr) :: data
       character(c_char), dimension(*) :: data
     end function lo_arg_size
     
     !int lo_send_message(lo_address targ, const char *path, lo_message msg);
     function lo_send_message(addr, path, msg) bind(c)
       use, intrinsic :: iso_c_binding
       integer(c_int) :: lo_send_message
       type(c_ptr), value :: addr
       character(kind=c_char), dimension(*) :: path
       type(c_ptr), value :: msg
     end function lo_send_message
     
  end interface

contains


end module loF_message

