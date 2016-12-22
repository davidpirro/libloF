
module loF_address

  implicit none

  ! typedef struct {
  ! 	/** The number of seconds since Jan 1st 1900 in the UTC timezone. */
  ! 	uint32_t sec;
  ! 	/** The fractions of a second offset from above, expressed as 1/2^32nds
  !          * of a second */
  ! 	uint32_t frac;
  ! } lo_timetag;
  type, bind(c) :: lo_timetag
     integer(c_int32_t) sec
     integer(c_int32_t) frac
  end type lo_timetag

  type loarg
     character(c_char), dimension(16) :: p
  end type loarg

  
   
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

     !const char *lo_address_errstr(lo_address a);
     ! function lo_address_errstr(addr) bind(c, name="lo_address_errstr")
     !   use iso_c_binding, only: c_ptr, c_char
     !   implicit none
     !   type(c_ptr) :: addr
     !   character(kind=c_char), dimension(*) :: lo_address_errstr
     ! end function lo_address_errstr
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


     ! void *lo_message_serialise(lo_message m, const char *path, void *to,
     !    		   size_t *size);
     function lo_message_serialise(msg, path, to, size) bind(c)
       use, intrinsic :: iso_c_binding
       type(c_ptr) :: lo_message_serialise
       type(c_ptr), value :: msg
       character(kind=c_char), dimension(*) :: path
       type(c_ptr), value :: to
       integer(c_size_t) :: size
     end function lo_message_serialise
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
     
     !int lo_send_message(lo_address targ, const char *path, lo_message msg);
     ! function lo_send_message(address, path, msg) bind(c, name="lo_send_message")
     !   use iso_c_binding, only: c_ptr, c_char, c_int
     !   type(c_ptr) :: address, msg
     !   character(kind=c_char), dimension(*) :: path
     !   integer(c_int) :: lo_address_new
     ! end function lo_send_message

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

  function cfstring(incs)
    use, intrinsic :: iso_c_binding
    character(len=128) :: cfstring
    type(c_ptr) :: incs
    call c_f_string(incs,cfstring)
    cfstring = trim(cfstring)
  end function cfstring

  function genh (path, types, argv, argc, msg, data)
    use, intrinsic :: iso_c_binding
    integer(c_int) :: genh
    character(c_char), dimension(*) :: path, types
    ! type(c_ptr), dimension(*), value :: argv
    ! type(c_ptr), dimension(*) :: argv
    type(loarg), dimension(:) :: argv
    integer(c_int), value :: argc
    type(c_ptr), value :: msg, data
    ! integer :: nar, i
    ! character(c_char) :: cc = "c"//c_null_char
    ! character(kind=1) :: cc = "c"
    ! type(c_ptr) :: to = c_null_ptr
    ! character(c_char), dimension(:), pointer :: toc
    ! integer(c_size_t) :: size
    ! integer(4) :: ti
    type(loarg), dimension(:), pointer :: floarg
    integer :: i, na
    integer(c_size_t) :: ts
    ! nar = argc
    print*, "called generic with ", path(1:10), " path"
    print*, "called generic with ", types(1:10), " types"
    ! print*, "called generic with ", argc, " arguments"
    na = lo_message_get_argc(c_loc(msg))
    print*, "called generic message with ", na, " arguments"
    call lo_message_pp(c_loc(msg))

    ! ts = 0
    ! do i = 1, na
    !    print*, "type", i, types(i)
    !    ts = ts + lo_arg_size(types(i), argv(1 + ts)%p)
    !    print*, "size ", i, lo_arg_size(types(i), argv(1 + ts)%p)
    ! end do

    ts = 0
    print*, "print int ", transfer(argv(1+ts)%p(1:4), 1_c_int)
    ts = ts + lo_arg_size(types(1), argv(1 + ts)%p)
    print*, "print char ", argv(1+ts)%p(1:4)
    ts = ts + lo_arg_size(types(2), argv(1 + ts)%p)
    print*, "print float ", transfer(argv(1+ts)%p(1:4), 1.0_c_float)

    
    
    ! print*, "print int ", transfer(argv(1)%p(1:4), 1_c_int)
    ! print*, "print char ", argv(5)%p(1:4)
    ! do i = 1, 10
    !    print*, "print float ", i, transfer(argv(5+i)%p(1:4), 1.0_c_float)
    ! end do

    ! print*, "to ass ", c_associated(to)
    ! print*, "size ", size
    ! to = lo_message_serialise(c_loc(msg), path, to, size)
    ! print*, "to ass ", c_associated(to)
    ! print*, "size ", size
    ! do i = 1, size
    !    print*, "c ", i , transfer(argv(i), cc)
    !    print*, "i ", i , transfer(argv(i), ti)
    ! end do
    ! print*, "argv ", argv(1:16)
    ! call c_f_pointer(to, toc)
    ! toc = transfer(to, toc)
    ! print*, "c", toc(1)
    ! do i = 1, 24
    !    print*, "c", i, toc(i)
    ! end do

    ! call c_f_pointer(argv, floarg)

    genh = 1
  end function genh


  ! function lo_message_serialise(msg, path, to, size) bind(c)
  !      use, intrinsic :: iso_c_binding
  !      type(c_ptr) :: lo_message_serialise
  !      type(c_ptr), value :: msg
  !      character(kind=c_char), value :: path
  !      type(c_ptr), value :: to
  !      integer(c_size_t) :: size
  !    end function lo_message_serialise

end module loF_address

program osctest
  use lotest

  implicit none

  character(kind=c_char), dimension(64) :: host, port, path
  type(c_ptr) :: message
  type(c_ptr) :: address, server
  integer :: err
  integer(4) :: timew
  
  host = trim("127.0.0.1")//c_null_char
  port = trim("57121")//c_null_char

  path = trim("liblotest")//c_null_char
  print*, "here"

  print*, "creating address", C_ASSOCIATED(address)

  ! address = lo_address_new(host, port)
  address = lo_address_new("localhost"//c_null_char, "57120"//c_null_char)

  print*, "created address", C_ASSOCIATED(address)

  err = lo_address_errno(address)
  print*, "error",  err
  if (err .ne. 0) then
     print*, "closing "
  else
     print*, "creating message", C_ASSOCIATED(message)

     message = lo_message_new()
     
     print*, "created message", C_ASSOCIATED(message)
     
     print*, "argument count ", lo_message_get_argc(message)
     ! print*, "adding float ", lo_message_add_float(message, real(1.2, c_float))
     print*, "adding float ", lo_message_add_float(message, 1.2_c_float)
     print*, "argument count ", lo_message_get_argc(message)
     print*, "adding float 2", lo_message_add_float(message, real(3.5, c_float))
     print*, "adding int ", lo_message_add_int32(message, 11)
     print*, "adding string ", lo_message_add_string(message, "tester"//c_null_char)
     print*, "adding int64 ", lo_message_add_int32(message, 1134221_c_long)
     print*, "adding double ", lo_message_add_double(message, 0.1265365447123127487876123_c_double)
     print*, "adding symbol ", lo_message_add_symbol(message, "f"//c_null_char)
     print*, "adding char ", lo_message_add_char(message, "b"//c_null_char)
     print*, "argument count ", lo_message_get_argc(message)

     call lo_message_pp(message)
     
     ! print*, "sending ", lo_send_message(address, path, message)
     print*, "sending ", lo_send_message(address, "/testloF"//c_null_char, message)

     print*, "inspecting address host", cfstring(lo_address_get_hostname(address))
     print*, "inspecting address port", cfstring(lo_address_get_port(address))
     
     print*, "freeing msg"
     call lo_message_free(message)
     
     print*, "freeing address"
     call lo_address_free(address)
     
  end if

  print*, "opening server"
  ! server = lo_server_new("57121"//c_null_char, c_null_ptr)
  server = lo_server_thread_new("57121"//c_null_char, c_null_ptr)
  print*, "starting server thread", lo_server_thread_start(server)
  
  call lo_server_thread_add_method(server, "/foo"//c_null_char, "isf"//c_null_char, c_funloc(genh), c_null_ptr)
  ! call lo_server_thread_add_method(server, c_null_ptr, c_null_ptr, c_funloc(genh), c_null_ptr)
  
  
  timew = 1
  do while (timew .ne. 0)
     read*, timew
     print*, "sleeping some more ", timew
     call sleep(timew)
  end do
  
  print*, "free server"
  ! call lo_server_free(server)
  call lo_server_thread_free(server)
  read*


end program osctest
