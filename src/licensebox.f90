module licensebox
 use, intrinsic :: iso_c_binding
 implicit none
 private
 public :: lb_client, lb_callbacks, lb_verify, lb_derive, lb_native
 public :: assignment(=)
 type, bind(C) :: lb_callbacks
  type(c_ptr) :: context=c_null_ptr
  type(c_funptr) :: on_sign_in=c_null_funptr, clock=c_null_funptr
  type(c_funptr) :: machine_binding=c_null_funptr, cancelled=c_null_funptr
 end type
 type :: lb_client
  private
  type(c_ptr) :: owner=c_null_ptr
 contains
  procedure :: init
  procedure :: close
  procedure :: call
  procedure :: heartbeat
  final :: finalize
 end type
 interface assignment(=)
  module procedure assign
 end interface
 interface
  integer(c_int) function create_c(options,callbacks,owner,error) bind(C,name='lbf_create')
   import
   character(c_char), intent(in) :: options(*)
   type(c_ptr), value :: callbacks
   type(c_ptr), intent(out) :: owner,error
  end function
  integer(c_int) function call_c(owner,op,args,output,error) bind(C,name='lbf_call')
   import
   type(c_ptr), value :: owner
   character(c_char), intent(in) :: op(*),args(*)
   type(c_ptr), intent(out) :: output,error
  end function
  integer(c_int) function heartbeat_c(owner,module,output,error) bind(C,name='lbf_heartbeat')
   import
   type(c_ptr), value :: owner
   character(c_char), intent(in) :: module(*)
   type(c_ptr), intent(out) :: output,error
  end function
  integer(c_int) function close_c(owner) bind(C,name='lbf_close')
   import
   type(c_ptr), value :: owner
  end function
  subroutine retain_c(owner) bind(C,name='lbf_retain')
   import
   type(c_ptr), value :: owner
  end subroutine
  subroutine release_c(owner) bind(C,name='lbf_release')
   import
   type(c_ptr), value :: owner
  end subroutine
  subroutine free_c(p) bind(C,name='lb_free')
   import
   type(c_ptr), value :: p
  end subroutine
  integer(c_size_t) function strlen_c(p) bind(C,name='strlen')
   import
   type(c_ptr), value :: p
  end function
  type(c_ptr) function result_c(code) bind(C,name='lb_result_code')
   import
   integer(c_int), value :: code
  end function
  integer(c_int) function verify_c(token,product,keys,now,output) bind(C,name='lb_verify_lease_json')
   import
   character(c_char), intent(in) :: token(*),product(*),keys(*)
   real(c_double), value :: now
   type(c_ptr), intent(out) :: output
  end function
  integer(c_int) function derive_c(product,source,raw,output,capacity) bind(C,name='lb_derive_binding')
   import
   character(c_char), intent(in) :: product(*),source(*),raw(*)
   character(c_char), intent(out) :: output(*)
   integer(c_size_t), value :: capacity
  end function
  integer(c_int) function native_c(product,output,capacity) bind(C,name='lb_native_binding')
   import
   character(c_char), intent(in) :: product(*)
   character(c_char), intent(out) :: output(*)
   integer(c_size_t), value :: capacity
  end function
 end interface
contains
 function text(p) result(s)
  type(c_ptr), value :: p
  character(:), allocatable :: s
  character(c_char), pointer :: chars(:)
  integer :: i,n
  s=''; if(.not.c_associated(p)) return
  n=int(strlen_c(p));call c_f_pointer(p,chars,[n]);s=repeat(" ",n)
  do i=1,n;s(i:i)=chars(i);end do
 end function
 logical function valid(s)
  character(*),intent(in)::s
  valid=index(s,c_null_char)==0
 end function
 subroutine assign(lhs,rhs)
  type(lb_client),intent(inout)::lhs
  type(lb_client),intent(in)::rhs
  call retain_c(rhs%owner);call release_c(lhs%owner);lhs%owner=rhs%owner
 end subroutine
 subroutine finalize(self)
  type(lb_client),intent(inout)::self
  call release_c(self%owner);self%owner=c_null_ptr
 end subroutine
 subroutine init(self,options,error,callbacks)
  class(lb_client),intent(inout)::self
  character(*),intent(in)::options
  character(:),allocatable,intent(out)::error
  type(lb_callbacks),target,intent(in),optional::callbacks
  type(c_ptr)::e,cb
  integer(c_int)::r
  error='';if(.not.valid(options))then;error='invalid_argument';return;end if
  call release_c(self%owner);self%owner=c_null_ptr
  cb=c_null_ptr;if(present(callbacks))cb=c_loc(callbacks)
  r=create_c(options//c_null_char,cb,self%owner,e)
  if(r/=0)then;error=text(e);if(error=='')error=text(result_c(r));end if
  call free_c(e)
 end subroutine
 subroutine close(self,error)
  class(lb_client),intent(inout)::self
  character(:),allocatable,intent(out),optional::error
  integer(c_int)::r
  r=close_c(self%owner)
  if(present(error))then
   error='';if(r/=0)error=text(result_c(r))
  end if
 end subroutine
 subroutine call(self,operation,args,value,error)
  class(lb_client),intent(inout)::self
  character(*),intent(in)::operation,args
  character(:),allocatable,intent(out)::value,error
  type(c_ptr)::p,e
  integer(c_int)::r
  value='';error=''
  if(.not.valid(operation).or..not.valid(args))then;error='invalid_argument';return;end if
  r=call_c(self%owner,operation//c_null_char,args//c_null_char,p,e)
  if(r==0)then
   if(c_associated(p))then;value=text(p);else;error='protocol_error';end if
  else
   error=text(e);if(error=='')error=text(result_c(r))
  end if
  call free_c(p);call free_c(e)
 end subroutine
 subroutine heartbeat(self,module,value,error)
  class(lb_client),intent(inout)::self
  character(*),intent(in)::module
  character(:),allocatable,intent(out)::value,error
  type(c_ptr)::p,e
  integer(c_int)::r
  value='';error=''
  if(len(module)==0.or..not.valid(module))then;error='invalid_argument';return;end if
  r=heartbeat_c(self%owner,module//c_null_char,p,e)
  if(r==0)then
   if(c_associated(p))then;value=text(p);else;error='protocol_error';end if
  else
   error=text(e);if(error=='')error=text(result_c(r))
  end if
  call free_c(p);call free_c(e)
 end subroutine
 subroutine lb_verify(token,product,keys,now,value,error)
  character(*),intent(in)::token,product,keys
  real(c_double),intent(in)::now
  character(:),allocatable,intent(out)::value,error
  type(c_ptr)::p
  integer(c_int)::r
  value='';error=''
  if(.not.valid(token).or..not.valid(product).or..not.valid(keys))then;error='invalid_argument';return;end if
  p=c_null_ptr;r=verify_c(token//c_null_char,product//c_null_char,keys//c_null_char,now,p)
  if(r==0)then;value=text(p);else;error=text(result_c(r));end if
  call free_c(p)
 end subroutine
 subroutine lb_derive(product,source,raw,value,error)
  character(*),intent(in)::product,source,raw
  character(:),allocatable,intent(out)::value,error
  character(c_char),target::b(128)
  integer(c_int)::r
  integer::n
  value='';error='';n=len(raw)
  do while(n>0)
   if(raw(n:n)/=c_null_char)exit
   n=n-1
  end do
  if(.not.valid(product).or..not.valid(source))then;error='invalid_argument';return;end if
  if(.not.valid(raw(:n)))then;error='identity_unavailable';return;end if
  r=derive_c(product//c_null_char,source//c_null_char,raw(:n)//c_null_char,b,128_c_size_t)
  if(r==0)then;value=text(c_loc(b));else;error=text(result_c(r));end if
 end subroutine
 subroutine lb_native(product,value,error)
  character(*),intent(in)::product
  character(:),allocatable,intent(out)::value,error
  character(c_char),target::b(128)
  integer(c_int)::r
  value='';error=''
  if(.not.valid(product))then;error='invalid_argument';return;end if
  r=native_c(product//c_null_char,b,128_c_size_t)
  if(r==0)then;value=text(c_loc(b));else;error=text(result_c(r));end if
 end subroutine
end module
