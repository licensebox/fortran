module synthetic_callbacks
 use iso_c_binding
 implicit none
 real(c_double)::current_time
 character(:),allocatable::current_binding
contains
 function clock_cb(ctx) bind(C) result(value)
  type(c_ptr),value::ctx
  real(c_double)::value
  value=current_time
 end function
 function binding_cb(ctx,output,capacity) bind(C) result(code)
  type(c_ptr),value::ctx
  integer(c_size_t),value::capacity
  character(c_char)::output(*)
  integer(c_int)::code
  integer::i
  code=1;if(len(current_binding)>=capacity)return
  do i=1,len(current_binding);output(i)=current_binding(i:i);end do
  output(len(current_binding)+1)=c_null_char;code=0
 end function
 subroutine signin_cb(ctx,url,code) bind(C)
  type(c_ptr),value::ctx,url,code
 end subroutine
end module
program adapter
 use licensebox
 use synthetic_callbacks
 implicit none
 character(:),allocatable::op,product,source,raw,token,keys,now,options,args,value,error
 type(lb_callbacks),target::callbacks
 type(lb_client)::client
 call readhex(op);call readhex(product);call readhex(source);call readhex(raw)
 call readhex(token);call readhex(keys);call readhex(now);call readhex(current_binding)
 call readhex(options);call readhex(args);read(now,*)current_time
 select case(op)
 case('derive')
  call lb_derive(product,source,raw,value,error)
  if(error=='')value='"'//value//'"'
 case('native')
  call lb_native(product,value,error)
  if(error=='')value='"'//value//'"'
 case('verify')
  call lb_verify(token,product,keys,current_time,value,error)
 case default
  callbacks%clock=c_funloc(clock_cb);callbacks%machine_binding=c_funloc(binding_cb)
  callbacks%on_sign_in=c_funloc(signin_cb)
  call client%init(options,error,callbacks)
  value='null'
  if(error=='')then
   select case(op)
   case('require')
    call client%call('require',args,value,error)
   case('repeat')
    call client%call('require','{"module":"core"}',value,error)
    if(error=='')call client%call('require','{"module":"core"}',value,error)
   case('sign_in')
    call client%call(op,'{"timeout":3}',value,error)
   case('request')
    call client%call(op,'{"modules":["export"],"note":"café – test"}',value,error)
   case('record')
    call client%call(op,'{"command":"export","success":true,"elapsed":0.125}',value,error)
    if(error=='')call client%call(op,'{"command":"export","success":false,"elapsed":0.025}',value,error)
   case default
    call client%call(op,'{}',value,error)
   end select
  end if
  call client%close()
 end select
 call writehex(error);call writehex(value)
contains
 subroutine readhex(s)
  character(:),allocatable,intent(out)::s
  character(2097152)::line
  integer::i,n,v
  read(*,'(A)')line;n=len_trim(line)/2;s=repeat(' ',n)
  do i=1,n;read(line(i*2-1:i*2),'(Z2)')v;s(i:i)=achar(v);end do
 end subroutine
 subroutine writehex(s)
  character(*),intent(in)::s
  integer::i
  do i=1,len(s);write(*,'(Z2.2)',advance='no')iachar(s(i:i));end do
  write(*,*)
 end subroutine
end program
