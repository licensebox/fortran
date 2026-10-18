program test_binding
 use licensebox
 implicit none
 type(lb_client)::a,b
 character(:),allocatable::value,error
 call lb_derive('demo','linux-machine-id','12345678123456789abc1234567890ab',value,error)
 if(error/='')stop 1
 if(value/='m1:linux-machine-id:783b8906a4af61095e68f3f0b4468a953be69ddd7afcf35850b501a7361f309e')stop 2
 call a%init('{"product":"demo","keys":{"k":"A6EHv_POEL4dcN0Y50vAmWfk1jCbpQ1fHdyGZBJVMbg"},'// &
 '"directory":"build/test-state"}',error)
 if(error/='')then;print *,error;stop 3;end if
 b=a
 call b%call('status','{}',value,error)
 if(error/='not_signed_in')stop 4
 call a%close();call b%close()
 call b%call('status','{}',value,error)
 if(error/='invalid_argument')stop 5
 print *,'binding checks passed'
end program
