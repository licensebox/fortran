program status_example
 use licensebox
 implicit none
 type(lb_client)::client
 character(:),allocatable::value,error
 call client%init('{"product":"your-product","keys":{"current":"YOUR_PUBLIC_KEY"}}',error)
 if(error=='')call client%call('require','{"module":"core"}',value,error)
 if(error/='')then;print *,error;else;print *,value;end if
 call client%close()
end program
