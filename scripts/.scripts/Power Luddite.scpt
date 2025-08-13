on run
	set kvm_host to "ludditekvm"
	set power_pin to "503"
	set gpio_path to "/sys/class/gpio/gpio" & power_pin
	
	set ssh_command to "ssh root@" & kvm_host & " -q -o ConnectTimeout=1 -o BatchMode=yes 'echo 1 > " & gpio_path & "/value; sleep 1; echo 0 > " & gpio_path & "/value'"
	
	try
		do shell script ssh_command
		display notification "Successfully toggled power on " & kvm_host
	on error
		display notification "Failed to connect to " & kvm_host
	end try
end run
