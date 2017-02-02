# Provision the Linux Data Science Virtual Machine
<https://docs.microsoft.com/en-us/azure/machine-learning/machine-learning-data-science-linux-dsvm-intro>

## x2goclient
1. Download and install the X2Go client for your client platform from [X2Go](http://wiki.x2go.org/doku.php/doc:installation:x2goclient)
2. Run the X2Go client, and select New Session
  * Configuration parameters:  
    - Host: 52.175.221.211  
    - Login: User name on the Linux VM  
    - SSH Port: Leave it at 22, the default value  
    - Session Type: Change the value to XFCE  
  * Media tab: Turn off sound support and client printing
  * Shared folders: If you want directories from your client machines mounted on the Linux VM, add the client machine directories that you want to share with the VM on this tab.

## Tools
### RStudio
To create a link to RStudio on the desktop, right-click the rstudio executable
```
xdg-open /usr/lib/rstudio/bin
```

## Superuser notes
### User accounts
To create a unique OS local non-admin login account and password for each user,
save the following bash script to your home directory
```
#!/bin/bash

for i in {1..2} # 2 users
do
u=`openssl rand -hex 2`;
useradd user$u;
p=`openssl rand -hex 5`;
echo $p | passwd user$u --stdin;
echo user$u, $p >> 'usersinfo.csv'
done
```

```
chmod 700 ~/path/to/login_accounts.sh
```

Then, in Terminal, type
```
$ sudo ~/path/to/login_accounts.sh
```
