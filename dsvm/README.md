## Superuser notes
### Provision the Linux Data Science Virtual Machine
<https://docs.microsoft.com/en-us/azure/machine-learning/machine-learning-data-science-linux-dsvm-intro>

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

Then, from Terminal, run
```
$ chmod 700 ~/path/to/login_accounts.sh
$ sudo ~/path/to/login_accounts.sh
```
