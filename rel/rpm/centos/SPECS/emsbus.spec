Summary: A service-oriented bus developed in Erlang/OTP by Everton de Vargas Agilar
Name: ems-bus
Version: 1.0.11
Release: el7.centos
License: GPL
Group: System/Utilities
URL: https://github.com/erlangms
Vendor: ERLANGMS
Packager: ErlangMS Team <evertonagilar@unb.br>
BuildRoot: %{_tmppath}/%{name}-{%version}
Source0: %{name}-%version.tar.gz
Provides: ems-bus

%description
ErlangMS is a enterprise service bus developed in Erlang/OTP to facilitate the integration of systems through a service-oriented approach for the systems of the University of Brazilia. 

This work is the result of efforts made in the Master of Applied Computing at the University of Brasilia
by graduate student Everton Vargas Agilar.

%prep

  rm -rf %{name}-%{version}
  mkdir -p %{name}-%{version}
  #tar -zxf $RPM_SOURCE_DIR/%{name}-%{version}.tar.gz


%install

  mkdir -p $RPM_BUILD_ROOT
  cp -R $RPM_SOURCE_DIR/* $RPM_BUILD_ROOT/


%post

  LOG="/var/log/ems-bus/ems-bus.log"
  USER_EMS_BUS=erlangms
  GROUP_EMS_BUS=erlangms
  HOME_EMS_BUS=/var/opt/erlangms

  # database and log path
  mkdir -p /usr/lib/ems-bus/priv/db > /dev/null 2>&1
  mkdir -p /var/log/ems-bus > /dev/null 2>&1

  # create user and group erlangms if it not exist
  if ! grep erlangms /etc/passwd | cut -d: -f1 > /dev/null ; then
	  groupadd --system  $GROUP_EMS_BUS  > /dev/null 2>&1
	  password="admin"
	  #password_crypt=$(perl -e 'print crypt($ARGV[0], "wtf")' $password)
	  password_crypt="wtdgpkEyPdF1A"
	  useradd -g $GROUP_EMS_BUS --create-home --system \
								--home-dir $HOME_EMS_BUS \
								--shell /bin/bash \
								--password $password_crypt \
								--comment "User do barramento ERLANGMS" $USER_EMS_BUS  > /dev/null 2>&1
      echo "User erlangms created with admin passwd. Change after installation!"
  fi
  
  # The starters need to be Suid root.
  chmod 4777 /usr/lib/ems-bus/bin/ems-bus > /dev/null 2>&1
  # The starters need to be Suid root para Erts.
  for ExecutableErts in `find /usr/lib/ems-bus/erts-*/bin/`; do
      chmod 4511 $ExecutableErts > /dev/null 2>&1
  done
  
  # Cria a pasta /var/log/ems-bus
  mkdir -p /var/log/ems-bus
  chown -Rf $USER_EMS_BUS:$GROUP_EMS_BUS /var/log/ems-bus > /dev/null 2>&1


  # configure home user (/var/opt/erlangms)
  mkdir -p $HOME_EMS_BUS/.erlangms > /dev/null 2>&1
  # Creates the configuration file only if it does not exist
  if [ ! -f /var/opt/erlangms/.erlangms/emsbus.conf ]; then
	cp /usr/lib/ems-bus/priv/conf/emsbus.conf $HOME_EMS_BUS/.erlangms/ > /dev/null 2>&1
  fi
  ln -s /usr/lib/ems-bus/ $HOME_EMS_BUS/ems-bus > /dev/null 2>&1
  
  # remove o .odbc.ini original criado pelo useradd e cria um link para o priv/conf/odbc.ini
  rm -f $HOME_EMS_BUS/.odbc.ini > /dev/null 2>&1
  ln -s /usr/lib/ems-bus/priv/conf/odbc.ini $HOME_EMS_BUS/.odbc.ini > /dev/null 2>&1

  chown -Rf $USER_EMS_BUS:$GROUP_EMS_BUS /var/opt/erlangms > /dev/null 2>&1	
  chown -Rf $USER_EMS_BUS:$GROUP_EMS_BUS /var/opt/erlangms/.erlangms > /dev/null 2>&1	


  # create .hosts.erlang if it not exist
  if [ ! -f ~/.hosts.erlang ]; then
	echo \'$(hostname | cut -d. -f1)\' > $HOME_EMS_BUS/.hosts.erlang 
  fi
  

  # Iptables firewall
  #iptables -C INPUT -p tcp -m multiport --dports 2301,2302,2389 -j ACCEPT 2> /dev/null
  #if [ "$?" -eq "1" ]; then
  #    iptables -A INPUT -p tcp -m multiport --dports 2301,2302,2389 -j ACCEPT  > /dev/null 2>&1 || true
  #fi


  # Firewalld
  firewall-cmd --zone=public --add-port=2301/tcp > /dev/null 2>&1 || true
  firewall-cmd --zone=public --add-port=2302/tcp > /dev/null 2>&1 || true
  firewall-cmd --zone=public --add-port=2389/tcp > /dev/null 2>&1 || true
  firewall-cmd --zone=public --add-port=4369/tcp > /dev/null 2>&1 || true
  firewall-cmd --reload  > /dev/null 2>&1 || true


  # change owners to erlangms:erlangms
  chown -Rf $USER_EMS_BUS:$GROUP_EMS_BUS /usr/lib/ems-bus > /dev/null 2>&1
  chown -Rf $USER_EMS_BUS:$GROUP_EMS_BUS /etc/ems-bus > /dev/null 2>&1
  chown -Rf $USER_EMS_BUS:$GROUP_EMS_BUS /var/log/ems-bus > /dev/null 2>&1
  chown -Rf $USER_EMS_BUS:$GROUP_EMS_BUS /var/opt/erlangms > /dev/null 2>&1
  chown -f $USER_EMS_BUS:$GROUP_EMS_BUS /usr/bin/ems-bus > /dev/null 2>&1

  # change owners to root:root
  chown root:root /etc/sudoers.d/ems-bus.sudoers > /dev/null 2>&1
  chown root:root /etc/systemd/system/ems-bus.service > /dev/null 2>&1

  # config /etc/odbcinst.ini if necessary for FreeTDS SQL-server driver
  JTDS_ENTRY_CONF=$(sed -rn '/\[FreeTDS\]/, /(^$|^#)/p' /etc/odbcinst.ini 2> /dev/null)
  if [ -z "$JTDS_ENTRY_CONF" ]; then
	echo " " >> /etc/odbcinst.ini 
	echo "# Driver for SQL-server" >> /etc/odbcinst.ini 
	echo "# Setup from the ems-bus package" >> /etc/odbcinst.ini 
	echo "[FreeTDS]" >> /etc/odbcinst.ini 
	echo "Description=FreeTDS Driver" >> /etc/odbcinst.ini 
	echo "Driver=/usr/lib64/libtdsodbc.so.0" >> /etc/odbcinst.ini 
	echo " " >> /etc/odbcinst.ini 
  fi

  # ldconfig
  /sbin/ldconfig  > /dev/null 2>&1 || true


  # systemd
  systemctl stop ems-bus.service  > /dev/null 2>&1 || true
  systemctl enable ems-bus.service  > /dev/null 2>&1 || true
  systemctl daemon-reload  > /dev/null 2>&1 || true
  systemctl start ems-bus.service  > /dev/null 2>&1 || true


%postun 

	# pare e desative o serviço systemctl
	systemctl stop ems-bus.service  > /dev/null 2>&1 || true
	systemctl disable ems-bus.service > /dev/null 2>&1 || true

	# remove user
	#groupdel erlangms > /dev/null 2>&1 || true
	#userdel erlangms > /dev/null 2>&1 || true

	/sbin/ldconfig




%files
%defattr(0755,root,root)
/etc/ems-bus/*
/etc/systemd/system/ems-bus.service
/etc/systemd/system/ems-bus.service.d/limits.conf
/etc/firewalld/services/ems-bus.xml
/etc/sudoers.d/ems-bus.sudoers
/usr/lib/ems-bus/*


