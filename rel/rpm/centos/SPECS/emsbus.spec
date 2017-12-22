Summary: A service-oriented bus developed in Erlang/OTP by Everton de Vargas Agilar
Name: ems-bus
Version: 1.0.13
Release: centos.7
License: GPL
Group: System/Utilities
URL: https://github.com/erlangms
Vendor: ERLANGMS
Packager: ErlangMS Team <evertonagilar@unb.br>
BuildRoot: %{_tmppath}/%{name}-{%version}
Source0: %{name}-%version.tar.gz
Provides: ems-bus
AutoReqProv: no
Requires: /bin/bash /bin/sh /bin/sh /bin/sh /usr/bin/env bash >= 2.0 libc.so.6()(64bit)  libc.so.6(GLIBC_2.10)(64bit) libc.so.6(GLIBC_2.11)(64bit) libc.so.6(GLIBC_2.14)(64bit) libc.so.6(GLIBC_2.15)(64bit) libc.so.6(GLIBC_2.2.5)(64bit) libc.so.6(GLIBC_2.3)(64bit) libc.so.6(GLIBC_2.3.2)(64bit) libc.so.6(GLIBC_2.3.4)(64bit) libc.so.6(GLIBC_2.4)(64bit) libc.so.6(GLIBC_2.7)(64bit) libc.so.6(GLIBC_2.8)(64bit) libcrypto.so.10()(64bit) libcrypto.so.10(OPENSSL_1.0.1)(64bit) libcrypto.so.10(OPENSSL_1.0.1_EC)(64bit) libcrypto.so.10(libcrypto.so.10)(64bit) libdl.so.2()(64bit) libdl.so.2(GLIBC_2.2.5)(64bit) libgcc_s.so.1()(64bit) libgcc_s.so.1(GCC_3.0)(64bit) libm.so.6()(64bit) libm.so.6(GLIBC_2.2.5)(64bit) libodbc.so.2()(64bit) libpthread.so.0()(64bit) libpthread.so.0(GLIBC_2.12)(64bit) libpthread.so.0(GLIBC_2.2.5)(64bit) libpthread.so.0(GLIBC_2.3.2)(64bit) librt.so.1()(64bit) librt.so.1(GLIBC_2.2.5)(64bit) libstdc++.so.6()(64bit) libstdc++.so.6(CXXABI_1.3)(64bit) libtinfo.so.5()(64bit) libutil.so.1()(64bit) libutil.so.1(GLIBC_2.2.5)(64bit) libz.so.1()(64bit) libz.so.1(ZLIB_1.2.2)(64bit) rpmlib(CompressedFileNames) <= 3.0.4-1 rpmlib(FileDigests) <= 4.6.0-1 rpmlib(PartialHardlinkSets) <= 4.0.4-1 rpmlib(PayloadFilesHavePrefix) <= 4.0-1 rtld(GNU_HASH) rpmlib(PayloadIsXz) <= 5.2-1


%description
ErlangMS is a enterprise service bus developed in Erlang/OTP to facilitate the integration of systems through a service-oriented approach for the systems of the University of Brazilia. 

This work is the result of efforts made in the Master of Applied Computing at the University of Brasilia
by graduate student Everton Vargas Agilar.

%prep

  rm -rf %{name}-%{version}
  mkdir -p %{name}-%{version}
  #tar -zxf $RPM_SOURCE_DIR/%{name}-%{version}.tar.gz


%install

  sudo systemctl daemon-reload
  sudo systemctl stop ems-bus.service  > /dev/null 2>&1 || true
  mkdir -p $RPM_BUILD_ROOT
  cp -R $RPM_SOURCE_DIR/* $RPM_BUILD_ROOT/


%post

  LOG="/var/log/ems-bus/ems-bus.log"
  USER_EMS_BUS=erlangms
  GROUP_EMS_BUS=erlangms
  HOME_EMS_BUS=/var/opt/erlangms
  USER_CREATED="false"

  # database and log path
  mkdir -p /usr/lib/ems-bus/priv/db > /dev/null 2>&1
  mkdir -p /var/log/ems-bus > /dev/null 2>&1

  # create user and group erlangms if it not exist
  if ! grep "^erlangms:" /etc/passwd >> /dev/null ; then
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
      USER_CREATED="true"
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
  ln -s /usr/lib/ems-bus/priv/ $HOME_EMS_BUS/ems-bus/priv > /dev/null 2>&1
  

  # It only changes the $ HOME_EMS_BUS/.odbc.ini file when the user was created
  if [ "$USER_CREATED" == "true" ]; then
	  rm -f $HOME_EMS_BUS/.odbc.ini > /dev/null 2>&1
	  cp /usr/lib/ems-bus/priv/conf/odbc.ini $HOME_EMS_BUS/.odbc.ini > /dev/null 2>&1
  fi
  
  chown -Rf $USER_EMS_BUS:$GROUP_EMS_BUS /var/opt/erlangms > /dev/null 2>&1	
  chown -Rf $USER_EMS_BUS:$GROUP_EMS_BUS /var/opt/erlangms/.erlangms > /dev/null 2>&1	


  # create .hosts.erlang if it not exist
  if [ ! -f $HOME_EMS_BUS/.hosts.erlang ]; then
	echo \'$(hostname | cut -d. -f1)\'. > $HOME_EMS_BUS/.hosts.erlang 
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


  # Config /etc/odbcinst.ini if necessary for FreeTDS SQL-server driver
  JTDS_ENTRY_CONF=$(sed -rn '/\[FreeTDS\]/, /(^$|^#)/p' /etc/odbcinst.ini 2> /dev/null)
  if [ -z "$JTDS_ENTRY_CONF" ]; then
	updatedb
	LIB_TDODBC_PATH=$(locate libtdsodbc.so | sed -n '1p')
	if [ ! -z "$LIB_TDODBC_PATH" ]; then
		echo " " >> /etc/odbcinst.ini 
		echo "# Driver for SQL-server" >> /etc/odbcinst.ini 
		echo "# Setup from the ems-bus package" >> /etc/odbcinst.ini 
		echo "[FreeTDS]" >> /etc/odbcinst.ini 
		echo "Description=FreeTDS Driver" >> /etc/odbcinst.ini 
		echo "Driver=$LIB_TDODBC_PATH" >> /etc/odbcinst.ini 
		echo " " >> /etc/odbcinst.ini 
	fi
  fi
  

  # Config /etc/security/limits.conf if necessary for erlangms group
  if ! grep -q '@erlangms' /etc/security/limits.conf ; then
	echo " " >> /etc/security/limits.conf
	echo "# Security for ERLANGMS ESB" >> /etc/security/limits.conf
	echo "@erlangms         hard    nofile      500000" >> /etc/security/limits.conf
	echo "@erlangms         soft    nofile      500000" >> /etc/security/limits.conf
	echo "@erlangms         hard    nproc       500000" >> /etc/security/limits.conf
	echo "@erlangms         soft    nproc       500000" >> /etc/security/limits.conf
	echo "" >> /etc/security/limits.conf
	sed -ri '/^# *End of file$/d;' /etc/security/limits.conf
	sed -i '$ a # End of file' /etc/security/limits.conf	 
  fi

  # Tunning fs.file-max. At least it should be 1000000
  FILE_MAX_DEF=1000000
  FILE_MAX=$(cat /proc/sys/fs/file-max)
  if [ $FILE_MAX -lt $FILE_MAX_DEF ]; then
		# Ajusta ou adiciona o valor para fs.file-max
		if grep -q 'fs.file-max' /etc/sysctl.conf ; then
			sed -ri "s/^fs.file-max=[0-9]{1,10}$/fs.file-max=$FILE_MAX_DEF/" /etc/sysctl.conf
		else
			echo "" >> /etc/sysctl.conf
			echo "# File descriptors limit" >> /etc/sysctl.conf
			echo "fs.file-max=$FILE_MAX_DEF" >> /etc/sysctl.conf
		fi
		sysctl -p > /dev/null 2>&1
  fi


  # ldconfig
  /sbin/ldconfig  > /dev/null 2>&1 || true

  # database recreate
  rm -rf /usr/lib/ems-bus/priv/db

  # systemd
  systemctl stop ems-bus.service  > /dev/null 2>&1 || true
  systemctl stop ems-bus.epmd.service  > /dev/null 2>&1 || true

  # systemd
  systemctl enable /usr/lib/ems-bus/priv/systemd/ems-bus.epmd.service  > /dev/null 2>&1 || true
  systemctl enable /usr/lib/ems-bus/priv/systemd/ems-bus.service  > /dev/null 2>&1 || true
  systemctl daemon-reload  > /dev/null 2>&1 || true

  sleep 2
  systemctl start ems-bus.epmd.service  > /dev/null 2>&1 || true
  sleep 2
  systemctl start ems-bus.service  > /dev/null 2>&1 || true

%postun 

	# pare e desative o serviço systemctl
	systemctl stop ems-bus.service  > /dev/null 2>&1 || true
	systemctl disable ems-bus.service > /dev/null 2>&1 || true

	systemctl stop ems-bus.epmd.service  > /dev/null 2>&1 || true
	systemctl disable ems-bus.epmd.service > /dev/null 2>&1 || true

	systemctl daemon-reload

	# remove user
	#groupdel erlangms > /dev/null 2>&1 || true
	#userdel erlangms > /dev/null 2>&1 || true

    # database recreate
	rm -rf /usr/lib/ems-bus/priv/db

	/sbin/ldconfig
	
	




%files
%defattr(0755,root,root)
/etc/ems-bus/*
/etc/systemd/system/ems-bus.epmd.service
/etc/systemd/system/ems-bus.epmd.service.d/limits.conf
/etc/systemd/system/ems-bus.service
/etc/systemd/system/ems-bus.service.d/limits.conf
/etc/firewalld/services/ems-bus.xml
/etc/sudoers.d/ems-bus.sudoers
/usr/lib/ems-bus/*


