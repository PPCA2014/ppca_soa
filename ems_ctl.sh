#!/bin/bash
#
# ErlangMS Control Manager
# Autor: Everton de Vargas Agilar
# Data: 03/12/2015
#


# Documentação sobre o comando ems_ctl.sh
#--------------------------------------------------------------------
# 1) Opções que podem ser utilizadas no comando ems_ctl.sh
#     $1 -> opção do comando (start, start-daemon, stop e console)
#     $2 -> nome do node que quer instanciar ou conectar
#
#
# 2) Como instânciar um node ErlangMS com nome padrão "msbus": 
#    
#            ./ems_ctl.sh start
#
#
# 3) Instanciar um node ErlangMS com um node específico): 
#
#            ./ems_ctl.sh start nome_do_node
#
#         Exemplo 1: ./ems_ctl.sh start node_01
#         Exemplo 2: ./ems_ctl.sh start prod_esb
#
#
# 4) Instanciar um node ErlangMS como daemon
#
#            ./ems_ctl.sh start_daemon
#
# 5) Conectar em uma instância ErlangMS
#
#            ./ems_ctl.sh console nome_do_node
#
#         Exemplo 1: ./ems_ctl.sh console node_01
#         Exemplo 2: ./ems_ctl.sh console   (vai conectar na instância padrão msbus)
#

export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin"

# Set working dir
current_dir=$(dirname $0)
cd $current_dir

# Parameters
ems_ctl_version="1.0.0"
ems_cookie=erlangms
ems_node="emsbus"
ems_init="ems_bus:start()"
ems_stop="ems_bus:stop()"
ems_log_conf="$current_dir/priv/conf/elog"  
ems_hostname=`hostname`
ems_ctl_node="ems_shell_`date +"%I%M%S"`@$ems_hostname"
ems_path="-pa $current_dir/ebin `ls -d deps/*/ebin`"


# Conectar no terminal de uma instância ErlangMS
function console() {
	node_name=$(format_node_name $1)
	echo "Connecting in instance $node_name..."
	erl -remsh $node_name -sname $ems_ctl_node \
		-setcookie $ems_cookie \
		$ems_path \
		-boot start_sasl -config $ems_log_conf \
		
}

# Instanciar um node ErlangMS
function start() {
	node_name=$(format_node_name $1)
	is_daemon=$2
	status $node_name
	if [ $? != 0 ]; then
		echo "ATTENTION: Instance $node_name is already open in the cluster!!!"
		console $node_name
	else
		if [ "$is_daemon" == "daemon" ]; then
			echo "Starting instance $node_name daemon..."
			erl -detached $ems_path -noinput \
				-sname $node_name -setcookie $ems_cookie \
				-eval $ems_init -boot start_sasl -config $ems_log_conf 
			echo "OK!"
		else
			echo "Starting instance $node_name..."
			erl $ems_path \
				-sname $node_name -setcookie $ems_cookie -eval $ems_init \
				-boot start_sasl -config $ems_log_conf 
		fi
	fi
}


# Verifica se uma instância ErlangMS está executando
function status(){
	node_name=$(format_node_name $1)
	erl $ems_path \
		-sname emsbus -setcookie erlangms \
		-boot start_sasl \
		-config ./priv/conf/elog 1> /dev/null 2> /dev/null <<EOF
			halt().
EOF
}

function list_nodes(){
	epmd -names
}

function format_node_name(){
	node_name=$1
	if [ "$node_name" == "" ]; then
		node_name="emsbus@$ems_hostname"
	fi
	if [ `expr index "$node_name" "@"` == 0 ]; then
		node_name="$node_name@$ems_hostname" 
	fi
	echo $node_name
}

help() {
	echo "Use: ems_ctl.sh start|start-daemon|console|list|status [node]"
	echo ""
	echo "Params:"
	echo "    start -> starts a new instance in the foreground"
	echo "    start-daemon -> starts a new instance in the background"
	echo "    console -> connect the console to a running instance"
	echo "    list -> list the instances running on cluster"
	echo "    tstatus -> it indicates whether the instance is running"
	echo "    [node] -> which the instance of the bus. By default it is emsbus"
	
}

# header do comando
clear
echo "ErlangMS Control Manager ( Hostname: $ems_hostname,  Version: $ems_ctl_version )"

# Aciona o comando escolhido
case "$1" in

	  'start')
			start $2
	  ;;

	  'start_daemon')
			Node="$2"
			start "$Node" "daemon"
	  ;;

	  'start-daemon')
			Node="$2"
			start "$Node" "daemon"
	  ;;

	  'stop')
			echo "To stop an instance, connect it first!"
	  ;;

	  'restart')
			echo "To restart an instance, connect it first!"
	  ;;

	  'console')
			console $2
	  ;;

	  'list')
			list_nodes
	  ;;

	  '--help')
			help
	  ;;

	  'status')
			status $2
			if [ "$?" == "1" ]; then
				echo "$node_name is already running!"
			else
				echo "$node_name is stopped!"
			fi
	  ;;

	  *)
			help
	  ;;

esac
