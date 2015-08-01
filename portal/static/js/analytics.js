      function getData(data) {
      	rawData = data;
      	console.log(rawData);
      }
      google.load("visualization", "1.1", {packages:["bar"]});
      google.setOnLoadCallback(plota_grafico_top_services);

      google.load("visualization", "1.1", {packages:["line"]});
      google.setOnLoadCallback(plota_grafico_qtd_requisicoes_by_date);
     
      function plota_grafico_top_services(rawData) {
		$.get('http://ec2-54-94-128-45.sa-east-1.compute.amazonaws.com:2301/health/top_services/10', function(rawData){
		  	rawData.unshift(['TOP 10 Servicos mais Acessados', 'Acessos']);
		  	
		    var data = new google.visualization.arrayToDataTable(rawData);		
		     var options = {
				  title: 'TOP 10 Servicos mais Acessados',
				  width: 750,
				  height: 340,
				  legend: { position: 'none' },
				  chart: { title: 'TOP 10 Servicos mais Acessados',
						   subtitle: 'Quantidade de Acessos' },
				  bars: 'horizontal', // Required for Material Bar Charts.
				  axes: {
					x: {
					  0: { side: 'top', label: 'Acessos'} // Top x-axis.
					}
				  },
		      bar: { groupWidth: "90%" }
		    };
			
		    var chart = new google.charts.Bar(document.getElementById('top_x_div'));
			var chart2 = new google.charts.Bar(document.getElementById('top_x_div2'));
        	chart.draw(data, options); 
        	chart2.draw(data, options); 
		});
      }
   

      function plota_grafico_qtd_requisicoes_by_date(rawData) {
	      $.get('http://ec2-54-94-128-45.sa-east-1.compute.amazonaws.com:2301/health/qtd_requests_by_date/10?sort=date', function(rawData){

				rawData.unshift(['Total de Requisicões no Último Mês', 'Acessos']);
				var data = new google.visualization.arrayToDataTable(rawData);		
			
				var options = {
					chart: {
						title: 'Total de Requisicões no Último Mês',
						subtitle: 'Quantidade de Acessos'
					},
			   
					width: 750,
					height: 200
				}; 		

				var chart = new google.charts.Line(document.getElementById('linechart_material')); 
				var chart2 = new google.charts.Line(document.getElementById('linechart_material2'));
				chart.draw(data, options);
				chart2.draw(data, options);

		  });
	   }
