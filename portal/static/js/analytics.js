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
		  	rawData.unshift(['Os 10 serviços mais acessados', 'Acessos']);
		  	
		    var data = new google.visualization.arrayToDataTable(rawData);		
		     var options = {
				  title: 'Os 10 serviços mais acessados',
				  width: 750,
				  height: 340,
				  legend: { position: 'none' },
				  chart: { title: 'Os 10 serviços mais acessados no PPCA API Management Plataform',
						   subtitle: 'Quantidade de acessos por mês' },
				  bars: 'horizontal', // Required for Material Bar Charts.
				  axes: {
					x: {
					  0: { side: 'top', label: 'Acessos'}
					  
					  
					}
					
					
				  },
		      bar: { groupWidth: "90%" }
		    };
			
		    var chart = new google.charts.Bar(document.getElementById('top_x_div'));
			
        	chart.draw(data, options); 
        	 
		});
      }
   

      function plota_grafico_qtd_requisicoes_by_date(rawData) {
	      $.get('http://ec2-54-94-128-45.sa-east-1.compute.amazonaws.com:2301/health/qtd_requests_by_date/10?sort=date', function(rawData){

				rawData.unshift(['Quantidade de requisições diárias no último mês', 'Acessos']);
				var data = new google.visualization.arrayToDataTable(rawData);		
			
				var options = {
					chart: {
						title: 'Quantidade de requisições diárias no último mês',
						subtitle: 'Quantidade de acessos'
					},
			   
					width: 750,
					height: 200
				}; 		

				var chart = new google.charts.Line(document.getElementById('linechart_material')); 
				
				chart.draw(data, options);
				

		  });
	   }
