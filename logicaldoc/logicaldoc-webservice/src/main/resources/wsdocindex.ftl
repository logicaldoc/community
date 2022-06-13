<#escape x as (x!)?html>
	<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
	<html>
		<head>
			<style type="text/css">
				body {background-color: rgb(246,246,246); }
				h1 { color: rgb(0,51,102); font-family: helvetica; font-size: 15pt; font-weight: bold; }
				h2 { color: rgb(0,51,102); font-family: helvetica; font-size: 13pt; font-weight: bold; }
				h3 { color: rgb(0,51,102); font-family: helvetica; font-size: 9pt; font-weight: bold; }
				p { color: rgb(0,51,102); font-family: helvetica; font-size: 10pt; font-weight: normal; }
				a { color: rgb(0,51,102); font-family: helvetica; font-size: 10pt; font-weight: normal; }
				a.th { color: rgb(246,246,246); background-color: rgb(102,102,102); font-family: helvetica; font-size: 10pt; font-weight: bold; }
				p.error { color: rgb(255,0,0); font-family: helvetica; font-size: 10pt; font-weight: normal; }
				th.color { color: rgb(246,246,246); background-color: rgb(102,102,102); font-family: helvetica; font-size: 10pt; font-weight: bold; }
				th.no_color { color: rgb(0,51,102); font-family: helvetica; font-size: 10pt; font-weight: bold; }
				td.color { color: rgb(0,51,102); background-color: rgb(255,225,225); font-family: helvetica; font-size: 10pt; font-weight: normal; }
				td.color_pre { color: rgb(0,51,102); background-color: rgb(255,225,225); font-family: helvetica; font-size: 10pt; font-weight: normal; white-space: pre }
				td.no_color { color: rgb(0,51,102); font-family: helvetica; font-size: 10pt; font-weight: normal; }
				td.no_color_pre { color: rgb(0,51,102); font-family: helvetica; font-size: 10pt; font-weight: normal; white-space: pre }
				.methodBody {padding-top: 50px}
			</style>
		
			<title>
				LogicalDOC API ${release} 
			</title>
		</head>	
		<body>
			<h1>LogicalDOC SOAP API ${service.release} </h1>
			
			<table cellspacing="1" cellpadding="1" border="0">
				<tbody>
						<tr valign="top">
							<th align="left" class="color">Web Service</th>
							<th class="no_color"/>
							<th align="left" class="color">Description</th>																															
						</tr>
					<#list service.methodStubs as webservice>						
						<tr class="no_color">
							<td/>
						</tr>					
						<tr valign="top">
							<td align="left" class="color"><a href="./${webservice.methodName}.html">${webservice.methodName}</a></td>
							<td class="no_color"/>
							<td align="left" class="color">${webservice.description}</td>								
						</tr>
					</#list>
				</tbody>
			</table>								
		</body>
	</html>  
</#escape>